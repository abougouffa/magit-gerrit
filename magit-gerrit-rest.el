;;; magit-gerrit-rest.el --- REST backend of magit-gerrit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa
;; URL: https://github.com/abougouffa/magit-gerrit

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


;;; Commentary:

;; The REST backend of magit-gerrit, see `magit-gerrit-backend'.  It
;; uses Gerrit's REST API over HTTP(S), see
;; https://gerrit-review.googlesource.com/Documentation/rest-api.html
;;
;; Besides what the SSH backend does, it supports replies to comments,
;; resolving comment threads, draft comments, the values of the labels
;; of the server, suggestions of reviewers, hashtags, stars, private
;; changes, editing commit messages, reverting changes and
;; cherry-picking them on the server.
;;
;; Configuration:
;;
;; - `magit-gerrit-rest-url': the URL of the Gerrit server, such as
;;   "https://review.example.com" (or "https://example.com/r" when
;;   Gerrit is not at the root of its host).  When nil, it is deduced
;;   from the remote, see `magit-gerrit-rest-hosts'.
;; - Authentication: Gerrit authenticates REST requests with HTTP basic
;;   authentication, using the HTTP password generated in the settings
;;   of the user (not the password of the account).  Put it in an
;;   auth-source file, such as ~/.authinfo.gpg:
;;
;;     machine review.example.com login john password GENERATED-PASSWORD
;;
;;   See `magit-gerrit-rest-auth'.  Without credentials, the requests
;;   are anonymous: changes can be read but not modified.
;;
;; The backend is used automatically for HTTPS remotes of the hosts
;; listed in `magit-gerrit-rest-hosts' (and of *.googlesource.com), or
;; when `magit-gerrit-rest-url' is set and has the host of the remote.
;; Set `magit-gerrit-backend' to `rest' to use it in any case.

;;; Code:

(require 'magit-gerrit)
(require 'auth-source)
(require 'url)
(require 'url-http)


;;; Options

(defcustom magit-gerrit-rest-url nil
  "Base URL of the Gerrit server for the REST API.
For instance \"https://review.example.com\", without the \"/a/\"
prefix of authenticated requests.  When nil, it is deduced from the
remote, see `magit-gerrit-rest-hosts'.  This is typically set per
repository, in a .dir-locals.el file."
  :group 'magit-gerrit
  :type '(choice (const :tag "Deduce from the remote" nil) string))
(put 'magit-gerrit-rest-url 'safe-local-variable #'stringp)

(defcustom magit-gerrit-rest-hosts nil
  "Hosts of Git remotes served by a Gerrit server with a REST API.
Each element is a host name, whose Gerrit server is at
https://HOST, or a cons (HOST . URL) when the URL of the server is
different.  The REST backend is used automatically for the remotes on
these hosts (see `magit-gerrit-auto-backends')."
  :group 'magit-gerrit
  :type '(repeat (choice (string :tag "Host")
                         (cons (string :tag "Host") (string :tag "Gerrit URL")))))

(defcustom magit-gerrit-rest-googlesource t
  "Whether to use the REST API for the remotes on googlesource.com.
The repositories of HOST.googlesource.com are reviewed on
HOST-review.googlesource.com."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-rest-auth 'auth-source
  "How to get the credentials of the REST API.
`auth-source' searches them in the auth-source files (see
`auth-sources') for the host of the server and the user
`magit-gerrit-rest-user' (if set).  `anonymous' doesn't authenticate.
A function is called with the host and the user (or nil), it returns
\(USER . PASSWORD) or nil.  The password is the HTTP password
generated in the settings of the user on Gerrit."
  :group 'magit-gerrit
  :type '(choice (const :tag "Auth-source" auth-source)
                 (const :tag "Anonymous" anonymous)
                 (function :tag "Function")))

(defvar-local magit-gerrit-rest-user nil
  "The user name for the REST API, to choose among several credentials.")
(put 'magit-gerrit-rest-user 'safe-local-variable #'stringp)

(defcustom magit-gerrit-rest-timeout 30
  "Number of seconds to wait for the answer of a REST request."
  :group 'magit-gerrit
  :type 'integer)

(defcustom magit-gerrit-rest-extra-change-options '("SUBMIT_REQUIREMENTS")
  "Options of the REST API added when fetching a change for its buffer.
They are not supported by old versions of Gerrit, the change is
fetched again without them when the server refuses them.  See
https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#query-options"
  :group 'magit-gerrit
  :type '(repeat string))


;;; Backend

(cl-defstruct (magit-gerrit-rest-backend
               (:include magit-gerrit-backend)
               (:constructor magit-gerrit-rest-backend-create)
               (:copier nil))
  "Backend using the REST API of Gerrit."
  (url nil :documentation "The base URL of the server, without trailing slash.")
  (user nil :documentation "The user name to authenticate with, or nil.")
  (auth 'unknown :documentation "The credentials, (USER . PASSWORD), nil when
anonymous or `unknown' before they are searched."))

(defun magit-gerrit-rest--googlesource-url (host)
  "Return the URL of the Gerrit server of HOST on googlesource.com, or nil."
  (when (and magit-gerrit-rest-googlesource host
             (string-match "\\`\\([^.]+?\\)\\(-review\\)?\\.googlesource\\.com\\'" host))
    (format "https://%s-review.googlesource.com" (match-string 1 host))))

(defun magit-gerrit-rest--url-for-remote (remote-url explicit)
  "Return the URL of the Gerrit server of REMOTE-URL, or nil.
See `magit-gerrit-rest-hosts'.  When EXPLICIT is non-nil, deduce it
from the host of the remote in any case."
  (when-let* ((host (magit-gerrit--remote-host remote-url)))
    (or (seq-some (lambda (entry)
                    (cond ((equal entry host) (concat "https://" host))
                          ((equal (car-safe entry) host) (cdr entry))))
                  magit-gerrit-rest-hosts)
        (magit-gerrit-rest--googlesource-url host)
        (when explicit
          (or magit-gerrit-web-url
              (let ((url (url-generic-parse-url remote-url)))
                (if (member (url-type url) '("http" "https"))
                    (format "%s://%s%s" (url-type url) host
                            (if (url-portspec url) (format ":%s" (url-portspec url)) ""))
                  (concat "https://" host))))))))

(defun magit-gerrit-rest--make-backend (remote-url explicit)
  "Return a REST backend for REMOTE-URL, or nil.
It is usable when `magit-gerrit-rest-url' is set or when the URL of
the server is known from the remote, see
`magit-gerrit-rest--url-for-remote' for EXPLICIT."
  (when-let* ((url (or magit-gerrit-rest-url (magit-gerrit-rest--url-for-remote remote-url explicit))))
    (magit-gerrit-rest-backend-create :url (string-trim-right url "/") :user magit-gerrit-rest-user)))

(magit-gerrit-register-backend 'rest #'magit-gerrit-rest--make-backend
                               'magit-gerrit-rest-url 'magit-gerrit-rest-user
                               'magit-gerrit-rest-hosts)

(defun magit-gerrit-rest--host (backend)
  "Return the host of the server of BACKEND."
  (url-host (url-generic-parse-url (magit-gerrit-rest-backend-url backend))))

(cl-defmethod magit-gerrit-backend-name ((_backend magit-gerrit-rest-backend))
  "Return the name of the REST backend."
  "REST")

(cl-defmethod magit-gerrit-backend-key ((backend magit-gerrit-rest-backend))
  "Return a string identifying the server of the REST BACKEND."
  (format "rest:%s:%s" (magit-gerrit-rest-backend-url backend)
          (or (magit-gerrit-rest-backend-user backend) "")))

(cl-defmethod magit-gerrit-backend-features ((_backend magit-gerrit-rest-backend))
  "Return the features of the REST backend."
  '(query async vote submit abandon restore rebase move topic reviewers cc wip comment reply
    drafts label-values suggest-accounts hashtags star private edit-message revert cherry-pick))

(cl-defmethod magit-gerrit-backend-matches-remote-p ((backend magit-gerrit-rest-backend) remote-url)
  "Return non-nil when the REST BACKEND is the server of REMOTE-URL."
  (let ((url (magit-gerrit-rest-backend-url backend)))
    (or (equal (magit-gerrit--remote-host remote-url) (magit-gerrit-rest--host backend))
        (when-let* ((remote-server (magit-gerrit-rest--url-for-remote remote-url nil)))
          (equal (string-trim-right remote-server "/") url)))))

(cl-defmethod magit-gerrit-backend-web-url ((backend magit-gerrit-rest-backend))
  "Return the URL of the server of the REST BACKEND."
  (magit-gerrit-rest-backend-url backend))


;;; Requests

(defun magit-gerrit-rest--auth (backend)
  "Return the credentials of BACKEND as (USER . PASSWORD), or nil.
See `magit-gerrit-rest-auth'.  They are searched once per backend."
  (when (eq (magit-gerrit-rest-backend-auth backend) 'unknown)
    (setf (magit-gerrit-rest-backend-auth backend)
          (let ((host (magit-gerrit-rest--host backend))
                (user (magit-gerrit-rest-backend-user backend)))
            (pcase magit-gerrit-rest-auth
              ('auth-source
               (when-let* ((entry (car (apply #'auth-source-search
                                              :host host :port '("https" "443" "http" "80")
                                              :max 1 :require '(:secret)
                                              (when user (list :user user)))))
                           (secret (plist-get entry :secret)))
                 (cons (or (plist-get entry :user) user)
                       (if (functionp secret) (funcall secret) secret))))
              ((pred functionp) (funcall magit-gerrit-rest-auth host user))
              (_ nil)))))
  (magit-gerrit-rest-backend-auth backend))

(defun magit-gerrit-rest--path (&rest segments)
  "Return the path made of SEGMENTS, URL-encoded (\"/\" too)."
  (mapconcat (lambda (segment) (concat "/" (url-hexify-string (format "%s" segment))))
             segments ""))

(defun magit-gerrit-rest--url (backend path params auth)
  "Return the URL of the REST endpoint PATH of BACKEND with PARAMS.
PATH starts with a slash.  PARAMS is an alist of query parameters,
\(NAME . VALUE) or (NAME) without value.  AUTH is non-nil for an
authenticated request (the path is prefixed with \"/a\")."
  (concat (magit-gerrit-rest-backend-url backend)
          (if auth "/a" "")
          path
          (when params
            (concat "?" (mapconcat (lambda (param)
                                     (if (cdr param)
                                         (concat (car param) "="
                                                 (url-hexify-string (format "%s" (cdr param))))
                                       (car param)))
                                   params "&")))))

(defun magit-gerrit-rest--headers (auth data)
  "Return the HTTP headers of a request with AUTH and DATA."
  (append
   '(("Accept" . "application/json"))
   (when data '(("Content-Type" . "application/json; charset=UTF-8")))
   (when auth
     `(("Authorization"
        . ,(concat "Basic " (base64-encode-string
                             (encode-coding-string (concat (car auth) ":" (cdr auth)) 'utf-8)
                             t)))))))

(defun magit-gerrit-rest--parse-body (body)
  "Parse the JSON BODY of a response, without its anti-XSSI prefix."
  (let* ((body (string-trim-left body))
         (body (if (string-prefix-p ")]}'" body)
                   (substring body (or (string-search "\n" body) (length body)))
                 body)))
    (unless (string-blank-p body)
      (magit-gerrit--json-parse body))))

(defun magit-gerrit-rest--response (buffer)
  "Return the response in BUFFER as (STATUS . BODY), and kill BUFFER.
STATUS is the HTTP status code (0 when unknown), BODY is the decoded
text of the response."
  (unwind-protect
      (with-current-buffer buffer
        (let ((status (or (bound-and-true-p url-http-response-status) 0)))
          (goto-char (or (bound-and-true-p url-http-end-of-headers)
                         (progn (goto-char (point-min))
                                (re-search-forward "\r?\n\r?\n" nil 'move)
                                (point))))
          ;; The marker of the end of the headers is before the empty line
          (when (looking-at "\r?\n")
            (goto-char (match-end 0)))
          (cons status (decode-coding-region (point) (point-max) 'utf-8 t))))
    (kill-buffer buffer)))

(defun magit-gerrit-rest--error (method url status body)
  "Return the error message of the request METHOD URL.
STATUS and BODY are the status and body of the response."
  (format "Gerrit request %s %s failed: %s%s" method url
          (if (eq status 0) "no response" (format "HTTP %s" status))
          (let ((text (car (split-string (string-trim (or body "")) "\n" t))))
            (cond (text (concat ", " text))
                  ((memq status '(401 403))
                   ", check the credentials (see `magit-gerrit-rest-auth')")
                  (t "")))))

(defmacro magit-gerrit-rest--with-request (spec &rest body)
  "Bind the variables of `url-retrieve' for a request, then run BODY.
SPEC is (URL BACKEND METHOD PATH PARAMS DATA): URL is bound to the URL
of the request of METHOD on PATH of BACKEND, with PARAMS and DATA."
  (declare (indent 1) (debug ((symbolp form form form form form) body)))
  (let ((auth (make-symbol "auth")))
    (pcase-let ((`(,url ,backend ,method ,path ,params ,data) spec))
      `(let* ((,auth (magit-gerrit-rest--auth ,backend))
              (,url (magit-gerrit-rest--url ,backend ,path ,params ,auth))
              (url-request-method ,method)
              (url-request-extra-headers (magit-gerrit-rest--headers ,auth ,data))
              (url-request-data (when ,data
                                  (encode-coding-string (magit-gerrit--json-encode ,data) 'utf-8)))
              (url-request-noninteractive t)
              (url-show-status nil))
         ,@body))))

(defun magit-gerrit-rest--retrieve (backend method path &optional params data)
  "Send the request METHOD on PATH of BACKEND, return (STATUS . BODY).
PARAMS are the query parameters (see `magit-gerrit-rest--url'), DATA
is encoded as the JSON body of the request.  STATUS is 0 when the
server can't be reached.  The requests are logged in
`magit-gerrit-log-buffer-name'."
  (magit-gerrit-rest--with-request (url backend method path params data)
    (let* ((buffer (condition-case err
                       (url-retrieve-synchronously url t t magit-gerrit-rest-timeout)
                     (error (magit-gerrit--log (concat method " " url) 0 (error-message-string err))
                            (user-error "Gerrit request %s %s failed: %s"
                                        method url (error-message-string err)))))
           (response (if buffer (magit-gerrit-rest--response buffer) (cons 0 "timeout"))))
      (magit-gerrit--log (concat method " " url) (car response)
                         (unless (<= 200 (car response) 299) (cdr response)))
      response)))

(defun magit-gerrit-rest--request (backend method path &optional params data)
  "Send the request METHOD on PATH of BACKEND, return the parsed response.
See `magit-gerrit-rest--retrieve' for PARAMS and DATA.  Signal a
`user-error' when the request fails."
  (pcase-let ((`(,status . ,body) (magit-gerrit-rest--retrieve backend method path params data)))
    (if (<= 200 status 299)
        (magit-gerrit-rest--parse-body body)
      (user-error "%s" (magit-gerrit-rest--error
                        method (magit-gerrit-rest--url backend path params nil) status body)))))

(defun magit-gerrit-rest--request-async (backend method path params callback)
  "Send the request METHOD on PATH of BACKEND with PARAMS in the background.
CALLBACK is called with the parsed response and nil, or with nil and
an error message."
  (magit-gerrit-rest--with-request (url backend method path params nil)
    (url-retrieve
     url
     (lambda (_status)
       (pcase-let ((`(,status . ,body) (magit-gerrit-rest--response (current-buffer))))
         (magit-gerrit--log (concat method " " url) status (unless (<= 200 status 299) body))
         (if (<= 200 status 299)
             (let ((result (condition-case err
                               (cons t (magit-gerrit-rest--parse-body body))
                               (t (cons nil (error-message-string err))))))
               (if (car result)
                   (funcall callback (cdr result) nil)
                 (funcall callback nil (cdr result))))
           (funcall callback nil (magit-gerrit-rest--error method url status body)))))
     nil t t)))

(defun magit-gerrit-rest--body (&rest fields)
  "Return a JSON object of FIELDS, an alternation of names and values.
The fields whose value is nil are omitted; the object is {} when they
all are."
  (or (cl-loop for (name value) on fields by #'cddr
               when value collect (cons name value))
      (make-hash-table)))


;;; Conversion to the change model

;; The REST entities are converted to the format of "gerrit query", the
;; model of the frontend (see "Change data model" in magit-gerrit.el).

(defun magit-gerrit-rest--time (timestamp)
  "Convert the TIMESTAMP of Gerrit to seconds since the epoch, or nil.
Gerrit's timestamps are in UTC, as \"yyyy-mm-dd hh:mm:ss.fffffffff\"."
  (when (and (stringp timestamp)
             (string-match (concat "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)[ T]"
                                   "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")
                           timestamp))
    (let ((field (lambda (n) (string-to-number (match-string n timestamp)))))
      (time-convert (encode-time (list (funcall field 6) (funcall field 5) (funcall field 4)
                                       (funcall field 3) (funcall field 2) (funcall field 1)
                                       nil nil t))
                    'integer))))

(defun magit-gerrit-rest--account (info)
  "Convert the AccountInfo entity INFO to an account."
  (when info
    (seq-filter #'cdr
                `((name . ,(or (alist-get 'name info) (alist-get 'display_name info)))
                  (email . ,(alist-get 'email info))
                  (username . ,(alist-get 'username info))
                  (accountId . ,(alist-get '_account_id info))))))

(defun magit-gerrit-rest--ref (number patchset)
  "Return the ref of PATCHSET of the change NUMBER."
  (format "refs/changes/%02d/%d/%d" (% number 100) number patchset))

(defconst magit-gerrit-rest--file-types
  '(("A" . "ADDED") ("D" . "DELETED") ("R" . "RENAMED") ("C" . "COPIED") ("W" . "REWRITE"))
  "The types of the files for the status letters of FileInfo entities.")

(defun magit-gerrit-rest--files (files)
  "Convert FILES, an alist of (PATH . FileInfo), to a list of files."
  (mapcar (lambda (entry)
            (let ((info (cdr entry)))
              (seq-filter
               #'cdr
               `((file . ,(symbol-name (car entry)))
                 (type . ,(or (cdr (assoc (alist-get 'status info) magit-gerrit-rest--file-types))
                              "MODIFIED"))
                 (insertions . ,(or (alist-get 'lines_inserted info) 0))
                 (deletions . ,(- (or (alist-get 'lines_deleted info) 0)))
                 (fileOld . ,(alist-get 'old_path info))))))
          files))

(defun magit-gerrit-rest--comment (path info)
  "Convert the CommentInfo entity INFO, on the file PATH, to a comment."
  (seq-filter
   #'cdr
   `((file . ,path)
     (line . ,(alist-get 'line info))
     (reviewer . ,(magit-gerrit-rest--account (alist-get 'author info)))
     (message . ,(alist-get 'message info))
     (id . ,(alist-get 'id info))
     (inReplyTo . ,(alist-get 'in_reply_to info))
     (unresolved . ,(eq (alist-get 'unresolved info) t))
     (updated . ,(magit-gerrit-rest--time (alist-get 'updated info)))
     (side . ,(alist-get 'side info))
     (patchSet . ,(alist-get 'patch_set info))
     (revision . ,(alist-get 'commit_id info)))))

(defun magit-gerrit-rest--comments (map)
  "Convert MAP, an alist of (PATH . CommentInfo list), to a comment list."
  (mapcan (lambda (entry)
            (let ((path (symbol-name (car entry))))
              (mapcar (lambda (info) (magit-gerrit-rest--comment path info)) (cdr entry))))
          map))

(defun magit-gerrit-rest--approvals (labels)
  "Return the votes of LABELS, an alist of (LABEL . LabelInfo)."
  (mapcan (lambda (entry)
            (let ((label (symbol-name (car entry))))
              (seq-keep (lambda (approval)
                          (let ((value (alist-get 'value approval)))
                            (when (and (numberp value) (/= value 0))
                              `((type . ,label)
                                (value . ,(number-to-string value))
                                (by . ,(magit-gerrit-rest--account approval))
                                (grantedOn . ,(magit-gerrit-rest--time (alist-get 'date approval)))))))
                        (alist-get 'all (cdr entry)))))
          labels))

(defun magit-gerrit-rest--vote-value (string)
  "Return the number of the vote STRING, such as \" 0\" or \"+1\"."
  (string-to-number (string-trim (format "%s" string))))

(defun magit-gerrit-rest--label-ranges (labels)
  "Return the ranges of LABELS as an alist of (LABEL MIN . MAX)."
  (seq-keep (lambda (entry)
              (when-let* ((values (mapcar (lambda (value) (magit-gerrit-rest--vote-value (car value)))
                                          (alist-get 'values (cdr entry)))))
                (cons (symbol-name (car entry)) (cons (apply #'min values) (apply #'max values)))))
            labels))

(defun magit-gerrit-rest--submit-records (info)
  "Return the submit records of the ChangeInfo INFO, or nil.
They are deduced from the `submittable' attribute and the labels."
  (when (assq 'submittable info)
    `(((status . ,(cond ((member (alist-get 'status info) '("MERGED" "ABANDONED")) "CLOSED")
                        ((alist-get 'submittable info) "OK")
                        (t "NOT_READY")))
       (labels . ,(mapcar (lambda (entry)
                            (let ((label (cdr entry)))
                              `((label . ,(symbol-name (car entry)))
                                (status . ,(cond ((alist-get 'rejected label) "REJECT")
                                                 ((alist-get 'approved label) "OK")
                                                 ((alist-get 'optional label) "MAY")
                                                 (t "NEED"))))))
                          (alist-get 'labels info)))))))

(defun magit-gerrit-rest--patchsets (info comments)
  "Return the patchsets of the ChangeInfo INFO, with their inline COMMENTS."
  (let ((number (alist-get '_number info)))
    (sort (mapcar
           (lambda (entry)
             (let* ((revision (cdr entry))
                    (ps (alist-get '_number revision))
                    (files (magit-gerrit-rest--files (alist-get 'files revision)))
                    (real-files (seq-remove (lambda (f) (string-prefix-p "/" (alist-get 'file f)))
                                            files)))
               (seq-filter
                #'cdr
                `((number . ,ps)
                  (revision . ,(symbol-name (car entry)))
                  (ref . ,(or (alist-get 'ref revision) (magit-gerrit-rest--ref number ps)))
                  (uploader . ,(magit-gerrit-rest--account (alist-get 'uploader revision)))
                  (createdOn . ,(magit-gerrit-rest--time (alist-get 'created revision)))
                  (kind . ,(alist-get 'kind revision))
                  (files . ,files)
                  (sizeInsertions . ,(when real-files
                                       (apply #'+ (mapcar (lambda (f) (alist-get 'insertions f))
                                                          real-files))))
                  (sizeDeletions . ,(when real-files
                                      (apply #'+ (mapcar (lambda (f) (alist-get 'deletions f))
                                                         real-files))))
                  (comments . ,(seq-filter (lambda (c) (eql (alist-get 'patchSet c) ps))
                                           comments))))))
           (alist-get 'revisions info))
          (lambda (a b) (< (alist-get 'number a) (alist-get 'number b))))))

(defun magit-gerrit-rest--related (number related)
  "Return the dependencies of the change NUMBER, from the RELATED list.
RELATED is the list of RelatedChangeAndCommitInfo entities, the
descendants first.  Return (DEPENDS-ON . NEEDED-BY)."
  (let* ((deps (mapcar (lambda (info)
                         (let ((n (alist-get '_change_number info))
                               (ps (alist-get '_revision_number info)))
                           `((number . ,n)
                             (id . ,(alist-get 'change_id info))
                             (revision . ,(alist-get 'commit (alist-get 'commit info)))
                             (ref . ,(and n ps (magit-gerrit-rest--ref n ps)))
                             (isCurrentPatchSet . ,(and ps (eql ps (alist-get '_current_revision_number info)))))))
                       related))
         (index (seq-position deps number (lambda (dep n) (eql (alist-get 'number dep) n)))))
    (if index
        (cons (seq-drop deps (1+ index)) (reverse (seq-take deps index)))
      (cons nil nil))))

(defun magit-gerrit-rest--change (backend info &optional extra)
  "Convert the ChangeInfo entity INFO of BACKEND to a change.
EXTRA is a plist with the properties :comments (the inline comments),
:drafts, :related (see `magit-gerrit-rest--related') and :starred."
  (let* ((number (alist-get '_number info))
         (project (alist-get 'project info))
         (current (alist-get 'current_revision info))
         (labels (alist-get 'labels info))
         (approvals (magit-gerrit-rest--approvals labels))
         (patchsets (magit-gerrit-rest--patchsets info (plist-get extra :comments)))
         (current-ps (or (seq-find (lambda (ps) (equal (alist-get 'revision ps) current)) patchsets)
                         (when-let* ((n (alist-get 'current_revision_number info)))
                           `((number . ,n) (ref . ,(magit-gerrit-rest--ref number n))))))
         (current-ps (append
                      `((approvals . ,approvals)
                        (sizeInsertions . ,(or (alist-get 'insertions info) 0))
                        (sizeDeletions . ,(- (or (alist-get 'deletions info) 0))))
                      (seq-remove (lambda (field) (memq (car field) '(sizeInsertions sizeDeletions)))
                                  current-ps)))
         (patchsets (mapcar (lambda (ps) (if (equal (alist-get 'revision ps) current) current-ps ps))
                            patchsets))
         (reviewers (alist-get 'reviewers info))
         (related (plist-get extra :related))
         (status (alist-get 'status info)))
    (seq-filter
     #'cdr
     `((project . ,project)
       (branch . ,(alist-get 'branch info))
       (topic . ,(alist-get 'topic info))
       (id . ,(alist-get 'change_id info))
       (number . ,number)
       (subject . ,(alist-get 'subject info))
       (owner . ,(magit-gerrit-rest--account (alist-get 'owner info)))
       (url . ,(format "%s/c/%s/+/%s" (magit-gerrit-rest-backend-url backend) project number))
       (commitMessage . ,(alist-get 'message (alist-get 'commit (cdr (assq (and current (intern current))
                                                                          (alist-get 'revisions info))))))
       (createdOn . ,(magit-gerrit-rest--time (alist-get 'created info)))
       (lastUpdated . ,(magit-gerrit-rest--time (alist-get 'updated info)))
       (open . ,(equal status "NEW"))
       (status . ,status)
       (wip . ,(eq (alist-get 'work_in_progress info) t))
       (private . ,(eq (alist-get 'is_private info) t))
       (starred . ,(or (eq (alist-get 'starred info) t) (plist-get extra :starred)))
       (hashtags . ,(alist-get 'hashtags info))
       (currentPatchSet . ,current-ps)
       (patchSets . ,patchsets)
       (comments . ,(mapcar (lambda (msg)
                              `((timestamp . ,(magit-gerrit-rest--time (alist-get 'date msg)))
                                (reviewer . ,(or (magit-gerrit-rest--account (alist-get 'author msg))
                                                 '((name . "Gerrit"))))
                                (message . ,(alist-get 'message msg))))
                            (alist-get 'messages info)))
       (allReviewers . ,(mapcar #'magit-gerrit-rest--account (alist-get 'REVIEWER reviewers)))
       (ccs . ,(mapcar #'magit-gerrit-rest--account (alist-get 'CC reviewers)))
       (submitRecords . ,(magit-gerrit-rest--submit-records info))
       (submitRequirements . ,(mapcar (lambda (req) `((name . ,(alist-get 'name req))
                                                      (status . ,(alist-get 'status req))))
                                      (alist-get 'submit_requirements info)))
       (labelRanges . ,(magit-gerrit-rest--label-ranges labels))
       (permittedLabels . ,(mapcar (lambda (entry)
                                     (cons (symbol-name (car entry))
                                           (mapcar #'magit-gerrit-rest--vote-value (cdr entry))))
                                   (alist-get 'permitted_labels info)))
       (drafts . ,(plist-get extra :drafts))
       (dependsOn . ,(car related))
       (neededBy . ,(cdr related))))))


;;; Queries

(defconst magit-gerrit-rest--query-options
  '((:current-patch-set "CURRENT_REVISION" "DETAILED_LABELS")
    (:patch-sets "ALL_REVISIONS")
    (:approvals "DETAILED_LABELS")
    (:files "CURRENT_FILES")
    (:comments "MESSAGES")
    (:commit-message "CURRENT_COMMIT")
    (:submit-records "SUBMITTABLE" "DETAILED_LABELS")
    (:reviewers "DETAILED_LABELS"))
  "Options of the REST API for the keywords of `magit-gerrit-query-options'.
The inline comments and the dependencies are fetched separately (see
`magit-gerrit-backend-get-change').")

(defun magit-gerrit-rest--options (options)
  "Return the REST options for the keywords OPTIONS, as query parameters."
  (mapcar (lambda (option) (cons "o" option))
          (seq-uniq (cons "DETAILED_ACCOUNTS"
                          (mapcan (lambda (option)
                                    (copy-sequence (alist-get option magit-gerrit-rest--query-options)))
                                  options)))))

(defun magit-gerrit-rest--query-params (query options)
  "Return the parameters of the request running QUERY with OPTIONS."
  (cons (cons "q" (if (listp query) (string-join query " ") query))
        (magit-gerrit-rest--options options)))

(cl-defmethod magit-gerrit-backend-query ((backend magit-gerrit-rest-backend) query options)
  "Run QUERY with OPTIONS with the REST BACKEND, see `magit-gerrit-backend-query'."
  (mapcar (lambda (info) (magit-gerrit-rest--change backend info))
          (magit-gerrit-rest--request backend "GET" "/changes/"
                                      (magit-gerrit-rest--query-params query options))))

(cl-defmethod magit-gerrit-backend-query-async ((backend magit-gerrit-rest-backend) query options callback)
  "Run QUERY with OPTIONS with the REST BACKEND in the background.
See `magit-gerrit-backend-query-async' for CALLBACK."
  (magit-gerrit-rest--request-async
   backend "GET" "/changes/" (magit-gerrit-rest--query-params query options)
   (lambda (infos error)
     (if error
         (funcall callback nil error)
       (funcall callback (mapcar (lambda (info) (magit-gerrit-rest--change backend info)) infos)
                nil)))))

(defun magit-gerrit-rest--change-path (change &rest segments)
  "Return the path of the endpoint SEGMENTS of CHANGE."
  (apply #'magit-gerrit-rest--path "changes"
         (if (numberp change) change (magit-gerrit-change-number change))
         segments))

(defun magit-gerrit-rest--get-change-info (backend number)
  "Return the ChangeInfo of the change NUMBER from BACKEND, or nil.
It has all the details of the change.  The
`magit-gerrit-rest-extra-change-options' are dropped if the server
refuses them."
  (let* ((path (magit-gerrit-rest--change-path number))
         (base (magit-gerrit-rest--options magit-gerrit-query-options))
         (extra (mapcar (lambda (option) (cons "o" option)) magit-gerrit-rest-extra-change-options))
         (response (magit-gerrit-rest--retrieve backend "GET" path (append base extra))))
    (when (and extra (eq (car response) 400))
      (setq response (magit-gerrit-rest--retrieve backend "GET" path base)))
    (pcase-let ((`(,status . ,body) response))
      (cond ((<= 200 status 299) (magit-gerrit-rest--parse-body body))
            ((eq status 404) nil)
            (t (user-error "%s" (magit-gerrit-rest--error
                                 "GET" (magit-gerrit-rest--url backend path nil nil) status body)))))))

(cl-defmethod magit-gerrit-backend-get-change ((backend magit-gerrit-rest-backend) number)
  "Return the change NUMBER from the REST BACKEND, with all its details.
This includes the inline comments, the draft comments of the user,
the related changes and whether the user starred it."
  (when-let* ((info (magit-gerrit-rest--get-change-info backend number)))
    (let* ((authenticated (magit-gerrit-rest--auth backend))
           (comments (magit-gerrit-rest--comments
                      (magit-gerrit-rest--request backend "GET" (magit-gerrit-rest--change-path number "comments"))))
           (drafts (when authenticated
                     (magit-gerrit-rest--comments
                      (magit-gerrit-rest--request backend "GET" (magit-gerrit-rest--change-path number "drafts")))))
           (related (ignore-errors
                      (alist-get 'changes (magit-gerrit-rest--request
                                           backend "GET" (magit-gerrit-rest--change-path
                                                          number "revisions" "current" "related")))))
           (starred (when (and authenticated (not (eq (alist-get 'starred info) t)))
                      (ignore-errors
                        (magit-gerrit-rest--request backend "GET" "/changes/"
                                                    `(("q" . ,(format "change:%s is:starred" number))))))))
      (magit-gerrit-rest--change backend info
                                 (list :comments comments :drafts drafts
                                       :related (magit-gerrit-rest--related number related)
                                       :starred (and starred t))))))


;;; Actions

(defun magit-gerrit-rest--review (backend change input &optional revision)
  "Post the ReviewInput INPUT on REVISION of CHANGE with BACKEND.
REVISION defaults to the current patchset."
  (magit-gerrit-rest--request backend "POST"
                              (magit-gerrit-rest--change-path change "revisions" (or revision "current") "review")
                              nil input))

(defun magit-gerrit-rest--parse-vote (vote)
  "Parse VOTE, a \"LABEL=VALUE\" string, as (LABEL . VALUE)."
  (if (string-match "\\`\\(.+\\)=\\([-+]?[0-9]+\\)\\'" vote)
      (cons (match-string 1 vote) (string-to-number (match-string 2 vote)))
    (user-error "Invalid vote: %s" vote)))

(cl-defmethod magit-gerrit-backend-vote ((backend magit-gerrit-rest-backend) change votes options)
  "Vote VOTES on CHANGE with the REST BACKEND, OPTIONS is a plist.
See `magit-gerrit-backend-vote'."
  (magit-gerrit-rest--review backend change
                             (magit-gerrit-rest--body
                              'labels (mapcar #'magit-gerrit-rest--parse-vote votes)
                              'message (plist-get options :message)
                              'notify (plist-get options :notify))
                             (magit-gerrit-change-revision change))
  (when (plist-get options :submit)
    (magit-gerrit-backend-submit backend change (list :notify (plist-get options :notify)))))

(cl-defmethod magit-gerrit-backend-submit ((backend magit-gerrit-rest-backend) change options)
  "Submit CHANGE with the REST BACKEND, OPTIONS is a plist.
The message, if any, is posted before submitting."
  (when-let* ((message (plist-get options :message)))
    (magit-gerrit-rest--review backend change (magit-gerrit-rest--body 'message message)))
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "submit") nil
                              (magit-gerrit-rest--body 'notify (plist-get options :notify))))

(cl-defmethod magit-gerrit-backend-abandon ((backend magit-gerrit-rest-backend) change options)
  "Abandon CHANGE with the REST BACKEND, OPTIONS is a plist."
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "abandon") nil
                              (magit-gerrit-rest--body 'message (plist-get options :message)
                                                       'notify (plist-get options :notify))))

(cl-defmethod magit-gerrit-backend-restore ((backend magit-gerrit-rest-backend) change options)
  "Restore CHANGE with the REST BACKEND, OPTIONS is a plist."
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "restore") nil
                              (magit-gerrit-rest--body 'message (plist-get options :message))))

(cl-defmethod magit-gerrit-backend-rebase ((backend magit-gerrit-rest-backend) change _options)
  "Rebase CHANGE with the REST BACKEND."
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "rebase") nil
                              (magit-gerrit-rest--body)))

(cl-defmethod magit-gerrit-backend-move ((backend magit-gerrit-rest-backend) change branch options)
  "Move CHANGE to BRANCH with the REST BACKEND, OPTIONS is a plist."
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "move") nil
                              (magit-gerrit-rest--body 'destination_branch branch
                                                       'message (plist-get options :message))))

(cl-defmethod magit-gerrit-backend-set-topic ((backend magit-gerrit-rest-backend) change topic)
  "Set the TOPIC of CHANGE with the REST BACKEND (nil to remove it)."
  (if topic
      (magit-gerrit-rest--request backend "PUT" (magit-gerrit-rest--change-path change "topic") nil
                                  (magit-gerrit-rest--body 'topic topic))
    (magit-gerrit-rest--request backend "DELETE" (magit-gerrit-rest--change-path change "topic"))))

(cl-defmethod magit-gerrit-backend-add-reviewers ((backend magit-gerrit-rest-backend) change accounts
                                                  &optional cc)
  "Add ACCOUNTS as reviewers (or in CC when CC is non-nil) of CHANGE.
BACKEND is the REST backend."
  (magit-gerrit-rest--review backend change
                             (magit-gerrit--reviewers-input accounts (if cc "CC" "REVIEWER"))))

(cl-defmethod magit-gerrit-backend-remove-reviewers ((backend magit-gerrit-rest-backend) change accounts)
  "Remove the reviewers ACCOUNTS of CHANGE with the REST BACKEND."
  (dolist (account accounts)
    (magit-gerrit-rest--request backend "DELETE"
                                (magit-gerrit-rest--change-path change "reviewers" account))))

(cl-defmethod magit-gerrit-backend-set-wip ((backend magit-gerrit-rest-backend) change wip options)
  "Set the WIP state of CHANGE with the REST BACKEND, OPTIONS is a plist."
  (magit-gerrit-rest--request backend "POST"
                              (magit-gerrit-rest--change-path change (if wip "wip" "ready")) nil
                              (magit-gerrit-rest--body 'message (plist-get options :message))))

(defun magit-gerrit-rest--draft-input (comment)
  "Return the DraftInput entity of COMMENT, a plist.
See `magit-gerrit-backend-post-comments'."
  (cons (cons 'path (plist-get comment :file)) (magit-gerrit--comment-input comment)))

(cl-defmethod magit-gerrit-backend-post-comments ((backend magit-gerrit-rest-backend) change revision
                                                  comments options)
  "Post COMMENTS on REVISION of CHANGE with the REST BACKEND.
See `magit-gerrit-backend-post-comments' for OPTIONS."
  (if (plist-get options :draft)
      (dolist (comment comments)
        (magit-gerrit-rest--request backend "PUT"
                                    (magit-gerrit-rest--change-path change "revisions" revision "drafts")
                                    nil (magit-gerrit-rest--draft-input comment)))
    (magit-gerrit-rest--review backend change
                               (magit-gerrit-rest--body
                                'comments (magit-gerrit--comments-input comments)
                                'message (plist-get options :message)
                                'notify (plist-get options :notify)
                                'drafts "KEEP")
                               revision)))

(defun magit-gerrit-rest--draft-path (change draft)
  "Return the path of the DRAFT comment of CHANGE."
  (magit-gerrit-rest--change-path change "revisions"
                                  (or (alist-get 'revision draft) (alist-get 'patchSet draft))
                                  "drafts" (alist-get 'id draft)))

(cl-defmethod magit-gerrit-backend-update-draft ((backend magit-gerrit-rest-backend) change draft
                                                 message unresolved)
  "Set the MESSAGE and UNRESOLVED state of the DRAFT of CHANGE.
BACKEND is the REST backend."
  (magit-gerrit-rest--request
   backend "PUT" (magit-gerrit-rest--draft-path change draft) nil
   (magit-gerrit-rest--draft-input
    (list :file (alist-get 'file draft) :line (alist-get 'line draft)
          :side (alist-get 'side draft) :in-reply-to (alist-get 'inReplyTo draft)
          :message message :unresolved unresolved))))

(cl-defmethod magit-gerrit-backend-delete-draft ((backend magit-gerrit-rest-backend) change draft)
  "Delete the DRAFT comment of CHANGE with the REST BACKEND."
  (magit-gerrit-rest--request backend "DELETE" (magit-gerrit-rest--draft-path change draft)))

(cl-defmethod magit-gerrit-backend-publish-drafts ((backend magit-gerrit-rest-backend) change options)
  "Publish the drafts of CHANGE with the REST BACKEND, OPTIONS is a plist."
  (magit-gerrit-rest--review backend change
                             (magit-gerrit-rest--body 'drafts "PUBLISH_ALL_REVISIONS"
                                                      'message (plist-get options :message)
                                                      'notify (plist-get options :notify))))

(cl-defmethod magit-gerrit-backend-set-hashtags ((backend magit-gerrit-rest-backend) change add remove)
  "Add the hashtags ADD to CHANGE and remove REMOVE, with the REST BACKEND."
  (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "hashtags") nil
                              (magit-gerrit-rest--body 'add (and add (vconcat add))
                                                       'remove (and remove (vconcat remove)))))

(cl-defmethod magit-gerrit-backend-set-starred ((backend magit-gerrit-rest-backend) change starred)
  "Star CHANGE when STARRED is non-nil, unstar it otherwise.
BACKEND is the REST backend."
  (magit-gerrit-rest--request backend (if starred "PUT" "DELETE")
                              (magit-gerrit-rest--path "accounts" "self" "starred.changes"
                                                       (magit-gerrit-change-number change))))

(cl-defmethod magit-gerrit-backend-set-private ((backend magit-gerrit-rest-backend) change private options)
  "Mark CHANGE as private if PRIVATE is non-nil, as public otherwise.
BACKEND is the REST backend, OPTIONS a plist."
  (if private
      (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "private") nil
                                  (magit-gerrit-rest--body 'message (plist-get options :message)))
    (magit-gerrit-rest--request backend "DELETE" (magit-gerrit-rest--change-path change "private"))))

(cl-defmethod magit-gerrit-backend-set-commit-message ((backend magit-gerrit-rest-backend) change
                                                       message options)
  "Set the commit MESSAGE of CHANGE with the REST BACKEND.
OPTIONS is a plist."
  (magit-gerrit-rest--request backend "PUT" (magit-gerrit-rest--change-path change "message") nil
                              (magit-gerrit-rest--body 'message message
                                                       'notify (plist-get options :notify))))

(cl-defmethod magit-gerrit-backend-revert ((backend magit-gerrit-rest-backend) change options)
  "Revert CHANGE with the REST BACKEND and return the new change.
OPTIONS is a plist."
  (magit-gerrit-rest--change
   backend
   (magit-gerrit-rest--request backend "POST" (magit-gerrit-rest--change-path change "revert") nil
                               (magit-gerrit-rest--body 'message (plist-get options :message)
                                                        'notify (plist-get options :notify)))))

(cl-defmethod magit-gerrit-backend-cherry-pick ((backend magit-gerrit-rest-backend) change revision
                                                branch options)
  "Cherry-pick REVISION of CHANGE to BRANCH with the REST BACKEND.
Return the new change.  OPTIONS is a plist."
  (magit-gerrit-rest--change
   backend
   (magit-gerrit-rest--request backend "POST"
                               (magit-gerrit-rest--change-path change "revisions" revision "cherrypick")
                               nil (magit-gerrit-rest--body 'destination branch
                                                            'message (plist-get options :message)))))

(cl-defmethod magit-gerrit-backend-suggest-accounts ((backend magit-gerrit-rest-backend) query
                                                     &optional change)
  "Return the accounts matching QUERY, using the REST BACKEND.
When CHANGE is non-nil, suggest reviewers for it."
  (seq-keep (lambda (entry)
              (magit-gerrit--account-string
               (magit-gerrit-rest--account (or (alist-get 'account entry)
                                               (and (alist-get '_account_id entry) entry)))))
            (if change
                (magit-gerrit-rest--request backend "GET"
                                            (magit-gerrit-rest--change-path change "suggest_reviewers")
                                            `(("q" . ,query) ("n" . "10")))
              (magit-gerrit-rest--request backend "GET" "/accounts/"
                                          `(("suggest") ("q" . ,query) ("n" . "10"))))))

(provide 'magit-gerrit-rest)

;;; magit-gerrit-rest.el ends here
