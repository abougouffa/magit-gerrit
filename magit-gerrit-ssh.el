;;; magit-gerrit-ssh.el --- SSH backend of magit-gerrit  -*- lexical-binding: t; -*-

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

;; The SSH backend of magit-gerrit, see `magit-gerrit-backend'.
;;
;; It runs the commands of Gerrit's SSH daemon: "gerrit query",
;; "gerrit review" (also with a ReviewInput read from its standard
;; input, for CCs, the WIP state and inline comments),
;; "gerrit set-reviewers" and "gerrit set-topic".
;;
;; The comments returned by "gerrit query" have no identifier, and the
;; SSH commands can neither list draft comments nor tell the values of
;; labels: replying to comments, resolving them, draft comments, the
;; label values of the server, hashtags, stars, private changes,
;; reverts... need the REST backend (see magit-gerrit-rest.el).
;;
;; The backend is used for repositories whose remote is an SSH URL on
;; Gerrit's port (`magit-gerrit-port'), or when `magit-gerrit-ssh-creds'
;; is set.

;;; Code:

(require 'magit-gerrit)


;;; Options

(defcustom magit-gerrit-ssh-program "ssh"
  "The SSH program used to run Gerrit commands."
  :group 'magit-gerrit
  :type 'string)

(defcustom magit-gerrit-scp-program "scp"
  "The scp program used to download the commit-msg hook."
  :group 'magit-gerrit
  :type 'string)

(defvar-local magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server.")

(defvar-local magit-gerrit-port 29418
  "The default port to use with the Gerrit host.")

(put 'magit-gerrit-ssh-creds 'safe-local-variable #'stringp)
(put 'magit-gerrit-port 'safe-local-variable #'integerp)


;;; Backend

(cl-defstruct (magit-gerrit-ssh-backend
               (:include magit-gerrit-backend)
               (:constructor magit-gerrit-ssh-backend-create)
               (:copier nil))
  "Backend running the commands of Gerrit's SSH daemon."
  (creds nil :documentation "The SSH credentials, as USER@HOST.")
  (port 29418 :documentation "The port of Gerrit's SSH daemon."))

(defun magit-gerrit-detect-ssh-creds (remote-url)
  "Derive `magit-gerrit-ssh-creds' from REMOTE-URL.
Assumes REMOTE-URL is a Gerrit repo if scheme is SSH and port is
`magit-gerrit-port' or when `magit-gerrit-force-enable' is non-nil."
  (prog1 (or magit-gerrit-ssh-creds
             (when-let* ((remote-url remote-url)
                         (url (url-generic-parse-url remote-url))
                         (user (url-user url))
                         (host (url-host url))
                         ((and (string= "ssh" (url-type url))
                               (or magit-gerrit-force-enable (eq magit-gerrit-port (url-port url))))))
               (setq-local magit-gerrit-ssh-creds (format "%s@%s" user host))))
    (let ((inhibit-message t))
      (message "Detected magit-gerrit-ssh-creds=%s" magit-gerrit-ssh-creds))))

(defun magit-gerrit-ssh--make-backend (remote-url _explicit)
  "Return an SSH backend for REMOTE-URL, or nil.
The backend is usable when `magit-gerrit-ssh-creds' is set or can be
deduced from REMOTE-URL, see `magit-gerrit-detect-ssh-creds'."
  (when-let* ((creds (magit-gerrit-detect-ssh-creds remote-url)))
    (magit-gerrit-ssh-backend-create :creds creds :port magit-gerrit-port)))

(defun magit-gerrit-ssh--variables-backend ()
  "Return an SSH backend made of the current SSH variables.
Use the backend of the buffer when it is an SSH backend."
  (let ((backend (magit-gerrit-current-backend t)))
    (if (magit-gerrit-ssh-backend-p backend)
        backend
      (magit-gerrit-ssh-backend-create
       :creds (or magit-gerrit-ssh-creds (user-error "`magit-gerrit-ssh-creds' must be set!"))
       :port magit-gerrit-port))))

(magit-gerrit-register-backend 'ssh #'magit-gerrit-ssh--make-backend
                               'magit-gerrit-ssh-creds 'magit-gerrit-port)


;;; Running commands

(defun magit-gerrit-ssh--quote (arg)
  "Quote ARG for the command line parser of Gerrit's SSH daemon.
Gerrit splits the command line itself (there is no remote shell), it
supports POSIX-like single quotes, double quotes and backslashes."
  (if (string-match-p "\\`[-[:alnum:]_.,:/=@+%^~]+\\'" arg)
      arg
    (concat "'" (string-replace "'" "'\\''" arg) "'")))

(defun magit-gerrit-ssh--command (backend cmd args)
  "Build the SSH parameters to run Gerrit's CMD with ARGS on BACKEND.
ARGS are flattened, nil elements are removed and the remaining ones
are quoted for Gerrit."
  `("-x" "-p"
    ,(number-to-string (magit-gerrit-ssh-backend-port backend))
    ,(magit-gerrit-ssh-backend-creds backend)
    "gerrit"
    ,cmd
    ,@(mapcar (lambda (arg) (magit-gerrit-ssh--quote (format "%s" arg)))
              (delq nil (flatten-list args)))))

(defun magit-gerrit-ssh--log (ssh-args exit-code stderr)
  "Log the SSH call with SSH-ARGS, its EXIT-CODE and STDERR."
  (magit-gerrit--log (string-join (cons magit-gerrit-ssh-program ssh-args) " ")
                     exit-code stderr))

(defun magit-gerrit-ssh--failure-message (cmd exit-code stderr)
  "Return the error message of the failed Gerrit command CMD.
EXIT-CODE and STDERR are the exit code and error output of SSH."
  (format "Gerrit command \"%s\" failed: %s" cmd
          (if (string-empty-p stderr)
              (format "exit code %s" exit-code)
            (car (split-string stderr "\n" t)))))

(defun magit-gerrit-ssh--run (backend input cmd args)
  "Call Gerrit's command CMD with ARGS via SSH on BACKEND, return its output.
When INPUT is non-nil, it is sent to the standard input of the
command.  ARGS are processed as in `magit-gerrit-ssh--command'.
Signal a `user-error' containing the error output when the command
fails.  All the calls are logged in `magit-gerrit-log-buffer-name'."
  (let ((ssh-args (magit-gerrit-ssh--command backend cmd args))
        (err-file (make-temp-file "magit-gerrit-"))
        (in-file (when input (make-temp-file "magit-gerrit-in-" nil nil input)))
        ;; SSH is always run locally, even for remote repositories
        (default-directory (if (file-remote-p default-directory)
                               temporary-file-directory
                             default-directory)))
    (unwind-protect
        (with-temp-buffer
          (let* ((coding-system-for-write 'utf-8)
                 (coding-system-for-read 'utf-8)
                 (exit-code (apply #'call-process magit-gerrit-ssh-program
                                   in-file (list t err-file) nil ssh-args))
                 (stderr (with-temp-buffer
                           (insert-file-contents err-file)
                           (string-trim (buffer-string)))))
            (magit-gerrit-ssh--log ssh-args exit-code stderr)
            (unless (eq exit-code 0)
              (user-error "%s" (magit-gerrit-ssh--failure-message cmd exit-code stderr)))
            (buffer-string)))
      (delete-file err-file)
      (when in-file (delete-file in-file)))))

(defun magit-gerrit-ssh-call (cmd &rest args)
  "Call Gerrit's command CMD with ARGS via SSH and return its output.
The SSH backend of the current buffer is used, or one made of
`magit-gerrit-ssh-creds' and `magit-gerrit-port'.  See
`magit-gerrit-ssh--run'."
  (magit-gerrit-ssh--run (magit-gerrit-ssh--variables-backend) nil cmd args))

(defun magit-gerrit-ssh--review (backend change &rest args)
  "Run \"gerrit review\" with ARGS on the current patchset of CHANGE.
BACKEND is the SSH backend."
  (magit-gerrit-ssh--run backend nil "review"
                         (list "--project" (magit-gerrit-change-project change)
                               args
                               (magit-gerrit-change-revision change))))

(defun magit-gerrit-ssh--review-json (backend change input &optional revision)
  "Run \"gerrit review --json\" on BACKEND for REVISION of CHANGE.
REVISION defaults to the current patchset.  INPUT is an alist, sent
as a ReviewInput entity, see
https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#review-input"
  (magit-gerrit-ssh--run backend (magit-gerrit--json-encode input) "review"
                         (list "--project" (magit-gerrit-change-project change)
                               "--json"
                               (or revision (magit-gerrit-change-revision change)))))

(defun magit-gerrit-ssh--message-args (options)
  "Return the \"gerrit review\" arguments for the plist OPTIONS.
These are the message and the notification setting."
  (append
   (when-let* ((msg (plist-get options :message)))
     (list "--message" msg))
   (when-let* ((notify (plist-get options :notify)))
     (list "--notify" notify))))


;;; Queries

(defconst magit-gerrit-ssh--query-flags
  '((:current-patch-set . "--current-patch-set")
    (:patch-sets . "--patch-sets")
    (:approvals . "--all-approvals")
    (:files . "--files")
    (:comments . "--comments")
    (:commit-message . "--commit-message")
    (:dependencies . "--dependencies")
    (:submit-records . "--submit-records")
    (:reviewers . "--all-reviewers"))
  "Options of \"gerrit query\" for the keywords of `magit-gerrit-query-options'.")

(defun magit-gerrit-ssh--query-args (query options)
  "Return the arguments of \"gerrit query\" for QUERY and OPTIONS.
OPTIONS are keywords of `magit-gerrit-query-options'."
  (list "--format=JSON"
        (seq-keep (lambda (option) (alist-get option magit-gerrit-ssh--query-flags))
                  options)
        query))

(defun magit-gerrit-ssh--parse-json-lines (output)
  "Parse OUTPUT of a Gerrit query in JSON format.
Return the list of changes, as alists.  Signal a `user-error' when
Gerrit reports an error."
  (let (changes)
    (dolist (line (split-string output "\n" t "[ \t\r]+"))
      (let ((obj (magit-gerrit--json-parse line)))
        (pcase (alist-get 'type obj)
          ("stats" nil)
          ("error" (user-error "Gerrit query failed: %s" (alist-get 'message obj)))
          (_ (push obj changes)))))
    (nreverse changes)))

(cl-defmethod magit-gerrit-backend-name ((_backend magit-gerrit-ssh-backend))
  "Return the name of the SSH backend."
  "SSH")

(cl-defmethod magit-gerrit-backend-key ((backend magit-gerrit-ssh-backend))
  "Return a string identifying the server of the SSH BACKEND."
  (format "ssh:%s:%s" (magit-gerrit-ssh-backend-creds backend)
          (magit-gerrit-ssh-backend-port backend)))

(cl-defmethod magit-gerrit-backend-features ((_backend magit-gerrit-ssh-backend))
  "Return the features of the SSH backend."
  '(query async vote submit abandon restore rebase move topic reviewers cc wip comment))

(cl-defmethod magit-gerrit-backend-matches-remote-p ((backend magit-gerrit-ssh-backend) remote-url)
  "Return non-nil when the SSH BACKEND is the server of REMOTE-URL."
  (and remote-url (string-search (magit-gerrit-ssh-backend-creds backend) remote-url)))

(cl-defmethod magit-gerrit-backend-query ((backend magit-gerrit-ssh-backend) query options)
  "Run QUERY with OPTIONS on the SSH BACKEND, see `magit-gerrit-backend-query'."
  (magit-gerrit-ssh--parse-json-lines
   (magit-gerrit-ssh--run backend nil "query" (magit-gerrit-ssh--query-args query options))))

(cl-defmethod magit-gerrit-backend-query-async ((backend magit-gerrit-ssh-backend) query options callback)
  "Run QUERY with OPTIONS on the SSH BACKEND in the background.
See `magit-gerrit-backend-query-async' for CALLBACK."
  (let* ((ssh-args (magit-gerrit-ssh--command backend "query"
                                              (magit-gerrit-ssh--query-args query options)))
         (stdout (generate-new-buffer " *magit-gerrit-async*"))
         (stderr (generate-new-buffer " *magit-gerrit-async-err*"))
         (default-directory (if (file-remote-p default-directory)
                                temporary-file-directory
                              default-directory)))
    (make-process
     :name "magit-gerrit-reviews"
     :buffer stdout
     :stderr stderr
     :command (cons magit-gerrit-ssh-program ssh-args)
     :connection-type 'pipe
     :coding 'utf-8
     :noquery t
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let* ((exit-code (process-exit-status process))
                (err (with-current-buffer stderr (string-trim (buffer-string))))
                (output (with-current-buffer stdout (buffer-string))))
           (magit-gerrit-ssh--log ssh-args exit-code err)
           (kill-buffer stdout)
           (kill-buffer stderr)
           (if (eq exit-code 0)
               (let ((changes (condition-case e
                                  (cons t (magit-gerrit-ssh--parse-json-lines output))
                                  (t (cons nil (error-message-string e))))))
                 (if (car changes)
                     (funcall callback (cdr changes) nil)
                   (funcall callback nil (cdr changes))))
             (funcall callback nil (magit-gerrit-ssh--failure-message "query" exit-code err)))))))))

(cl-defmethod magit-gerrit-backend-web-url ((backend magit-gerrit-ssh-backend))
  "Deduce the web URL of the SSH BACKEND from the URL of a change."
  (when-let* ((project (magit-gerrit-get-project))
              (change (car (ignore-errors
                             (magit-gerrit-backend-query
                              backend (list (concat "project:" project) "limit:1") nil))))
              (url (magit-gerrit-change-url change))
              ((string-match "\\`\\(.*?\\)\\(?:/c/.*\\|/[0-9]+/?\\)\\'" url)))
    (match-string 1 url)))


;;; Actions

(cl-defmethod magit-gerrit-backend-vote ((backend magit-gerrit-ssh-backend) change votes options)
  "Vote VOTES on CHANGE with the SSH BACKEND, see `magit-gerrit-backend-vote'.
OPTIONS is a plist."
  (magit-gerrit-ssh--review backend change
                            (mapcar (lambda (vote) (list "--label" vote)) votes)
                            (magit-gerrit-ssh--message-args options)
                            (when (plist-get options :submit) "--submit")))

(cl-defmethod magit-gerrit-backend-submit ((backend magit-gerrit-ssh-backend) change options)
  "Submit CHANGE with the SSH BACKEND, OPTIONS is a plist."
  (magit-gerrit-ssh--review backend change "--submit" (magit-gerrit-ssh--message-args options)))

(cl-defmethod magit-gerrit-backend-abandon ((backend magit-gerrit-ssh-backend) change options)
  "Abandon CHANGE with the SSH BACKEND, OPTIONS is a plist."
  (magit-gerrit-ssh--review backend change "--abandon" (magit-gerrit-ssh--message-args options)))

(cl-defmethod magit-gerrit-backend-restore ((backend magit-gerrit-ssh-backend) change options)
  "Restore CHANGE with the SSH BACKEND, OPTIONS is a plist."
  (magit-gerrit-ssh--review backend change "--restore" (magit-gerrit-ssh--message-args options)))

(cl-defmethod magit-gerrit-backend-rebase ((backend magit-gerrit-ssh-backend) change _options)
  "Rebase CHANGE with the SSH BACKEND."
  (magit-gerrit-ssh--review backend change "--rebase"))

(cl-defmethod magit-gerrit-backend-move ((backend magit-gerrit-ssh-backend) change branch options)
  "Move CHANGE to BRANCH with the SSH BACKEND, OPTIONS is a plist."
  (magit-gerrit-ssh--review backend change "--move" branch (magit-gerrit-ssh--message-args options)))

(cl-defmethod magit-gerrit-backend-set-topic ((backend magit-gerrit-ssh-backend) change topic)
  "Set the TOPIC of CHANGE with the SSH BACKEND (nil to remove it)."
  (magit-gerrit-ssh--run
   backend nil "set-topic"
   (list (format "%s~%s~%s" (magit-gerrit-change-project change)
                 (magit-gerrit-change-branch change) (magit-gerrit-change-id change))
         (when topic (list "--topic" topic)))))

(cl-defmethod magit-gerrit-backend-add-reviewers ((backend magit-gerrit-ssh-backend) change accounts
                                                  &optional cc)
  "Add ACCOUNTS as reviewers (or in CC when CC is non-nil) of CHANGE.
BACKEND is the SSH backend."
  (if cc
      ;; "gerrit set-reviewers" can't add CCs
      (magit-gerrit-ssh--review-json backend change (magit-gerrit--reviewers-input accounts "CC"))
    (magit-gerrit-ssh--run backend nil "set-reviewers"
                           (list "--project" (magit-gerrit-change-project change)
                                 (mapcar (lambda (account) (list "--add" account)) accounts)
                                 (magit-gerrit-change-id change)))))

(cl-defmethod magit-gerrit-backend-remove-reviewers ((backend magit-gerrit-ssh-backend) change accounts)
  "Remove the reviewers ACCOUNTS of CHANGE with the SSH BACKEND."
  (magit-gerrit-ssh--run backend nil "set-reviewers"
                         (list "--project" (magit-gerrit-change-project change)
                               (mapcar (lambda (account) (list "--remove" account)) accounts)
                               (magit-gerrit-change-id change))))

(cl-defmethod magit-gerrit-backend-set-wip ((backend magit-gerrit-ssh-backend) change wip options)
  "Set the WIP state of CHANGE with the SSH BACKEND, OPTIONS is a plist."
  (magit-gerrit-ssh--review-json
   backend change
   `(,(if wip '(work_in_progress . t) '(ready . t))
     ,@(when-let* ((msg (plist-get options :message))) `((message . ,msg))))))

(cl-defmethod magit-gerrit-backend-post-comments ((backend magit-gerrit-ssh-backend) change revision
                                                  comments options)
  "Post COMMENTS on REVISION of CHANGE with the SSH BACKEND.
See `magit-gerrit-backend-post-comments' for OPTIONS.  Draft
comments and replies are not supported."
  (when (plist-get options :draft)
    (magit-gerrit--unsupported backend 'drafts))
  (when (seq-some (lambda (comment) (plist-get comment :in-reply-to)) comments)
    (magit-gerrit--unsupported backend 'reply))
  (magit-gerrit-ssh--review-json
   backend change
   `((comments . ,(magit-gerrit--comments-input comments))
     ,@(when-let* ((msg (plist-get options :message))) `((message . ,msg)))
     ,@(when-let* ((notify (plist-get options :notify))) `((notify . ,notify))))
   revision))


;;; Commit-msg hook

(defun magit-gerrit-ssh--download-hook (file)
  "Download the commit-msg hook from the SSH daemon to FILE.
Return non-nil on success."
  (when-let* ((backend (ignore-errors (magit-gerrit-ssh--variables-backend))))
    (let ((tmp (make-temp-file "magit-gerrit-hook-"))
          (default-directory temporary-file-directory))
      (unwind-protect
          (when (seq-some
                 (lambda (legacy)
                   (let* ((args (append (when legacy '("-O"))
                                        (list "-P" (number-to-string (magit-gerrit-ssh-backend-port backend))
                                              (concat (magit-gerrit-ssh-backend-creds backend)
                                                      ":hooks/commit-msg")
                                              tmp)))
                          (exit-code (apply #'call-process magit-gerrit-scp-program nil nil nil args)))
                     (magit-gerrit--log (string-join (cons magit-gerrit-scp-program args) " ")
                                        exit-code)
                     (eq exit-code 0)))
                 ;; "-O" (legacy scp protocol) is needed since OpenSSH 9, but older
                 ;; versions don't know it
                 '(t nil))
            (copy-file tmp file t)
            t)
        (delete-file tmp)))))

(setf (alist-get 'scp magit-gerrit-commit-msg-hook-downloaders) #'magit-gerrit-ssh--download-hook)

(provide 'magit-gerrit-ssh)

;;; magit-gerrit-ssh.el ends here
