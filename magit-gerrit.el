;;; magit-gerrit.el --- Magit plugin for Gerrit Code Review  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Brian Fransioli
;; Copyright (C) 2025-2026 Abdelhak Bougouffa

;; Author: Brian Fransioli <assem@terranpro.org>
;; Author: Abdelhak Bougouffa
;; URL: https://github.com/terranpro/magit-gerrit
;; Package-Requires: ((emacs "29.1") (magit "3.3.0") (transient "0.3.0"))
;; Package-Version: 0.1.0

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

;; Magit plugin to make Gerrit code review easy-to-use from Emacs and
;; without the need for a browser!
;;
;; Currently uses the [deprecated] gerrit ssh interface, which has
;; meant that obtaining the list of reviewers is not possible, only
;; the list of approvals (those who have already verified and/or code
;; reviewed).


;;; To Use:

;; (require 'magit-gerrit)
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")

;; M-x `magit-status'
;; h R <= magit-gerrit uses the R prefix, see help


;;; Workflow:

;; 1) *check out branch => changes => (ma)git commit*
;; 2) R P  <= [ger*R*it *P*ush for review]
;; 3) R A  <= [ger*R*it *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) R S  <= [ger*R*it *S*ubmit review]


;;; Other Comments:

;; `magit-gerrit-ssh-creds' is buffer local, so if you work with
;; multiple Gerrit's, you can make this a file or directory local
;; variable for one particular project.

;; If your git remote for gerrit is not the default "origin", then
;; `magit-gerrit-remote' should be adjusted accordingly (e.g. "gerrit")

;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing 'R A' for every review.

;; `magit-gerrit' will be enabled automatically on `magit-status' if
;; the git remote repo uses the same creds found in
;; `magit-gerrit-ssh-creds'.

;; Ex:  magit-gerrit-ssh-creds == br.fransioli@gerrit.org
;; $ cd ~/elisp; git remote -v => https://github.com/terranpro/magit-gerrit.git
;; ^~~ `magit-gerrit-mode' would *NOT* be enabled here

;; $ cd ~/gerrit/prja; git remote -v => ssh://br.fransioli@gerrit.org/.../prja
;; ^~~ `magit-gerrit-mode' *WOULD* be enabled here


;;; Code:

(require 'magit)
(require 'transient)
(require 'json)
(require 'mule-util)
(require 'project)
(require 'savehist)
(eval-when-compile (require 'cl-lib))


;;; Customization

(defgroup magit-gerrit nil
  "Gerrit Code Review support for Magit."
  :group 'magit-extensions
  :prefix "magit-gerrit-")

(defcustom magit-gerrit-use-push-url nil
  "If non-nil, use the push URL of the remote for Gerrit connection.
Otherwise, the fetch URL is used (see \"git remote get-url\")."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-ssh-program "ssh"
  "The SSH program used to run Gerrit commands."
  :group 'magit-gerrit
  :type 'string)

(defcustom magit-gerrit-gitiles-base-url nil
  "The base URL for Gitiles.
When nil, it is deduced from the remote URL as
\"https://HOST/plugins/gitiles\"."
  :group 'magit-gerrit
  :type '(choice (const :tag "Deduce from remote" nil) string))

(defconst magit-gerrit-default-review-labels
  (list (list "Code-Review" "CR") (list "Verified" "Ve"))
  "Default review labels, as a list of (LABEL ABBREVIATION).")

(defcustom magit-gerrit-review-labels magit-gerrit-default-review-labels
  "List of review labels to show, including user defined ones.
Each element is a list (LABEL ABBREVIATION), where LABEL is the
label name as displayed in the Gerrit UI and ABBREVIATION is a
2 characters abbreviation used in the header of the reviews section."
  :group 'magit-gerrit
  :type '(repeat (list (string :tag "Label")
                       (string :tag "Abbreviation"))))

(defcustom magit-gerrit-popup-prefix "_"
  "Key used to open the `magit-gerrit-dispatch' transient."
  :group 'magit-gerrit
  :type 'key-sequence)

(defcustom magit-gerrit-signed-push-p nil
  "Whether or not to push with the --signed option."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-extra-options nil
  "Extra query terms added when fetching reviews (e.g. \"is:watched\").
This can be a list of terms, or a string of space-separated terms."
  :group 'magit-gerrit
  :type '(choice (repeat string) string))

(defcustom magit-gerrit-collapse-patchset-section nil
  "Collapse/hide gerrit section containing patchsets by default.
Collapse/hide the section body containing the list of gerrit
changesets by default when t.  This only has an effect when
creating the section for the first time.  When the section is
recreated during a refresh, then the visibility of predecessor is
inherited and this setting is ignored.  See also the HIDE
parameter of `magit-insert-section'."
  :group 'magit-gerrit
  :type 'boolean)


;;; Per-repository variables

(defvar-local magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server.")

(defvar-local magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\").")

(defvar magit-gerrit-fallback-remote "gerrit"
  "Fallback remote name to use for gerrit.")

(defvar-local magit-gerrit-force-enable nil
  "Force enabling `magit-gerrit' for the project.")

(defvar-local magit-gerrit-port 29418
  "The default port to use with the Gerrit host.")

;; https://gerrit-review.googlesource.com/Documentation/user-search.html
;; For example we can set it to: "(status:open OR (status:merged AND -age:6months))"
(defvar-local magit-gerrit-query-filters nil
  "Query used to list the reviews, defaults to \"status:open\" when nil.
See https://gerrit-review.googlesource.com/Documentation/user-search.html")

(put 'magit-gerrit-ssh-creds 'safe-local-variable #'stringp)
(put 'magit-gerrit-remote 'safe-local-variable #'stringp)
(put 'magit-gerrit-port 'safe-local-variable #'integerp)
(put 'magit-gerrit-force-enable 'safe-local-variable #'booleanp)
(put 'magit-gerrit-query-filters 'safe-local-variable #'stringp)


;;; Internal variables

(defvar magit-gerrit-query-filters-history nil
  "Minibuffer history of `magit-gerrit-edit-filters'.")
(add-to-list 'savehist-additional-variables 'magit-gerrit-query-filters-history)

(defvar magit-gerrit--rev-change-id-cache (make-hash-table :test #'equal)
  "Cache mapping revisions to their Change-Id.")

(defvar magit-gerrit--change-id-cache (make-hash-table :test #'equal)
  "Cache mapping Change-Ids to Gerrit query results.")

(defvar magit-gerrit--origin-action nil
  "Command bound to `magit-gerrit-popup-prefix' before enabling the mode.")

(defvar magit-gerrit--dispatch-is-added nil
  "Non-nil when the Gerrit entry has been added to `magit-dispatch'.")

(defconst magit-gerrit-log-buffer-name "*magit-gerrit-log*"
  "Name of the buffer logging the Gerrit SSH commands.")

(defconst magit-gerrit-push-options
  '((wip) (ready) (private) (remove-private) (publish-comments)
    (hashtag . t) (topic . t) (message . t) (notify . (NONE OWNER OWNER_REVIEWERS ALL)))
  "Gerrit push options, as an alist of (OPTION . VALUES).
VALUES is nil for flags, t for free text values or a list of
possible values.")


;;; Gerrit SSH commands

(defun magit-gerrit--quote (arg)
  "Quote ARG for the command line parser of Gerrit's SSH daemon.
Gerrit splits the command line itself (there is no remote shell), it
supports POSIX-like single quotes, double quotes and backslashes."
  (if (string-match-p "\\`[-[:alnum:]_.,:/=@+%^~]+\\'" arg)
      arg
    (concat "'" (string-replace "'" "'\\''" arg) "'")))

(defun magit-gerrit--command (cmd &rest args)
  "Build SSH parameters to run Gerrit's CMD with ARGS.
ARGS are flattened, nil elements are removed and the remaining ones
are quoted for Gerrit."
  `("-x" "-p"
    ,(number-to-string magit-gerrit-port)
    ,(or magit-gerrit-ssh-creds
         (user-error "`magit-gerrit-ssh-creds' must be set!"))
    "gerrit"
    ,cmd
    ,@(mapcar (lambda (arg) (magit-gerrit--quote (format "%s" arg)))
              (delq nil (flatten-list args)))))

(defun magit-gerrit--log (ssh-args exit-code stderr)
  "Log the SSH call with SSH-ARGS, its EXIT-CODE and STDERR."
  (with-current-buffer (get-buffer-create magit-gerrit-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] %s %s => %s\n"
                      (format-time-string "%F %T")
                      magit-gerrit-ssh-program
                      (string-join ssh-args " ")
                      exit-code))
      (unless (string-empty-p stderr)
        (insert stderr "\n")))))

(defun magit-gerrit--ssh-call (cmd &rest args)
  "Call Gerrit's command CMD with ARGS via SSH and return its output.
ARGS are processed as in `magit-gerrit--command'.  Signal a
`user-error' containing the error output when the command fails.
All the calls are logged in `magit-gerrit-log-buffer-name'."
  (let ((ssh-args (apply #'magit-gerrit--command cmd args))
        (err-file (make-temp-file "magit-gerrit-"))
        ;; SSH is always run locally, even for remote repositories
        (default-directory (if (file-remote-p default-directory)
                               temporary-file-directory
                             default-directory)))
    (unwind-protect
        (with-temp-buffer
          (let* ((exit-code (apply #'call-process magit-gerrit-ssh-program
                                   nil (list t err-file) nil ssh-args))
                 (stderr (with-temp-buffer
                           (insert-file-contents err-file)
                           (string-trim (buffer-string)))))
            (magit-gerrit--log ssh-args exit-code stderr)
            (unless (eq exit-code 0)
              (user-error "Gerrit command \"%s\" failed: %s" cmd
                          (if (string-empty-p stderr)
                              (format "exit code %s" exit-code)
                            (car (split-string stderr "\n" t)))))
            (buffer-string)))
      (delete-file err-file))))

(defun magit-gerrit--parse-json-lines (output)
  "Parse OUTPUT of a Gerrit query in JSON format.
Return the list of changes, as alists.  Signal a `user-error' when
Gerrit reports an error."
  (let (changes)
    (dolist (line (split-string output "\n" t "[ \t\r]+"))
      (let ((obj (json-parse-string line :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil)))
        (pcase (alist-get 'type obj)
          ("stats" nil)
          ("error" (user-error "Gerrit query failed: %s" (alist-get 'message obj)))
          (_ (push obj changes)))))
    (nreverse changes)))

(defun magit-gerrit--query-changes (query &rest options)
  "Run the Gerrit QUERY with OPTIONS, return the matching change list.
QUERY is a string or a list of query terms, OPTIONS are additional
options for \"gerrit query\" such as \"--current-patch-set\"."
  (magit-gerrit--parse-json-lines
   (magit-gerrit--ssh-call "query" "--format=JSON" options query)))

(defun magit-gerrit--query-terms (&optional prj)
  "Return the query terms to list the reviews of project PRJ.
Uses `magit-gerrit-query-filters' or \"status:open\"."
  (when-let* ((prj (or prj (magit-gerrit-get-project))))
    (append
     (list (concat "project:" prj))
     (if (stringp magit-gerrit-extra-options)
         (split-string magit-gerrit-extra-options)
       magit-gerrit-extra-options)
     (list (or magit-gerrit-query-filters "status:open")))))

(defun magit-gerrit--fetch-reviews ()
  "Fetch the reviews of the current project."
  (when-let* ((terms (magit-gerrit--query-terms)))
    (magit-gerrit--query-changes terms "--current-patch-set")))

(defun magit-gerrit--message-args (args)
  "Return the \"--message\" arguments from the transient ARGS."
  (when-let* ((msg (transient-arg-value "--message=" args)))
    (list "--message" msg)))

(defun magit-gerrit--review (change &rest args)
  "Run \"gerrit review\" on the current patchset of CHANGE with ARGS."
  (magit-gerrit--ssh-call "review"
                          "--project" (magit-gerrit-change-project change)
                          args
                          (magit-gerrit-change-revision change)))


;;; Change data model

;; Changes are the objects returned by "gerrit query --format=JSON", parsed
;; as alists (see `magit-gerrit--parse-json-lines').  Their structure is
;; documented in https://gerrit-review.googlesource.com/Documentation/json.html

(defun magit-gerrit--get (obj &rest keys)
  "Return the value at the path KEYS in the nested alist OBJ."
  (dolist (key keys obj)
    (setq obj (alist-get key obj))))

(defun magit-gerrit-change-number (change)
  "Return the number of CHANGE."
  (magit-gerrit--get change 'number))

(defun magit-gerrit-change-id (change)
  "Return the Change-Id of CHANGE."
  (magit-gerrit--get change 'id))

(defun magit-gerrit-change-project (change)
  "Return the project of CHANGE."
  (magit-gerrit--get change 'project))

(defun magit-gerrit-change-branch (change)
  "Return the target branch of CHANGE."
  (magit-gerrit--get change 'branch))

(defun magit-gerrit-change-topic (change)
  "Return the topic of CHANGE, if any."
  (magit-gerrit--get change 'topic))

(defun magit-gerrit-change-subject (change)
  "Return the subject of CHANGE."
  (magit-gerrit--get change 'subject))

(defun magit-gerrit-change-commit-message (change)
  "Return the commit message of the current patchset of CHANGE."
  (magit-gerrit--get change 'commitMessage))

(defun magit-gerrit-change-url (change)
  "Return the URL of CHANGE."
  (magit-gerrit--get change 'url))

(defun magit-gerrit-change-status (change)
  "Return the status of CHANGE (\"NEW\", \"MERGED\" or \"ABANDONED\")."
  (magit-gerrit--get change 'status))

(defun magit-gerrit-change-last-updated (change)
  "Return the last update time of CHANGE, in seconds since the epoch."
  (magit-gerrit--get change 'lastUpdated))

(defun magit-gerrit-change-owner-name (change)
  "Return the name of the owner of CHANGE."
  (magit-gerrit--get change 'owner 'name))

(defun magit-gerrit-change-owner-username (change)
  "Return the username of the owner of CHANGE."
  (magit-gerrit--get change 'owner 'username))

(defun magit-gerrit-change-wip-p (change)
  "Return non-nil when CHANGE is a work in progress (or a legacy draft)."
  (or (eq (magit-gerrit--get change 'wip) t)
      (eq (magit-gerrit--get change 'currentPatchSet 'isDraft) t)))

(defun magit-gerrit-change-patchset (change)
  "Return the current patchset of CHANGE."
  (magit-gerrit--get change 'currentPatchSet))

(defun magit-gerrit-change-patchset-number (change)
  "Return the number of the current patchset of CHANGE."
  (magit-gerrit--get change 'currentPatchSet 'number))

(defun magit-gerrit-change-revision (change)
  "Return the commit hash of the current patchset of CHANGE."
  (magit-gerrit--get change 'currentPatchSet 'revision))

(defun magit-gerrit-change-ref (change)
  "Return the ref of the current patchset of CHANGE."
  (magit-gerrit--get change 'currentPatchSet 'ref))

(defun magit-gerrit-change-approvals (change)
  "Return the approvals of the current patchset of CHANGE."
  (magit-gerrit--get change 'currentPatchSet 'approvals))

(defun magit-gerrit-change-insertions (change)
  "Return the number of inserted lines in the current patchset of CHANGE."
  (or (magit-gerrit--get change 'currentPatchSet 'sizeInsertions) 0))

(defun magit-gerrit-change-deletions (change)
  "Return the number of deleted lines in the current patchset of CHANGE.
Gerrit reports deletions as a negative number, this returns its
absolute value."
  (abs (or (magit-gerrit--get change 'currentPatchSet 'sizeDeletions) 0)))

(defun magit-gerrit-change-at-point ()
  "Return the Gerrit change at point, or nil."
  (magit-section-value-if 'gerrit-change))

(defun magit-gerrit--change-at-point-or-error ()
  "Return the Gerrit change at point, signal an error if there is none."
  (or (magit-gerrit-change-at-point)
      (user-error "No Gerrit change at point")))

(define-obsolete-function-alias 'magit-gerrit-review-at-point
  #'magit-gerrit-change-at-point "0.2.0")


;;; Remote and project

(defun magit-gerrit-get-remote-url ()
  "Return the URL of the Gerrit remote."
  (when-let* ((remote (cl-find-if (lambda (r) (member r (list magit-gerrit-remote magit-gerrit-fallback-remote)))
                                  (magit-git-lines "remote"))))
    (setq-local magit-gerrit-remote remote)
    (magit-git-string
     (if magit-gerrit-use-push-url
         '("remote" "get-url" "--push")
       '("ls-remote" "--get-url"))
     remote)))

(defun magit-gerrit-get-project ()
  "Get the Gerrit project name for the current repo."
  (when-let* ((url (magit-gerrit-get-remote-url))
              (url (url-generic-parse-url url))
              (prj (url-filename url)))
    (string-trim-right (string-trim-left prj "/") "\\.git")))

(defun magit-gerrit-detect-ssh-creds (remote-url)
  "Derive `magit-gerrit-ssh-creds' from REMOTE-URL.
Assumes REMOTE-URL is a Gerrit repo if scheme is SSH and port is
`magit-gerrit-port' or when `magit-gerrit-force-enable' is non-nil."
  (prog1 (or magit-gerrit-ssh-creds
             (when-let* ((url (url-generic-parse-url remote-url))
                         (user (url-user url))
                         (host (url-host url))
                         ((and (string= "ssh" (url-type url))
                               (or magit-gerrit-force-enable (eq magit-gerrit-port (url-port url))))))
               (setq-local magit-gerrit-ssh-creds (format "%s@%s" user host))))
    (let ((inhibit-message t))
      (message "Detected magit-gerrit-ssh-creds=%s" magit-gerrit-ssh-creds))))


;;; Change-Id

(defun magit-gerrit-commit-change-id (&optional rev)
  "Return Change-Id for REV or the commit at point."
  (when-let* ((rev (or rev (magit-commit-at-point)))
              (rev (substring rev 0 7)))
    (if-let* ((id (gethash rev magit-gerrit--rev-change-id-cache)))
        id
      (when-let* ((id (magit-git-string "show" "--format=%(trailers:key=Change-Id,valueonly)" rev)))
        (puthash rev id magit-gerrit--rev-change-id-cache)
        id))))

(defun magit-gerrit--get-change-id-info (id &optional invalidate-cache)
  "Return information about Change-Id ID.

When INVALIDATE-CACHE is non-nil, don't used cached values."
  (if-let* ((cached (and (not invalidate-cache) (gethash id magit-gerrit--change-id-cache))))
      cached
    (puthash id
             (magit-gerrit--query-changes
              (list (concat "project:" (magit-gerrit-get-project))
                    (concat "change:" id))
              "--current-patch-set")
             magit-gerrit--change-id-cache)))


;;; Gitiles

(defun magit-gerrit--get-gitiles-link (&optional line-number)
  "Get Gitiles link, optionally with LINE-NUMBER."
  (when-let* ((proj (magit-gerrit-get-project))
              (base-url (or magit-gerrit-gitiles-base-url
                            (when-let* ((push-url (magit-gerrit-get-remote-url))
                                        (url (url-generic-parse-url push-url)))
                              (concat "https://" (url-host url) "/plugins/gitiles"))))
              (gerrit-proj (magit-gerrit-get-project))
              (branch "master")
              (proj (project-current))
              (file-name (buffer-file-name))
              (file-name (file-relative-name file-name (project-root proj))))
    (concat base-url "/" gerrit-proj "/+/refs/heads/" branch "/" file-name (when line-number (format "#%d" line-number)))))


;;; Formatting

(defun magit-gerrit-string-real-length (s)
  "Return the display length of string S."
  (if (multibyte-string-p s)
      (let* ((mm (- (string-bytes s) (length s)))
             (n (- (length s) (/ mm 2))))
        (+ mm n))
    (length s)))

(defun magit-gerrit-format-short-timestring (seconds)
  "Format SECONDS as a short human readable duration."
  (let* ((years  (/ seconds 32140800))
         (months (/ seconds 2678400))
         (days (/ seconds 86400))
         (hours (/ seconds 3600))
         (minutes (/ seconds 60)))
    (cond
     ((> years 1) (format "%2d %s" years (if (> years 2) "years" "year")))
     ((> months 1) (format "%2d %s" months (if (> months 2) "months" "month")))
     ((> days 1) (format "%2d %s" days (if (> days 2) "days" "day")))
     ((> hours 1) (format "%2d %s" hours (if (> hours 2) "hours" "hour")))
     ((> minutes 1) (format "%2d %s" minutes (if (> minutes 2) "minutes" "minute")))
     (t "just now"))))

(defun magit-gerrit-create-review-labels ()
  "Create review labels heading."
  (let* ((review-labels ""))
    (dolist (label-tuple magit-gerrit-review-labels review-labels)
      (setq review-labels (concat review-labels (car (cdr label-tuple)) " ")))
    (string-trim-right review-labels)))

(defun magit-gerrit-pretty-print-review-header ()
  "Return the header line of the reviews section."
  (let* ((wid (window-width))
         ;; number
         (numstr (format "%-13s" "Patchset"))
         ;; branch info
         (branch (truncate-string-to-width "Branch" 20 nil ?\s))
         ;; sizeinfo
         (sizeinfo (truncate-string-to-width (format "     %s" "Delta") 15 nil ?\s))
         ;; owner
         (author (truncate-string-to-width (format "%s" "Owner") 10 nil ?\s))
         ;; lastupdate
         (lastupdate (truncate-string-to-width (format "%s" "Updated") 12 nil ?\s))
         ;; approvals
         (approvals-info (magit-gerrit-create-review-labels))
         ;; subject
         (subjstr (truncate-string-to-width
                   (format "%s" "Subject")
                   (- wid (length (concat numstr author
                                          (cond
                                           ((> wid 128) (concat branch sizeinfo lastupdate approvals-info))
                                           ((> wid 108) (concat sizeinfo lastupdate approvals-info))
                                           ((> wid 94)  (concat sizeinfo approvals-info))
                                           ((> wid 80)  (concat approvals-info))
                                           (t ""))))
                      1)
                   nil ?\s))
         (show-str (concat numstr subjstr author
                           (cond
                            ((> wid 128) (concat branch sizeinfo lastupdate approvals-info))
                            ((> wid 108) (concat sizeinfo lastupdate approvals-info))
                            ((> wid 94)  (concat sizeinfo approvals-info))
                            ((> wid 80)  (concat approvals-info))
                            (t "")))))
    (propertize (format "%s\n" show-str) 'font-lock-face 'highlight)))

(defun magit-gerrit-pretty-print-review (num patchsetn subj owner-name br size-i size-d ctime approvals-info &optional draft)
  "Return the line of a review.
NUM is the change number, PATCHSETN the patchset number, SUBJ the
subject, OWNER-NAME the owner, BR the branch, SIZE-I and SIZE-D the
inserted and deleted lines, CTIME the last update time and
APPROVALS-INFO the formatted approvals.  When DRAFT is non-nil, the
subject is dimmed."
  (let* ((wid (window-width)) ; to prevents long line arrow from being shown
         ;; number
         (numstr (propertize (format "%-8s" num) 'font-lock-face 'magit-hash))
         ;; patchset num
         (patchsetstr (propertize (format "%-5s" (format "[%s]" patchsetn))
                                  'font-lock-face 'magit-hash))
         ;; branch info
         (branch (propertize (truncate-string-to-width br 20 nil ?\s)
                             'font-lock-face 'magit-hash))
         ;; sizeinfo
         (sizeinfo (concat (propertize (truncate-string-to-width
                                        (format "%+7s"  (concat "+" (number-to-string size-i)))
                                        7 nil ?\s)
                                       'font-lock-face 'diff-added)
                           " "
                           (propertize (truncate-string-to-width (format "-%s" size-d) 7 nil ?\s)
                                       'font-lock-face 'diff-removed)))
         ;; owner
         (author (propertize (truncate-string-to-width (format "%s" owner-name) 10 nil ?\s)
                             'font-lock-face 'magit-log-author))
         ;; lastupdate
         (lastupdate (truncate-string-to-width
                      (magit-gerrit-format-short-timestring
                       (time-to-seconds (time-since ctime)))
                      12 nil ?\s))
         ;; subject
         (subjstr (propertize
                   (truncate-string-to-width
                    subj
                    (- wid (length (concat numstr patchsetstr
                                           (cond
                                            ((> wid 128) (concat branch sizeinfo lastupdate approvals-info))
                                            ((> wid 108) (concat sizeinfo lastupdate approvals-info))
                                            ((> wid 94)  (concat sizeinfo approvals-info))
                                            ((> wid 80)  (concat approvals-info))
                                            (t ""))))
                       (magit-gerrit-string-real-length author)
                       1)
                    nil ?\s)
                   'font-lock-face
                   (if draft 'magit-dimmed 'magit-filename)))
         (show-str (concat numstr patchsetstr subjstr author
                           (cond
                            ((> wid 128) (concat branch sizeinfo lastupdate approvals-info))
                            ((> wid 108) (concat sizeinfo lastupdate approvals-info))
                            ((> wid 94)  (concat sizeinfo approvals-info))
                            ((> wid 80)  (concat approvals-info))
                            (t "")))))
    (format "%s\n" show-str)))

(defun magit-gerrit-match-review-labels (score type)
  "Match SCORE to correct TYPE."
  (let ((matchlist nil))
    (dolist (labeltuple magit-gerrit-review-labels matchlist)
      (push (and (string= type (car labeltuple)) score) matchlist))
    (nreverse matchlist)))

(defun magit-gerrit-trans-score (type value-list)
  "Format the votes VALUE-LIST of label TYPE as a single character."
  (if (and value-list (length> value-list 0))
      (let ((min-v (apply 'min value-list))
            (max-v (apply 'max value-list))
            (t-max-v 1)
            (t-min-v -1))
        (if (string= "Code-Review" type)
            (setq t-max-v 2 t-min-v -2))
        (cond
         ((<= min-v t-min-v) (propertize "x" 'font-lock-face 'magit-signature-bad))
         ((>= max-v t-max-v) (propertize "✓" 'font-lock-face 'magit-signature-good))
         ((> min-v 0) (propertize (format "+%d" min-v) 'font-lock-face 'magit-signature-good))
         (t (propertize (format "%d" min-v) 'font-lock-face 'magit-signature-bad))))
    " "))

(defun magit-gerrit-wash-approvals-oneline (approvals)
  "Format APPROVALS on one line, one column per review label."
  (let* (type-values)
    (seq-doseq (approval approvals)
      (let ((type (cdr (assq 'type approval)))
            (value (string-to-number (cdr (assq 'value approval)))))
        (push value (alist-get type type-values nil nil #'string-equal))))
    (mapconcat
     (lambda (elem)
       (let* ((long (car elem))
              (score (magit-gerrit-trans-score
                      long (cdr (assoc long type-values)))))
         (format "%2s" score)))
     magit-gerrit-review-labels
     " ")))


;;; Status section

(defun magit-gerrit--insert-review (change)
  "Insert a section for CHANGE (as returned by a Gerrit query)."
  (when (and (magit-gerrit-change-number change)
             (magit-gerrit-change-subject change)
             (magit-gerrit-change-owner-name change))
    (magit-insert-section (gerrit-change change)
      (insert (magit-gerrit-pretty-print-review
               (magit-gerrit-change-number change)
               (magit-gerrit-change-patchset-number change)
               (magit-gerrit-change-subject change)
               (magit-gerrit-change-owner-name change)
               (magit-gerrit-change-branch change)
               (magit-gerrit-change-insertions change)
               (magit-gerrit-change-deletions change)
               (magit-gerrit-change-last-updated change)
               (magit-gerrit-wash-approvals-oneline
                (magit-gerrit-change-approvals change))
               (magit-gerrit-change-wip-p change))))))

(defun magit-gerrit-insert-reviews-section ()
  "Insert the Gerrit reviews section in the status buffer.
Errors are displayed in the section instead of aborting the refresh."
  (when (magit-gerrit-get-project)
    (magit-insert-section (gerrit-reviews nil magit-gerrit-collapse-patchset-section)
      (magit-insert-heading "Reviews:")
      (insert (magit-gerrit-pretty-print-review-header))
      (condition-case err
          (mapc #'magit-gerrit--insert-review (magit-gerrit--fetch-reviews))
        (error
         (insert (propertize (format "%s\n" (error-message-string err))
                             'font-lock-face 'error))))
      (insert "\n"))))


;;; Patchsets

;; Not yet implemented, could be if necessary
(defun magit-gerrit-remote-update (&optional _remote)
  "Not implemented.")

(defun magit-gerrit-create-branch (_branch _parent)
  "Not implemented.")

(defsubst magit-gerrit-process-wait ()
  "Wait for the current Magit process to finish."
  (while (and magit-this-process (eq (process-status magit-this-process) 'run))
    (sleep-for 0.005)))

(defun magit-gerrit-create-branch-force (branch parent)
  "Switch HEAD to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\(\"git checkout -B BRANCH REVISION\")."
  (cond ((run-hook-with-args-until-success
          'magit-create-branch-hook branch parent))
        ((and branch (not (string= branch "")))
         (magit-save-repository-buffers)
         (magit-run-git "checkout" "-B" branch parent))))

(defun magit-gerrit-get-ref (&optional select-number)
  "Return the ref of the review at point.
When SELECT-NUMBER is non-nil, ask for the patchset number."
  (when-let* ((change (magit-gerrit-change-at-point))
              (ref (magit-gerrit-change-ref change))
              (number (magit-gerrit-change-patchset-number change)))
    (if select-number
        (concat (string-join
                 (append (butlast (string-split ref "/"))
                         (list (number-to-string (read-number (format "Select patch number (up to %d): " number)))))
                 "/"))
      ref)))

(defun magit-gerrit-fetch-patchset (&optional ref select-number)
  "Fetch a Gerrit review patchset at REF, optionally SELECT-NUMBER."
  (when-let* ((ref (or ref (magit-gerrit-get-ref select-number))))
    (magit-git-fetch magit-gerrit-remote ref)
    (message (format "Waiting a git fetch from %s to complete..." magit-gerrit-remote))
    (magit-gerrit-process-wait)))


;;; Commands

(defun magit-gerrit-edit-filters (arg)
  "Edit the Gerrit query filters.
Edit globally when called with universal argument ARG."
  (interactive "P")
  (when-let* ((filters (read-string "Query filters: " magit-gerrit-query-filters 'magit-gerrit-query-filters-history))
              ((not (string-empty-p filters))))
    (kill-local-variable 'magit-gerrit-query-filters)
    (if arg
        (setq-default magit-gerrit-query-filters filters)
      (setq-local magit-gerrit-query-filters filters))))

(defun magit-gerrit-change-id-browse-url (&optional id)
  "Go to the Gerrit URL of the change with Change-Id ID."
  (interactive (list (magit-gerrit-commit-change-id)))
  (when-let* ((vals (magit-gerrit--get-change-id-info id))
              (vals (delq nil (mapcar #'magit-gerrit-change-url vals))))
    (browse-url (if (length= vals 1)
                    (car vals)
                  (completing-read "Select URL: " vals)))))

(defun magit-gerrit-gitiles-browse-url (with-line-p)
  "Browse this file in Gitiles, highlight current line when WITH-LINE-P."
  (interactive "P")
  (when-let* ((url (magit-gerrit--get-gitiles-link (when with-line-p (line-number-at-pos (point))))))
    (browse-url url)))

(defun magit-gerrit-view-patchset-diff (&optional select-number)
  "View the diff for a patchset, optionally SELECT-NUMBER."
  (interactive "P")
  (when-let* ((ref (magit-gerrit-get-ref select-number)))
    (let ((dir default-directory))
      (magit-gerrit-fetch-patchset ref)
      (message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
      (magit-diff-range "FETCH_HEAD~1..FETCH_HEAD"))))

(defun magit-gerrit-download-patchset (&optional select-number)
  "Download a Gerrit review patchset, optionally SELECT-NUMBER."
  (interactive "P")
  (when-let* ((change (magit-gerrit-change-at-point))
              (ref (magit-gerrit-get-ref select-number)))
    (let* ((dir default-directory)
           (num (string-join (last (string-split ref "/") 2) "/"))
           (topic (magit-gerrit-change-topic change))
           (branch (format "review/%s/%s%s"
                           (magit-gerrit-change-owner-username change)
                           (if topic (concat topic "-") "")
                           num)))
      (magit-gerrit-fetch-patchset ref)
      (message (format "Checking out refs %s to %s in %s" ref branch dir))
      (magit-gerrit-create-branch-force branch "FETCH_HEAD"))))

(defun magit-gerrit-cherry-pick-patchset (&optional select-number)
  "Cherry-pick a Gerrit review patchset, optionally SELECT-NUMBER."
  (interactive "P")
  (magit-gerrit-fetch-patchset nil select-number)
  (magit--cherry-pick '("FETCH_HEAD") nil))

(defun magit-gerrit-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (if-let* ((change (magit-gerrit-change-at-point)))
      (browse-url (magit-gerrit-change-url change))
    (call-interactively #'magit-visit-thing)))

(defun magit-gerrit-copy-review (with-commit-message)
  "Copy review url and commit message when WITH-COMMIT-MESSAGE is non-nil."
  (when-let* ((change (magit-gerrit-change-at-point)))
    (with-temp-buffer
      (insert
       (concat (when with-commit-message
                 (concat (format "%s: " (magit-gerrit-change-number change))
                         (car (split-string (magit-gerrit-change-commit-message change) "\n" t))
                         " | "))
               (magit-gerrit-change-url change)))
      (message "%s" (buffer-string))
      (clipboard-kill-region (point-min) (point-max)))))

(defun magit-gerrit-copy-review-url ()
  "Copy review's url only."
  (interactive)
  (magit-gerrit-copy-review nil))

(defun magit-gerrit-copy-review-url-commit-message ()
  "Copy review url with commit message."
  (interactive)
  (magit-gerrit-copy-review t))

(defun magit-gerrit-copy-ref (&optional select-number)
  "Copy review's reference hash, ask for patchset when SELECT-NUMBER."
  (interactive "P")
  (if-let* ((ref (magit-gerrit-get-ref select-number)))
      (with-temp-buffer
        (insert ref)
        (clipboard-kill-region (point-min) (point-max))
        (message "Copied %S to clipboard." ref))
    (user-error "No Gerrit revision at point")))

(defun magit-gerrit-add-reviewer ()
  "Add a reviewer to the review at point."
  (interactive)
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--ssh-call
     "set-reviewers"
     "--project" (magit-gerrit-change-project change)
     "--add" (read-string "Reviewer Name/Email: ")
     (magit-gerrit-change-id change))
    (magit-refresh)))

(defun magit-gerrit-arguments ()
  "Return the arguments of the `magit-gerrit-dispatch' transient."
  (transient-args 'magit-gerrit-dispatch))

(defun magit-gerrit--read-score ()
  "Read a review score."
  (completing-read "Score: " '("-2" "-1" "0" "+1" "+2") nil t "+1"))

(defun magit-gerrit-verify-review (args)
  "Verify a Gerrit review, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--verified" (magit-gerrit--read-score)
                        (magit-gerrit--message-args args))
  (magit-refresh))

(defun magit-gerrit-code-review (args)
  "Perform a Gerrit code review, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--code-review" (magit-gerrit--read-score)
                        (magit-gerrit--message-args args))
  (magit-refresh))

(defun magit-gerrit-submit-review (args)
  "Submit a Gerrit code review, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--submit" (magit-gerrit--message-args args))
  (magit-fetch-all-no-prune)
  (magit-refresh))

(defun magit-gerrit-abandon-review (args)
  "Abandon the review at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--abandon" (magit-gerrit--message-args args))
  (magit-refresh))

(defun magit-gerrit-restore-review (args)
  "Restore the abandoned review at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--restore" (magit-gerrit--message-args args))
  (magit-refresh))


;;; Push

(defun magit-gerrit-read-push-option ()
  "Read a Gerrit push option, see `magit-gerrit-push-options'."
  (let* ((option (intern (completing-read "Select an option: " magit-gerrit-push-options nil t)))
         (choice (alist-get option magit-gerrit-push-options))
         (prompt (format "Value for option %S: " option))
         (val
          (cond ((eq choice t) (read-string prompt))
                ((consp choice) (completing-read prompt choice nil t)))))
    (concat "%" (format "%s" option) (when val (format "=%s" val)))))

(defun magit-gerrit-push-review (status &optional option)
  "Push the commit at point to refs/STATUS/<branch> with push OPTION."
  (let* ((branch (magit-get-current-branch))
         (branch-remote (and branch (magit-get "branch" branch "remote")))
         (branch-remote (and (not (or (null branch-remote) (string= branch-remote "."))) branch-remote)))
    (when-let* (((or branch (y-or-n-p "Not on a branch, push a detached head? ")))
                (commitid (or (magit-section-value-if 'commit) (error "Couldn't find a commit at point")))
                (rev (magit-rev-parse (or commitid (error "Select a commit for review"))))
                (branch-merge (if (and branch branch-remote)
                                  (magit-get "branch" branch "merge")
                                (completing-read
                                 "Remote Branch: "
                                 (let ((rbs (magit-list-remote-branch-names)))
                                   (or
                                    (mapcar
                                     (lambda (rb)
                                       (and (string-match "^[^/]+/\\(.+\\)$" rb) (concat "refs/heads/" (match-string 1 rb))))
                                     rbs)
                                    '("refs/heads/master"))))))
                (branch-pub (and (string-match "refs/heads\\(.+\\)" branch-merge)
                                 (format "refs/%s%s" status (match-string 1 branch-merge)))))
      (unless branch-remote (setq branch-remote magit-gerrit-remote))
      (magit-run-git-async "push" "-v" (when magit-gerrit-signed-push-p "--signed") branch-remote (concat rev ":" branch-pub option)))))

(defun magit-gerrit-push-for-review (&optional with-opt)
  "Push the commit at point for review.
When WITH-OPT is non-nil, ask for a push option."
  (interactive "P")
  (magit-gerrit-push-review 'for (when with-opt (magit-gerrit-read-push-option))))

(defun magit-gerrit-push-draft-for-review (&optional with-opt)
  "Push the commit at point as a draft for review.
When WITH-OPT is non-nil, ask for a push option."
  (interactive "P")
  (magit-gerrit-push-review 'drafts (when with-opt (magit-gerrit-read-push-option))))

(define-obsolete-function-alias 'magit-gerrit-create-review 'magit-gerrit-push-for-review "1.0.0")
(define-obsolete-function-alias 'magit-gerrit-create-draft 'magit-gerrit-push-draft-for-review "1.0.0")


;;; Transient

;;;###autoload (autoload 'magit-gerrit-dispatch "magit-gerrit" nil t)
(transient-define-prefix magit-gerrit-dispatch ()
  "Transient menu for `magit-gerrit' commands."
  ["Arguments"
   ("-m" "Message"                         "--message=")]
  [["Actions"
    ("A" "Add reviewer"                    magit-gerrit-add-reviewer)
    ("B" "Abandon review"                  magit-gerrit-abandon-review)
    ("R" "Restore abandoned review"        magit-gerrit-restore-review)
    ("P" "Push commit for review"          magit-gerrit-push-for-review)
    ("S" "Submit review"                   magit-gerrit-submit-review)
    ("W" "Push commit for draft review"    magit-gerrit-push-draft-for-review)]
   ["Review"
    ("b" "Browse review"                   magit-gerrit-browse-review)
    ("C" "Code review"                     magit-gerrit-code-review)
    ("d" "View patchset diff"              magit-gerrit-view-patchset-diff)
    ("D" "Download patchset"               magit-gerrit-download-patchset)
    ("F" "Cherry-pick patchset"            magit-gerrit-cherry-pick-patchset)
    ("V" "Verify"                          magit-gerrit-verify-review)]]
  ["Others"
   ("y" "Copy review URL"                  magit-gerrit-copy-review-url)
   ("Y" "Copy review URL and message"      magit-gerrit-copy-review-url-commit-message)
   ("r" "Copy review reference (hash)"     magit-gerrit-copy-ref)])


;;; Minor mode

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map magit-gerrit-popup-prefix 'magit-gerrit-dispatch)
    map)
  "Keymap of `magit-gerrit-mode'.")

(define-minor-mode magit-gerrit-mode
  "Gerrit support for Magit."
  :lighter " Gerrit" :require 'magit-topgit :keymap magit-gerrit-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with magit"))
  (if-let* ((url (magit-gerrit-get-remote-url)))
      (unless (magit-gerrit-detect-ssh-creds url)
        (user-error "You can set `magit-gerrit-ssh-creds' to enable magit-gerrit-mode"))
    (user-error "You can set `magit-gerrit-remote' to a valid Gerrit remote"))
  (if magit-gerrit-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-gerrit-insert-reviews-section
                                'magit-insert-stashes t t)
        (add-hook 'magit-create-branch-command-hook 'magit-gerrit-create-branch nil t)
        (add-hook 'magit-remote-update-command-hook 'magit-gerrit-remote-update nil t)
        (add-hook 'magit-push-command-hook 'magit-gerrit-push nil t))
    (remove-hook 'magit-after-insert-stashes-hook 'magit-gerrit-insert-reviews-section t)
    (remove-hook 'magit-create-branch-command-hook 'magit-gerrit-create-branch t)
    (remove-hook 'magit-remote-update-command-hook 'magit-gerrit-remote-update t)
    (remove-hook 'magit-push-command-hook 'magit-gerrit-push t))
  (when (not magit-gerrit--origin-action)
    (setq magit-gerrit--origin-action (lookup-key magit-mode-map magit-gerrit-popup-prefix))
    (if magit-gerrit-mode
        (progn
          ;; Update keymap with prefix incase it has changed
          (define-key magit-mode-map magit-gerrit-popup-prefix 'magit-gerrit-dispatch)
          (define-key magit-mode-map [remap magit-visit-thing] 'magit-gerrit-browse-review)
          ;; Attach Magit Gerrit to Magit's default help popup
          (when (not magit-gerrit--dispatch-is-added)
            (transient-append-suffix 'magit-dispatch "z"
              `(,magit-gerrit-popup-prefix "Gerrit" magit-gerrit-dispatch)))
          (setq magit-gerrit--dispatch-is-added t))
      (define-key magit-mode-map magit-gerrit-popup-prefix magit-gerrit--origin-action)
      ;; Dettach Magit Gerrit to Magit's default help popup
      (setq magit-gerrit--dispatch-is-added nil)
      (transient-remove-suffix 'magit-dispatch magit-gerrit-popup-prefix)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-gerrit-check-enable ()
  "Enable `magit-gerrit-mode' when the repository is hosted on Gerrit."
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
               (or magit-gerrit-force-enable
                   (and (magit-gerrit-detect-ssh-creds remote-url)
                        (string-match magit-gerrit-ssh-creds remote-url))))
      (magit-gerrit-mode 1))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;;;###autoload
(defun magit-gerrit-auto-enable ()
  "Try to auto enable magit-gerrit in the `magit-status' buffer."
  (interactive)
  (add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
  (add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t))

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
