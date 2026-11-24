;;; magit-gerrit.el --- Magit plugin for Gerrit Code Review  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Brian Fransioli
;; Copyright (C) 2025-2026 Abdelhak Bougouffa

;; Author: Brian Fransioli <assem@terranpro.org>
;; Author: Abdelhak Bougouffa
;; URL: https://github.com/terranpro/magit-gerrit
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (transient "0.5.0"))
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
;; _ <= magit-gerrit uses the _ prefix (see `magit-gerrit-popup-prefix')


;;; Workflow:

;; 1) *check out branch => changes => (ma)git commit*
;; 2) _ P  <= [*P*ush for review]
;; 3) _ A  <= [*A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) _ S  <= [*S*ubmit review]


;;; Other Comments:

;; `magit-gerrit-ssh-creds' is buffer local, so if you work with
;; multiple Gerrit's, you can make this a file or directory local
;; variable for one particular project.

;; If your git remote for gerrit is not the default "origin", then
;; `magit-gerrit-remote' should be adjusted accordingly (e.g. "gerrit")

;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing '_ A' for every review.

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
(require 'savehist)
(require 'cl-lib)
(require 'url)


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

(defcustom magit-gerrit-scp-program "scp"
  "The scp program used to download the commit-msg hook."
  :group 'magit-gerrit
  :type 'string)

(defcustom magit-gerrit-web-url nil
  "The base URL of the Gerrit web interface (e.g. \"https://host/r\").
When nil, it is deduced from the URL of a change of the project, or
from the host of the remote as \"https://HOST\"."
  :group 'magit-gerrit
  :type '(choice (const :tag "Deduce" nil) string))
(put 'magit-gerrit-web-url 'safe-local-variable #'stringp)

(defcustom magit-gerrit-commit-msg-hook-methods '(https scp)
  "Methods tried, in order, to download Gerrit's commit-msg hook.
`https' downloads it from the web interface (see
`magit-gerrit-web-url'), `scp' from the SSH daemon (with the legacy
scp protocol, Gerrit doesn't support SFTP)."
  :group 'magit-gerrit
  :type '(repeat (choice (const https) (const scp))))

(defcustom magit-gerrit-check-change-id t
  "Whether to check that a commit has a Change-Id before pushing it.
When it has none, offer to install the commit-msg hook if missing."
  :group 'magit-gerrit
  :type 'boolean)

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

(defun magit-gerrit--key (key)
  "Return KEY as a key vector.
KEY can be a vector, a raw key string or a `kbd'-style string."
  (if (and (stringp key) (key-valid-p key))
      (key-parse key)
    key))

(defvar magit-gerrit-mode-map (make-sparse-keymap)
  "Keymap of `magit-gerrit-mode'.
The key opening `magit-gerrit-dispatch' is `magit-gerrit-popup-prefix'.")

(defcustom magit-gerrit-label-values
  '(("Code-Review" -2 . 2) ("Verified" -1 . 1))
  "Range of the values of the review labels, as (LABEL MIN . MAX).
Gerrit's SSH interface doesn't tell which values a label accepts, add
the custom labels of your projects here.  Labels not listed here use
`magit-gerrit-default-label-values'."
  :group 'magit-gerrit
  :type '(alist :key-type (string :tag "Label")
                :value-type (cons (integer :tag "Min") (integer :tag "Max"))))

(defcustom magit-gerrit-default-label-values '(-1 . 1)
  "Range of the values of labels missing from `magit-gerrit-label-values'."
  :group 'magit-gerrit
  :type '(cons (integer :tag "Min") (integer :tag "Max")))

(defcustom magit-gerrit-popup-prefix "_"
  "Key used to open the `magit-gerrit-dispatch' transient.
Set it with the Customize interface or `setopt' (or `setq' before
loading `magit-gerrit') so that `magit-gerrit-mode-map' is updated."
  :group 'magit-gerrit
  :type 'key-sequence
  :set (lambda (symbol value)
         (when (and (boundp symbol) (symbol-value symbol))
           (define-key magit-gerrit-mode-map
                       (magit-gerrit--key (symbol-value symbol)) nil t))
         (set-default-toplevel-value symbol value)
         (define-key magit-gerrit-mode-map
                     (magit-gerrit--key value) #'magit-gerrit-dispatch)))

(defcustom magit-gerrit-signed-push-p nil
  "Whether to push with the --signed option by default."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-extra-options nil
  "Extra query terms added when fetching reviews (e.g. \"is:watched\").
This can be a list of terms, or a string of space-separated terms."
  :group 'magit-gerrit
  :type '(choice (repeat string) string))

(defcustom magit-gerrit-saved-queries nil
  "Saved Gerrit queries, as an alist of (NAME . QUERY).
See https://gerrit-review.googlesource.com/Documentation/user-search.html
and `magit-gerrit-save-query'."
  :group 'magit-gerrit
  :type '(alist :key-type (string :tag "Name") :value-type (string :tag "Query")))

(defcustom magit-gerrit-show-revision-change t
  "Whether to show the Gerrit change of a commit in revision buffers.
This applies to buffers where `magit-gerrit-mode' is enabled (see
`magit-gerrit-auto-enable')."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-annotate-log nil
  "Whether to annotate the commits of log buffers with their Gerrit change.
This applies to buffers where `magit-gerrit-mode' is enabled (see
`magit-gerrit-auto-enable').  It costs a Gerrit query per refresh for
the commits whose change isn't cached yet."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-annotate-log-max 100
  "Maximum number of commits annotated in a log buffer."
  :group 'magit-gerrit
  :type 'integer)

(defcustom magit-gerrit-change-cache-ttl 300
  "Number of seconds the changes found by Change-Id are cached."
  :group 'magit-gerrit
  :type 'integer)

(defcustom magit-gerrit-async-reviews nil
  "Whether to fetch the reviews of the status buffer asynchronously.
When non-nil, refreshing the status buffer shows the reviews fetched
previously (or a placeholder) and fetches them in the background; the
status buffer is refreshed again when they are fetched."
  :group 'magit-gerrit
  :type 'boolean)

(defcustom magit-gerrit-async-min-interval 10
  "Minimum number of seconds between two asynchronous fetches of reviews.
Refreshing the status buffer more often reuses the last result."
  :group 'magit-gerrit
  :type 'integer)

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

(defcustom magit-gerrit-change-sections-hook
  '(magit-gerrit-insert-change-headers
    magit-gerrit-insert-change-message
    magit-gerrit-insert-change-labels
    magit-gerrit-insert-change-reviewers
    magit-gerrit-insert-change-dependencies
    magit-gerrit-insert-change-files
    magit-gerrit-insert-change-patchsets
    magit-gerrit-insert-change-messages
    magit-gerrit-insert-change-comments)
  "Hook run to insert sections into a Gerrit change buffer."
  :group 'magit-gerrit
  :type 'hook)


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

(defvar-local magit-gerrit-query-all-projects nil
  "When non-nil, list the reviews of all projects, not only the current one.")

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

(defconst magit-gerrit-log-buffer-name "*magit-gerrit-log*"
  "Name of the buffer logging the Gerrit SSH commands.")

(defvar magit-gerrit-reviewer-history nil
  "Minibuffer history of reviewers and CCs.")
(add-to-list 'savehist-additional-variables 'magit-gerrit-reviewer-history)


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

(defun magit-gerrit--ssh-run (input cmd args)
  "Call Gerrit's command CMD with ARGS via SSH and return its output.
When INPUT is non-nil, it is sent to the standard input of the
command.  ARGS are processed as in `magit-gerrit--command'.  Signal a
`user-error' containing the error output when the command fails.
All the calls are logged in `magit-gerrit-log-buffer-name'."
  (let ((ssh-args (apply #'magit-gerrit--command cmd args))
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
            (magit-gerrit--log ssh-args exit-code stderr)
            (unless (eq exit-code 0)
              (user-error "Gerrit command \"%s\" failed: %s" cmd
                          (if (string-empty-p stderr)
                              (format "exit code %s" exit-code)
                            (car (split-string stderr "\n" t)))))
            (buffer-string)))
      (delete-file err-file)
      (when in-file (delete-file in-file)))))

(defun magit-gerrit--ssh-call (cmd &rest args)
  "Call Gerrit's command CMD with ARGS via SSH and return its output.
See `magit-gerrit--ssh-run'."
  (magit-gerrit--ssh-run nil cmd args))

(defun magit-gerrit--review-json (change input)
  "Run \"gerrit review --json\" on the current patchset of CHANGE.
INPUT is an alist, sent as a ReviewInput entity, see
https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#review-input"
  (magit-gerrit--ssh-run (json-encode input) "review"
                         (list "--project" (magit-gerrit-change-project change)
                               "--json"
                               (magit-gerrit-change-revision change))))

(defun magit-gerrit--json-parse (string)
  "Parse the JSON object in STRING as an alist, arrays as lists.
JSON null and false are parsed as nil.  Use the native parser when
available (always the case since Emacs 30), `json.el' otherwise."
  (if (json-available-p)
      (json-parse-string string :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-null nil)
          (json-false nil))
      (json-read-from-string string))))

(defun magit-gerrit--parse-json-lines (output)
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

(defvar magit-gerrit--known-accounts (make-hash-table :test #'equal)
  "Accounts seen in query results, offered when reading reviewers.")

(defun magit-gerrit--remember-accounts (change)
  "Remember the accounts (owner, reviewers, voters) involved in CHANGE."
  (dolist (account (append (list (alist-get 'owner change))
                           (alist-get 'allReviewers change)
                           (mapcar (lambda (a) (alist-get 'by a))
                                   (magit-gerrit--get change 'currentPatchSet 'approvals))))
    (when-let* ((id (or (alist-get 'email account) (alist-get 'username account))))
      (puthash id t magit-gerrit--known-accounts))))

(defun magit-gerrit--query-args (query options)
  "Return the arguments of \"gerrit query\" for QUERY and OPTIONS."
  (list "--format=JSON" options query))

(defun magit-gerrit--parse-query-output (output)
  "Parse the OUTPUT of a Gerrit query, return the parsed change list."
  (let ((changes (magit-gerrit--parse-json-lines output)))
    (mapc #'magit-gerrit--remember-accounts changes)
    changes))

(defun magit-gerrit--query-changes (query &rest options)
  "Run the Gerrit QUERY with OPTIONS, return the matching change list.
QUERY is a string or a list of query terms, OPTIONS are additional
options for \"gerrit query\" such as \"--current-patch-set\"."
  (magit-gerrit--parse-query-output
   (magit-gerrit--ssh-call "query" (magit-gerrit--query-args query options))))

(defun magit-gerrit--query-terms (&optional prj)
  "Return the query terms to list the reviews of project PRJ.
Uses `magit-gerrit-query-filters' or \"status:open\".  The project
is not part of the query when `magit-gerrit-query-all-projects' is
non-nil."
  (when-let* ((prj (or prj (magit-gerrit-get-project))))
    (append
     (unless magit-gerrit-query-all-projects
       (list (concat "project:" prj)))
     (if (stringp magit-gerrit-extra-options)
         (split-string magit-gerrit-extra-options)
       magit-gerrit-extra-options)
     (list (or magit-gerrit-query-filters "status:open")))))

(defconst magit-gerrit--reviews-query-options '("--current-patch-set" "--submit-records")
  "Options of the query listing the reviews.")

(defun magit-gerrit--fetch-reviews ()
  "Fetch the reviews of the current project."
  (when-let* ((terms (magit-gerrit--query-terms)))
    (apply #'magit-gerrit--query-changes terms magit-gerrit--reviews-query-options)))

(defvar magit-gerrit--async-cache (make-hash-table :test #'equal)
  "Reviews fetched asynchronously.
Keys are (TOPLEVEL CREDENTIALS QUERY-TERMS), values are plists with
the properties :time (of the last completed fetch), :changes, :error
and :process (the running fetch).")

(defun magit-gerrit--async-refresh-status (toplevel)
  "Refresh the status buffers of the repository TOPLEVEL."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'magit-status-mode)
                 (equal (magit-toplevel) toplevel))
        (magit-refresh-buffer)))))

(defun magit-gerrit--async-fetch (key terms)
  "Fetch the reviews matching TERMS in the background, store them at KEY."
  (let* ((toplevel (car key))
         (ssh-args (apply #'magit-gerrit--command "query"
                          (magit-gerrit--query-args terms magit-gerrit--reviews-query-options)))
         (stdout (generate-new-buffer " *magit-gerrit-async*"))
         (stderr (generate-new-buffer " *magit-gerrit-async-err*"))
         (default-directory (if (file-remote-p default-directory)
                                temporary-file-directory
                              default-directory))
         (process
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
                      (entry (gethash key magit-gerrit--async-cache))
                      (result (if (eq exit-code 0)
                                  (condition-case e
                                      (list :changes (magit-gerrit--parse-query-output
                                                      (with-current-buffer stdout (buffer-string))))
                                    (error (list :error (error-message-string e))))
                                (list :changes (plist-get entry :changes)
                                      :error (format "Gerrit command \"query\" failed: %s"
                                                     (if (string-empty-p err)
                                                         (format "exit code %s" exit-code)
                                                       (car (split-string err "\n" t))))))))
                 (magit-gerrit--log ssh-args exit-code err)
                 (kill-buffer stdout)
                 (kill-buffer stderr)
                 (puthash key (append (list :time (current-time)) result)
                          magit-gerrit--async-cache)
                 (magit-gerrit--async-refresh-status toplevel)))))))
    (puthash key (plist-put (copy-sequence (gethash key magit-gerrit--async-cache))
                            :process process)
             magit-gerrit--async-cache)))

(defun magit-gerrit--async-reviews ()
  "Return the cached reviews, fetching them in the background if needed.
Return a plist with the :changes, :error and :process properties."
  (when-let* ((terms (magit-gerrit--query-terms)))
    (let* ((key (list (magit-toplevel) magit-gerrit-ssh-creds terms))
           (entry (gethash key magit-gerrit--async-cache))
           (time (plist-get entry :time)))
      (unless (or (process-live-p (plist-get entry :process))
                  (and time (< (float-time (time-since time)) magit-gerrit-async-min-interval)))
        (magit-gerrit--async-fetch key terms))
      (gethash key magit-gerrit--async-cache))))

(defun magit-gerrit--message-args (args)
  "Return the \"gerrit review\" arguments from the transient ARGS.
These are the message and the notification setting."
  (append
   (when-let* ((msg (transient-arg-value "--message=" args)))
     (list "--message" msg))
   (when-let* ((notify (transient-arg-value "--notify=" args)))
     (list "--notify" notify))))

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

(defvar-local magit-gerrit-buffer-change-number nil
  "Number of the change shown in a `magit-gerrit-change-mode' buffer.")

(defvar-local magit-gerrit-buffer-change nil
  "The change shown in a `magit-gerrit-change-mode' buffer.")

(defun magit-gerrit-change-at-point ()
  "Return the Gerrit change at point, or nil.
In a change buffer, this is the change shown in the buffer."
  (or (magit-section-value-if 'gerrit-change)
      (and (derived-mode-p 'magit-gerrit-change-mode)
           magit-gerrit-buffer-change)))

(defun magit-gerrit-patchset-at-point ()
  "Return the patchset at point in a change buffer, or nil."
  (magit-section-value-if 'gerrit-patchset))

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
  "Return Change-Id for REV or the commit at point.
When the commit has multiple Change-Id trailers, return the first one."
  (when-let* ((rev (or rev (magit-commit-at-point)))
              (hash (magit-rev-parse rev)))
    (or (gethash hash magit-gerrit--rev-change-id-cache)
        (when-let* ((id (magit-git-string
                         "show" "--no-patch"
                         "--format=%(trailers:key=Change-Id,valueonly)" hash))
                    ((not (string-empty-p id))))
          (puthash hash id magit-gerrit--rev-change-id-cache)))))

(defun magit-gerrit--cached-change-id-info (id)
  "Return the cache entry of Change-Id ID as (FOUND . LIST).
LIST holds the changes found for ID.  FOUND is nil when ID isn't
cached or the cache entry expired."
  (when-let* ((entry (gethash id magit-gerrit--change-id-cache))
              ((< (float-time (time-since (car entry))) magit-gerrit-change-cache-ttl)))
    (cons t (cdr entry))))

(defun magit-gerrit--get-change-ids-info (ids &optional invalidate-cache)
  "Look up the Change-Ids IDS, return an alist (ID . LIST).
LIST holds the changes found for ID.
The changes not in the cache are fetched with a single query.  When
INVALIDATE-CACHE is non-nil, don't use cached values."
  (let* ((ids (seq-uniq ids))
         (missing (if invalidate-cache
                      ids
                    (seq-remove #'magit-gerrit--cached-change-id-info ids))))
    (when missing
      (let ((changes (magit-gerrit--query-changes
                      (list (concat "project:" (magit-gerrit-get-project))
                            (concat "(" (mapconcat (lambda (id) (concat "change:" id))
                                                   missing " OR ")
                                    ")"))
                      "--current-patch-set"))
            (now (current-time)))
        (dolist (id missing)
          (puthash id
                   (cons now (seq-filter (lambda (c) (equal (magit-gerrit-change-id c) id))
                                         changes))
                   magit-gerrit--change-id-cache))))
    (mapcar (lambda (id) (cons id (cdr (gethash id magit-gerrit--change-id-cache))))
            ids)))

(defun magit-gerrit--get-change-id-info (id &optional invalidate-cache)
  "Look up the Change-Id ID, return what Gerrit found for it.
There is one change per branch the commit was cherry-picked to.
When INVALIDATE-CACHE is non-nil, don't use cached values."
  (cdar (magit-gerrit--get-change-ids-info (list id) invalidate-cache)))

(defun magit-gerrit--commit-changes (rev)
  "Look up the change of commit REV and its cherry-picks.
The change whose current patchset is REV comes first."
  (when-let* ((id (magit-gerrit-commit-change-id rev)))
    (let ((hash (magit-rev-parse rev)))
      (seq-sort-by (lambda (change) (if (equal (magit-gerrit-change-revision change) hash) 0 1))
                   #'< (magit-gerrit--get-change-id-info id)))))

(defun magit-gerrit--format-change-summary (change)
  "Return a one-line summary of CHANGE: number, status and votes."
  (let ((votes (make-hash-table :test #'equal)))
    (dolist (approval (magit-gerrit-change-approvals change))
      (push (string-to-number (alist-get 'value approval))
            (gethash (alist-get 'type approval) votes)))
    (string-join
     (delq nil
           (append
            (list (propertize (format "#%s" (magit-gerrit-change-number change))
                              'font-lock-face 'magit-hash)
                  (propertize (concat (magit-gerrit-change-status change)
                                      (if (magit-gerrit-change-wip-p change) " WIP" ""))
                              'font-lock-face
                              (pcase (magit-gerrit-change-status change)
                                ("MERGED" 'success) ("ABANDONED" 'shadow) (_ 'warning))))
            (mapcar (lambda (label)
                      (when-let* ((values (gethash (car label) votes)))
                        (let* ((min (apply #'min values)) (max (apply #'max values))
                               (value (if (< min 0) min max)))
                          (concat (cadr label) (magit-gerrit--format-vote value)))))
                    magit-gerrit-review-labels)))
     " ")))


;;; Gitiles

(defun magit-gerrit--get-gitiles-link (&optional line-number)
  "Get Gitiles link, optionally with LINE-NUMBER."
  (when-let* ((gerrit-proj (magit-gerrit-get-project))
              (base-url (or magit-gerrit-gitiles-base-url
                            (when-let* ((remote-url (magit-gerrit-get-remote-url))
                                        (url (url-generic-parse-url remote-url)))
                              (concat "https://" (url-host url) "/plugins/gitiles"))))
              (branch "master")
              (file-name (buffer-file-name))
              (toplevel (magit-toplevel))
              (file-name (file-relative-name file-name toplevel)))
    (concat base-url "/" gerrit-proj "/+/refs/heads/" branch "/" file-name
            (when line-number (format "#%d" line-number)))))


;;; Formatting

(defun magit-gerrit-format-short-timestring (seconds)
  "Format SECONDS as a short human readable duration (e.g. \" 3 days\")."
  (or (cl-loop for (unit . length) in '(("year" . 31536000) ("month" . 2592000)
                                        ("day" . 86400) ("hour" . 3600)
                                        ("minute" . 60))
               for count = (floor seconds length)
               when (>= count 1)
               return (format "%2d %s%s" count unit (if (> count 1) "s" "")))
      "just now"))

(define-obsolete-function-alias 'magit-gerrit-string-real-length #'string-width "0.2.0")

(defun magit-gerrit-create-review-labels ()
  "Create review labels heading."
  (mapconcat #'cadr magit-gerrit-review-labels " "))

(defun magit-gerrit--window-width ()
  "Return the width of the window showing the current buffer."
  (window-width (get-buffer-window nil t)))

(defun magit-gerrit--cell (string width &optional face)
  "Pad or truncate STRING to WIDTH columns, with optional FACE.
Widths are computed in columns, so wide characters are aligned."
  (let ((cell (truncate-string-to-width (or string "") width nil ?\s)))
    (if face (propertize cell 'font-lock-face face) cell)))

(defun magit-gerrit--format-line (patchset subject owner branch size updated labels
                                           &optional subject-face)
  "Format a line of the reviews section.
PATCHSET, SUBJECT, OWNER, BRANCH, SIZE, UPDATED and LABELS are the
cells.  The subject takes the remaining width, the branch, size,
update time and labels are hidden in narrow windows.  SUBJECT-FACE is
the face of the subject."
  (let* ((width (magit-gerrit--window-width))
         (columns (cond ((> width 128) '(branch size updated labels))
                        ((> width 108) '(size updated labels))
                        ((> width 94)  '(size labels))
                        ((> width 80)  '(labels))))
         (head (magit-gerrit--cell patchset 13))
         (tail (concat (magit-gerrit--cell owner 10 'magit-log-author)
                       (when (memq 'branch columns) (magit-gerrit--cell branch 20 'magit-hash))
                       (when (memq 'size columns) (magit-gerrit--cell size 15))
                       (when (memq 'updated columns) (magit-gerrit--cell updated 12))
                       (when (memq 'labels columns) labels)))
         ;; Keep one column free to prevent the long line arrow from being shown
         (subject-width (max 10 (- width (string-width head) (string-width tail) 1))))
    (concat head (magit-gerrit--cell subject subject-width subject-face) tail "\n")))

(defun magit-gerrit-pretty-print-review-header ()
  "Return the header line of the reviews section."
  (propertize (magit-gerrit--format-line "Patchset" "Subject" "Owner" "Branch"
                                         "     Delta" "Updated"
                                         (magit-gerrit-create-review-labels))
              'font-lock-face 'highlight))

(defun magit-gerrit--format-review (change)
  "Return the line of the reviews section for CHANGE."
  (magit-gerrit--format-line
   (concat (propertize (format "%-8s" (magit-gerrit-change-number change))
                       'font-lock-face 'magit-hash)
           (propertize (format "[%s]" (magit-gerrit-change-patchset-number change))
                       'font-lock-face 'magit-hash))
   (magit-gerrit-change-subject change)
   (magit-gerrit-change-owner-name change)
   (magit-gerrit-change-branch change)
   (concat (propertize (format "%7s" (format "+%d" (magit-gerrit-change-insertions change)))
                       'font-lock-face 'diff-added)
           " "
           (propertize (format "-%d" (magit-gerrit-change-deletions change))
                       'font-lock-face 'diff-removed))
   (when-let* ((time (magit-gerrit-change-last-updated change)))
     (magit-gerrit-format-short-timestring (time-to-seconds (time-since time))))
   (magit-gerrit-wash-approvals-oneline (magit-gerrit-change-approvals change))
   (if (magit-gerrit-change-wip-p change) 'magit-dimmed 'magit-filename)))

(defun magit-gerrit--label-range (label)
  "Return the range of the values of LABEL, as (MIN . MAX)."
  (or (cdr (assoc label magit-gerrit-label-values))
      magit-gerrit-default-label-values))

(defun magit-gerrit-trans-score (type value-list)
  "Format the votes VALUE-LIST of label TYPE as a single character."
  (if (and value-list (length> value-list 0))
      (let* ((min-v (apply #'min value-list))
             (max-v (apply #'max value-list))
             (range (magit-gerrit--label-range type))
             (t-min-v (car range))
             (t-max-v (cdr range)))
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
      (let ((type (alist-get 'type approval))
            (value (string-to-number (alist-get 'value approval))))
        (push value (alist-get type type-values nil nil #'string-equal))))
    (mapconcat
     (lambda (elem)
       (let* ((long (car elem))
              (score (magit-gerrit-trans-score
                      long (alist-get long type-values nil nil #'string-equal))))
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
      (insert (magit-gerrit--format-review change)))))

(defvar-keymap magit-gerrit-change-section-map
  :doc "Keymap for the sections of Gerrit changes."
  "<remap> <magit-visit-thing>" #'magit-gerrit-show-change)

(defun magit-gerrit-insert-reviews-section ()
  "Insert the Gerrit reviews section in the status buffer.
Errors are displayed in the section instead of aborting the refresh."
  (when (magit-gerrit-get-project)
    (magit-insert-section (gerrit-reviews nil magit-gerrit-collapse-patchset-section)
      (magit-insert-heading
        (format "Reviews [%s%s]:"
                (or magit-gerrit-query-filters "status:open")
                (if magit-gerrit-query-all-projects ", all projects" "")))
      (insert (magit-gerrit-pretty-print-review-header))
      (if magit-gerrit-async-reviews
          (let ((entry (magit-gerrit--async-reviews)))
            (mapc #'magit-gerrit--insert-review (plist-get entry :changes))
            (when-let* ((err (plist-get entry :error)))
              (insert (propertize (format "%s\n" err) 'font-lock-face 'error)))
            (when (and (not (plist-get entry :time)) (process-live-p (plist-get entry :process)))
              (insert (propertize "Loading...\n" 'font-lock-face 'shadow))))
        (condition-case err
            (mapc #'magit-gerrit--insert-review (magit-gerrit--fetch-reviews))
          (error
           (insert (propertize (format "%s\n" (error-message-string err))
                               'font-lock-face 'error)))))
      (insert "\n"))))


;;; Patchsets

(defun magit-gerrit-create-branch-force (branch parent)
  "Switch HEAD to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\(\"git checkout -B BRANCH REVISION\")."
  (when (and branch (not (string-empty-p branch)))
    (magit-save-repository-buffers)
    (magit-run-git "checkout" "-B" branch parent)))

(defun magit-gerrit-get-ref (&optional select-number)
  "Return the ref of the review at point.
This is the ref of the patchset at point in a change buffer, or of
the current patchset of the change at point otherwise.  When
SELECT-NUMBER is non-nil, ask for the patchset number."
  (if-let* (((not select-number))
            (patchset (magit-gerrit-patchset-at-point)))
      (alist-get 'ref patchset)
    (when-let* ((change (magit-gerrit-change-at-point))
                (ref (magit-gerrit-change-ref change))
                (number (magit-gerrit-change-patchset-number change)))
      (if select-number
          (string-join
           (append (butlast (string-split ref "/"))
                   (list (number-to-string
                          (read-number (format "Select patch number (up to %d): " number)))))
           "/")
        ref))))

(defun magit-gerrit-fetch-patchset (&optional ref select-number)
  "Fetch a Gerrit review patchset at REF, optionally SELECT-NUMBER.
The patchset is available as FETCH_HEAD afterwards.  Return REF."
  (let ((ref (or ref (magit-gerrit-get-ref select-number)
                 (user-error "No Gerrit change at point"))))
    (magit-gerrit-get-remote-url)  ; Make sure `magit-gerrit-remote' is set
    (message "Fetching %s from %s..." ref magit-gerrit-remote)
    (unless (zerop (magit-call-git "fetch" magit-gerrit-remote ref))
      (user-error "Failed to fetch %s from %s, see %s"
                  ref magit-gerrit-remote
                  (substitute-command-keys "\\[magit-process-buffer]")))
    ref))


;;; Commands

(defun magit-gerrit-set-query (query &optional global)
  "Set the QUERY listing the reviews and refresh.
QUERY nil means the default, \"status:open\".  Set it for the
current repository, or for all the repositories when GLOBAL is
non-nil."
  (if global
      (progn (kill-local-variable 'magit-gerrit-query-filters)
             (setq-default magit-gerrit-query-filters query))
    (setq-local magit-gerrit-query-filters query))
  (magit-refresh))

(defun magit-gerrit-edit-filters (arg)
  "Edit the Gerrit query filters (an empty query means the default).
Edit globally when called with universal argument ARG."
  (interactive "P")
  (let ((filters (string-trim
                  (read-string "Query filters: " magit-gerrit-query-filters
                               'magit-gerrit-query-filters-history))))
    (magit-gerrit-set-query (unless (string-empty-p filters) filters) arg)))

(defmacro magit-gerrit--define-query (name query doc)
  "Define a command named after NAME setting QUERY, with docstring DOC."
  (declare (indent 1) (doc-string 3))
  `(defun ,(intern (format "magit-gerrit-query-%s" name)) (&optional global)
     ,(concat doc "\nSet it globally when called with a prefix argument GLOBAL.")
     (interactive "P")
     (magit-gerrit-set-query ,query global)))

(magit-gerrit--define-query open nil
  "List the open changes (the default).")
(magit-gerrit--define-query mine "owner:self status:open"
  "List my open changes.")
(magit-gerrit--define-query incoming "reviewer:self -owner:self -is:wip status:open"
  "List the open changes I'm reviewing.")
(magit-gerrit--define-query starred "is:starred status:open"
  "List the open changes I starred.")
(magit-gerrit--define-query watched "is:watched status:open"
  "List the open changes I watch.")
(magit-gerrit--define-query merged "status:merged -age:1week"
  "List the changes merged during the last week.")

(defun magit-gerrit-query-topic (topic &optional global)
  "Show the reviews of TOPIC, by default the topic of the change at point.
Set it globally when called with a prefix argument GLOBAL."
  (interactive
   (list (let ((default (when-let* ((change (magit-gerrit-change-at-point)))
                          (magit-gerrit-change-topic change))))
           (read-string (format-prompt "Topic" default) nil nil default))
         current-prefix-arg))
  (when (string-empty-p topic)
    (user-error "No topic"))
  ;; Gerrit allows quoting values with curly braces
  (magit-gerrit-set-query (format "topic:{%s}" topic) global))

(defun magit-gerrit-query-saved (query &optional global)
  "Show the reviews matching the saved QUERY.
See `magit-gerrit-saved-queries'.
Set it globally when called with a prefix argument GLOBAL."
  (interactive
   (list (let ((name (completing-read "Saved query: " magit-gerrit-saved-queries nil t)))
           (or (cdr (assoc name magit-gerrit-saved-queries)) (user-error "No saved query")))
         current-prefix-arg))
  (magit-gerrit-set-query query global))

(defun magit-gerrit-save-query (name query)
  "Save the current QUERY as NAME in `magit-gerrit-saved-queries'."
  (interactive
   (let ((query (read-string "Query: " (or magit-gerrit-query-filters "status:open"))))
     (list (read-string "Name: ") query)))
  (customize-save-variable 'magit-gerrit-saved-queries
                           (cons (cons name query)
                                 (assoc-delete-all name (copy-sequence magit-gerrit-saved-queries)))))

(defun magit-gerrit-toggle-all-projects ()
  "Toggle listing the reviews of all the projects."
  (interactive)
  (setq-local magit-gerrit-query-all-projects (not magit-gerrit-query-all-projects))
  (magit-refresh))

;;;###autoload (autoload 'magit-gerrit-query "magit-gerrit" nil t)
(transient-define-prefix magit-gerrit-query ()
  "Choose the Gerrit query listing the reviews.
With a prefix argument, the query is set for all repositories."
  [:description
   (lambda () (format "Current query: %s%s"
                      (propertize (or magit-gerrit-query-filters "status:open")
                                  'face 'transient-value)
                      (if magit-gerrit-query-all-projects " (all projects)" "")))
   ["Presets"
    ("o" "Open changes"            magit-gerrit-query-open)
    ("m" "My changes"              magit-gerrit-query-mine)
    ("i" "Incoming reviews"        magit-gerrit-query-incoming)
    ("s" "Starred"                 magit-gerrit-query-starred)
    ("w" "Watched"                 magit-gerrit-query-watched)
    ("M" "Recently merged"         magit-gerrit-query-merged)]
   ["Custom"
    ("t" "Topic"                   magit-gerrit-query-topic)
    ("e" "Edit query"              magit-gerrit-edit-filters)
    ("l" "Saved query"             magit-gerrit-query-saved)
    ("S" "Save a query"            magit-gerrit-save-query)
    ("a" "Toggle all projects"     magit-gerrit-toggle-all-projects)]])

(define-obsolete-function-alias 'magit-gerrit-change-id-browse-url
  #'magit-gerrit-browse-commit "0.2.0")

(defun magit-gerrit-gitiles-browse-url (with-line-p)
  "Browse this file in Gitiles, highlight current line when WITH-LINE-P."
  (interactive "P")
  (browse-url (or (magit-gerrit--get-gitiles-link
                   (when with-line-p (line-number-at-pos (point))))
                  (user-error "Cannot build a Gitiles link for this buffer"))))

(defun magit-gerrit-view-patchset-diff (&optional select-number)
  "View the diff for a patchset, optionally SELECT-NUMBER."
  (interactive "P")
  (when-let* ((ref (magit-gerrit-get-ref select-number)))
    (let ((dir default-directory))
      (magit-gerrit-fetch-patchset ref)
      (message "Generating Gerrit Patchset for refs %s dir %s" ref dir)
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
      (message "Checking out refs %s to %s in %s" ref branch dir)
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

(defun magit-gerrit--copy (string)
  "Copy STRING to the kill ring (and the clipboard) and show it."
  (kill-new string)
  (message "Copied: %s" string))

(defun magit-gerrit-copy-review (with-commit-message)
  "Copy review url and commit message when WITH-COMMIT-MESSAGE is non-nil."
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--copy
     (concat (when with-commit-message
               (format "%s: %s | "
                       (magit-gerrit-change-number change)
                       (magit-gerrit-change-subject change)))
             (magit-gerrit-change-url change)))))

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
  (magit-gerrit--copy (or (magit-gerrit-get-ref select-number)
                          (user-error "No Gerrit change at point"))))

(defun magit-gerrit--read-account-list (prompt)
  "Read a non-empty list of accounts with PROMPT."
  (or (completing-read-multiple prompt (magit-gerrit--account-candidates)
                                nil nil nil 'magit-gerrit-reviewer-history)
      (user-error "No account")))

(defun magit-gerrit--change-reviewers (change)
  "Return the reviewers of CHANGE, querying them if needed."
  (or (alist-get 'allReviewers change)
      (alist-get 'allReviewers
                 (car (magit-gerrit--query-changes
                       (format "change:%s" (magit-gerrit-change-number change))
                       "--all-reviewers")))))

(defun magit-gerrit-add-reviewer (accounts)
  "Add ACCOUNTS as reviewers to the change at point."
  (interactive (list (magit-gerrit--read-account-list "Add reviewers: ")))
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--ssh-call
     "set-reviewers"
     "--project" (magit-gerrit-change-project change)
     (mapcar (lambda (account) (list "--add" account)) accounts)
     (magit-gerrit-change-id change))
    (magit-refresh)))

(defun magit-gerrit-remove-reviewer (accounts)
  "Remove ACCOUNTS from the reviewers of the change at point."
  (interactive
   (let ((change (magit-gerrit--change-at-point-or-error)))
     (list (or (completing-read-multiple
                "Remove reviewers: "
                (seq-keep (lambda (account)
                            (or (alist-get 'email account) (alist-get 'username account)))
                          (magit-gerrit--change-reviewers change)))
               (user-error "No account")))))
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--ssh-call
     "set-reviewers"
     "--project" (magit-gerrit-change-project change)
     (mapcar (lambda (account) (list "--remove" account)) accounts)
     (magit-gerrit-change-id change))
    (magit-refresh)))

(defun magit-gerrit-add-cc (accounts)
  "Add ACCOUNTS in CC of the change at point."
  (interactive (list (magit-gerrit--read-account-list "Add CC: ")))
  (magit-gerrit--review-json
   (magit-gerrit--change-at-point-or-error)
   `((reviewers . ,(vconcat (mapcar (lambda (account) `((reviewer . ,account) (state . "CC")))
                                    accounts)))))
  (magit-refresh))

(defun magit-gerrit-mark-wip (args)
  "Mark the change at point as work in progress, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review-json
   (magit-gerrit--change-at-point-or-error)
   `((work_in_progress . t)
     ,@(when-let* ((msg (transient-arg-value "--message=" args))) `((message . ,msg)))))
  (magit-refresh))

(defun magit-gerrit-mark-ready (args)
  "Mark the change at point as ready for review, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--review-json
   (magit-gerrit--change-at-point-or-error)
   `((ready . t)
     ,@(when-let* ((msg (transient-arg-value "--message=" args))) `((message . ,msg)))))
  (magit-refresh))

(defun magit-gerrit-set-topic (topic)
  "Set the TOPIC of the change at point (an empty topic to remove it)."
  (interactive
   (let ((change (magit-gerrit--change-at-point-or-error)))
     (list (read-string "Topic (empty to remove): " (magit-gerrit-change-topic change)))))
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--ssh-call
     "set-topic"
     (format "%s~%s~%s" (magit-gerrit-change-project change)
             (magit-gerrit-change-branch change) (magit-gerrit-change-id change))
     (unless (string-empty-p (string-trim topic))
       (list "--topic" (string-trim topic))))
    (magit-refresh)))

(defun magit-gerrit-rebase-review ()
  "Rebase the change at point on Gerrit."
  (interactive)
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error) "--rebase")
  (magit-refresh))

(defun magit-gerrit-move-review (branch args)
  "Move the change at point to BRANCH, using transient ARGS."
  (interactive (list (magit-gerrit--read-target-branch "Move to branch: ")
                     (magit-gerrit-arguments)))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        "--move" branch (magit-gerrit--message-args args))
  (magit-refresh))

(defun magit-gerrit-arguments ()
  "Return the arguments of the `magit-gerrit-dispatch' transient."
  (transient-args 'magit-gerrit-dispatch))

(defun magit-gerrit--label-votes (label)
  "Return the possible votes for LABEL, as \"LABEL=VALUE\" strings."
  (let ((range (magit-gerrit--label-range label)))
    (mapcar (lambda (value)
              (format "%s=%s" label (if (> value 0) (format "+%d" value) value)))
            (number-sequence (cdr range) (car range) -1))))

(defun magit-gerrit--read-votes (change &optional label)
  "Read votes on CHANGE, as a list of \"LABEL=VALUE\" strings.
When LABEL is non-nil, read a single vote for this label."
  (if label
      (list (completing-read "Vote: " (magit-gerrit--label-votes label) nil t))
    (completing-read-multiple "Votes (LABEL=VALUE, comma separated): "
                              (mapcan #'magit-gerrit--label-votes
                                      (magit-gerrit--change-labels change))
                              nil t)))

(defun magit-gerrit-vote (votes args)
  "Vote VOTES on the change at point, using transient ARGS.
VOTES is a list of \"LABEL=VALUE\" strings; interactively, they are
read with completion among the labels of the change and their values
\(see `magit-gerrit-label-values').  With the \"--submit\" argument,
submit the change after voting."
  (interactive (let ((change (magit-gerrit--change-at-point-or-error)))
                 (list (magit-gerrit--read-votes change) (magit-gerrit-arguments))))
  (unless votes
    (user-error "No vote"))
  (magit-gerrit--review (magit-gerrit--change-at-point-or-error)
                        (mapcar (lambda (vote) (list "--label" vote)) votes)
                        (magit-gerrit--message-args args)
                        (when (member "--submit" args) "--submit"))
  (when (member "--submit" args)
    (magit-fetch-all-no-prune))
  (magit-refresh))

(defun magit-gerrit-code-review (args)
  "Vote on the Code-Review label of the change at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit-vote (magit-gerrit--read-votes (magit-gerrit--change-at-point-or-error)
                                               "Code-Review")
                     args))

(defun magit-gerrit-verify-review (args)
  "Vote on the Verified label of the change at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit-vote (magit-gerrit--read-votes (magit-gerrit--change-at-point-or-error)
                                               "Verified")
                     args))

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


;;; Commit-msg hook

(defun magit-gerrit--web-url ()
  "Return the base URL of the Gerrit web interface.
See `magit-gerrit-web-url'."
  (or magit-gerrit-web-url
      (when-let* ((project (magit-gerrit-get-project))
                  (change (car (ignore-errors
                                 (magit-gerrit--query-changes
                                  (list (concat "project:" project) "limit:1")))))
                  (url (magit-gerrit-change-url change))
                  ((string-match "\\`\\(.*?\\)\\(?:/c/.*\\|/[0-9]+/?\\)\\'" url)))
        (match-string 1 url))
      (when-let* ((remote-url (magit-gerrit-get-remote-url))
                  (host (url-host (url-generic-parse-url remote-url))))
        (concat "https://" host))))

(defun magit-gerrit--commit-msg-hook-file ()
  "Return the path of the commit-msg hook of the current repository."
  (let ((default-directory (or (magit-toplevel) default-directory)))
    (expand-file-name (magit-git-string "rev-parse" "--git-path" "hooks/commit-msg"))))

(defun magit-gerrit--download-hook-https (file)
  "Download the commit-msg hook from the web interface to FILE.
Return non-nil on success."
  (let* ((url (concat (string-trim-right (magit-gerrit--web-url) "/") "/tools/hooks/commit-msg"))
         (buffer (ignore-errors (url-retrieve-synchronously url t t 30))))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (and (looking-at "HTTP/[0-9.]+ 200")
                       (re-search-forward "\r?\n\r?\n" nil t)
                       (looking-at "#!"))
              (let ((coding-system-for-write 'no-conversion))
                (write-region (point) (point-max) file nil 'silent))
              (magit-gerrit--log (list "GET" url) 0 "")
              t))
        (kill-buffer buffer)))))

(defun magit-gerrit--download-hook-scp (file)
  "Download the commit-msg hook from the SSH daemon to FILE.
Return non-nil on success."
  (let ((tmp (make-temp-file "magit-gerrit-hook-"))
        (default-directory temporary-file-directory))
    (unwind-protect
        (when (seq-some
               (lambda (legacy)
                 (let ((args (append (when legacy '("-O"))
                                     (list "-P" (number-to-string magit-gerrit-port)
                                           (concat magit-gerrit-ssh-creds ":hooks/commit-msg")
                                           tmp))))
                   (let ((exit-code (apply #'call-process magit-gerrit-scp-program nil nil nil args)))
                     (magit-gerrit--log (cons magit-gerrit-scp-program args) exit-code "")
                     (eq exit-code 0))))
               ;; "-O" (legacy scp protocol) is needed since OpenSSH 9, but older
               ;; versions don't know it
               '(t nil))
          (copy-file tmp file t)
          t)
      (delete-file tmp))))

(defun magit-gerrit-install-commit-msg-hook (&optional force)
  "Install Gerrit's commit-msg hook, generating the Change-Id of commits.
Ask before replacing an existing hook, unless FORCE is non-nil.  The
methods of `magit-gerrit-commit-msg-hook-methods' are tried in order."
  (interactive)
  (magit-gerrit-get-remote-url)
  (let ((file (magit-gerrit--commit-msg-hook-file)))
    (when (or force (not (file-exists-p file))
              (y-or-n-p (format "Replace the existing %s? " file)))
      (make-directory (file-name-directory file) t)
      (if (seq-some (lambda (method)
                      (pcase method
                        ('https (magit-gerrit--download-hook-https file))
                        ('scp (magit-gerrit--download-hook-scp file))))
                    magit-gerrit-commit-msg-hook-methods)
          (progn (set-file-modes file #o755)
                 (message "Installed the commit-msg hook in %s" file)
                 file)
        (user-error "Cannot download the commit-msg hook, see %s" magit-gerrit-log-buffer-name)))))

(defun magit-gerrit--check-change-id (rev)
  "Check that REV has a Change-Id before pushing it, return nil to abort.
When it has none and the commit-msg hook isn't installed, offer to
install it."
  (or (not magit-gerrit-check-change-id)
      (magit-gerrit-commit-change-id rev)
      (if (file-exists-p (magit-gerrit--commit-msg-hook-file))
          (y-or-n-p "The commit has no Change-Id, push anyway? ")
        (if (y-or-n-p "The commit has no Change-Id and the commit-msg hook isn't installed, install it? ")
            (progn (magit-gerrit-install-commit-msg-hook t)
                   (message "Hook installed, amend the commit to add a Change-Id, then push")
                   nil)
          (y-or-n-p "Push anyway? ")))))


;;; Push

(defun magit-gerrit--account-candidates ()
  "Return the accounts to offer when reading reviewers.
These are the previously entered ones and the ones seen in queries."
  (seq-uniq (append magit-gerrit-reviewer-history
                    (hash-table-keys magit-gerrit--known-accounts))))

(defun magit-gerrit--read-accounts (prompt &optional initial-input _history)
  "Read a comma separated list of accounts with PROMPT and INITIAL-INPUT.
Return them as a comma separated string."
  (let ((accounts (completing-read-multiple prompt (magit-gerrit--account-candidates)
                                            nil nil initial-input
                                            'magit-gerrit-reviewer-history)))
    (when accounts
      (string-join accounts ","))))

(defun magit-gerrit--default-target-branch ()
  "Return the branch the current branch is pushed for review to, or nil.
This is the upstream branch of the current branch, when it is on the
Gerrit remote."
  (when-let* ((branch (magit-get-current-branch))
              ((equal (magit-get "branch" branch "remote") magit-gerrit-remote))
              (merge (magit-get "branch" branch "merge")))
    (string-remove-prefix "refs/heads/" merge)))

(defun magit-gerrit--read-target-branch (prompt &optional initial-input history)
  "Read the target branch of a review with PROMPT, INITIAL-INPUT and HISTORY."
  (magit-gerrit-get-remote-url)  ; Make sure `magit-gerrit-remote' is set
  (magit-completing-read prompt
                         (magit-list-remote-branch-names magit-gerrit-remote t)
                         nil nil initial-input history
                         (magit-gerrit--default-target-branch)))

(defun magit-gerrit--encode-push-message (message)
  "Encode MESSAGE for the \"m\" push option.
Gerrit replaces underscores by spaces, then percent-decodes the
message, so all the characters except alphanumerics, \"-\", \".\" and
\"~\" are percent-encoded."
  (mapconcat (lambda (byte)
               (if (or (<= ?a byte ?z) (<= ?A byte ?Z) (<= ?0 byte ?9) (memq byte '(?- ?. ?~)))
                   (char-to-string byte)
                 (format "%%%02X" byte)))
             (encode-coding-string message 'utf-8)
             ""))

(defun magit-gerrit--check-push-value (name value)
  "Signal an error when VALUE, the value of push option NAME, is invalid."
  (when (string-match-p "[ ,%\t\n]" value)
    (user-error "The %s can't contain spaces, commas or %%: %S" name value))
  value)

(defun magit-gerrit--push-options (args)
  "Return the Gerrit push options for the transient ARGS.
See https://gerrit-review.googlesource.com/Documentation/user-upload.html#push_options"
  (let ((split (lambda (arg) (when-let* ((value (transient-arg-value arg args)))
                               (split-string value "," t "[ \t]+")))))
    (append
     (when-let* ((topic (transient-arg-value "--topic=" args)))
       (list (concat "topic=" (magit-gerrit--check-push-value "topic" topic))))
     (mapcar (lambda (tag) (concat "t=" (magit-gerrit--check-push-value "hashtag" tag)))
             (funcall split "--hashtag="))
     (mapcar (lambda (account) (concat "r=" (magit-gerrit--check-push-value "reviewer" account)))
             (funcall split "--reviewers="))
     (mapcar (lambda (account) (concat "cc=" (magit-gerrit--check-push-value "CC" account)))
             (funcall split "--cc="))
     (seq-keep (lambda (flag) (and (member (concat "--" flag) args) flag))
               '("wip" "ready" "private" "remove-private" "publish-comments"))
     (when-let* ((notify (transient-arg-value "--notify=" args)))
       (list (concat "notify=" notify)))
     (when-let* ((message (transient-arg-value "--message=" args)))
       (list (concat "m=" (magit-gerrit--encode-push-message message)))))))

(defun magit-gerrit--push-refspec (rev branch args)
  "Return the refspec to push REV for review on BRANCH with transient ARGS."
  (let ((options (magit-gerrit--push-options args)))
    (format "%s:refs/for/%s%s" rev branch
            (if options (concat "%" (string-join options ",")) ""))))

(defun magit-gerrit-push-for-review (args)
  "Push the commit at point (or HEAD) for review, using transient ARGS.
The target branch is the \"--branch=\" argument, the upstream branch
of the current branch or read from the minibuffer."
  (interactive (list (transient-args 'magit-gerrit-push)))
  (magit-gerrit-get-remote-url)  ; Make sure `magit-gerrit-remote' is set
  (let* ((commit (or (magit-commit-at-point) "HEAD"))
         (rev (or (magit-rev-parse commit) (user-error "No commit to push")))
         (branch (or (transient-arg-value "--branch=" args)
                     (magit-gerrit--default-target-branch)
                     (magit-gerrit--read-target-branch "Target branch: "))))
    (when (magit-gerrit--check-change-id rev)
      (magit-run-git-async "push" "-v" (when (member "--signed" args) "--signed")
                           magit-gerrit-remote
                           (magit-gerrit--push-refspec rev branch args)))))

(defun magit-gerrit-push-wip-for-review (args)
  "Push the commit at point (or HEAD) as work in progress, using ARGS."
  (interactive (list (transient-args 'magit-gerrit-push)))
  (magit-gerrit-push-for-review (cons "--wip" (remove "--ready" args))))

(define-obsolete-function-alias 'magit-gerrit-push-draft-for-review
  #'magit-gerrit-push-wip-for-review "0.2.0"
  "Drafts were replaced by work in progress changes in Gerrit 2.15.")
(define-obsolete-function-alias 'magit-gerrit-create-review 'magit-gerrit-push-for-review "1.0.0")
(define-obsolete-function-alias 'magit-gerrit-create-draft 'magit-gerrit-push-draft-for-review "1.0.0")

;;;###autoload (autoload 'magit-gerrit-push "magit-gerrit" nil t)
(transient-define-prefix magit-gerrit-push ()
  "Push a commit for review to Gerrit."
  :init-value (lambda (obj) (oset obj value (and magit-gerrit-signed-push-p '("--signed"))))
  :incompatible '(("--wip" "--ready") ("--private" "--remove-private"))
  ["Arguments"
   ("-b" "Target branch"      "--branch=" :reader magit-gerrit--read-target-branch)
   ("-t" "Topic"              "--topic=")
   ("-h" "Hashtags (a,b)"     "--hashtag=")
   ("-r" "Reviewers"          "--reviewers=" :reader magit-gerrit--read-accounts)
   ("-c" "CC"                 "--cc=" :reader magit-gerrit--read-accounts)
   ("-m" "Message"            "--message=")
   ("-n" "Notify"             "--notify=" :choices ("NONE" "OWNER" "OWNER_REVIEWERS" "ALL"))
   ("-w" "Work in progress"   "--wip")
   ("-R" "Ready for review"   "--ready")
   ("-p" "Private"            "--private")
   ("-P" "Remove private"     "--remove-private")
   ("-e" "Publish draft comments" "--publish-comments")
   ("-S" "Signed push"        "--signed")]
  ["Push commit at point (or HEAD)"
   ("p" "for review"                       magit-gerrit-push-for-review)
   ("w" "as work in progress"              magit-gerrit-push-wip-for-review)])


;;; Revision and log buffers

(defun magit-gerrit-insert-revision-change ()
  "Insert the Gerrit change of the commit shown in a revision buffer."
  (when (and magit-gerrit-show-revision-change
             (bound-and-true-p magit-gerrit-mode)
             (bound-and-true-p magit-buffer-revision))
    (condition-case err
        (when-let* ((changes (magit-gerrit--commit-changes magit-buffer-revision)))
          (magit-insert-section (gerrit-revision-changes)
            (dolist (change changes)
              (magit-insert-section (gerrit-change change)
                (insert (format "%-11s" "Gerrit:")
                        (magit-gerrit--format-change-summary change)
                        " "
                        (propertize (magit-gerrit-change-branch change) 'font-lock-face 'magit-branch-remote)
                        " "
                        (propertize (or (magit-gerrit-change-url change) "") 'font-lock-face 'link)
                        "\n")))
            (insert "\n")))
      (error
       (insert (propertize (format "Gerrit: %s\n\n" (error-message-string err))
                           'font-lock-face 'error))))))

(magit-add-section-hook 'magit-revision-sections-hook
                        #'magit-gerrit-insert-revision-change
                        #'magit-insert-revision-headers t)

(defun magit-gerrit--log-commits ()
  "Return the commit sections of the current log buffer."
  (let (sections)
    (magit-map-sections (lambda (section)
                          (when (eq (oref section type) 'commit)
                            (push section sections))))
    (nreverse sections)))

(defun magit-gerrit-annotate-log ()
  "Annotate the commits of the current log buffer with their change.
See `magit-gerrit-annotate-log'."
  (remove-overlays (point-min) (point-max) 'magit-gerrit-annotation t)
  (when (and magit-gerrit-annotate-log (bound-and-true-p magit-gerrit-mode))
    (condition-case err
        (let* ((sections (seq-take (magit-gerrit--log-commits) magit-gerrit-annotate-log-max))
               (ids (when sections
                      (mapcar (lambda (line) (split-string line "\0"))
                              (magit-git-lines "show" "--no-patch"
                                               "--format=%h%x00%(trailers:key=Change-Id,valueonly,separator=%x2C)"
                                               (mapcar (lambda (s) (oref s value)) sections)))))
               (ids (seq-filter (lambda (pair) (and (cadr pair) (not (string-empty-p (cadr pair))))) ids))
               (infos (magit-gerrit--get-change-ids-info
                       (mapcar (lambda (pair) (car (split-string (cadr pair) ","))) ids))))
          (cl-loop for section in sections
                   for (_ id) = (seq-find (lambda (pair) (string-prefix-p (car pair) (oref section value)))
                                          ids)
                   for change = (and id (cadr (assoc (car (split-string id ",")) infos)))
                   when change
                   do (let ((overlay (save-excursion
                                       (goto-char (oref section start))
                                       (make-overlay (line-end-position) (line-end-position)))))
                        (overlay-put overlay 'magit-gerrit-annotation t)
                        (overlay-put overlay 'after-string
                                     (concat " " (magit-gerrit--format-change-summary change))))))
      (error (message "magit-gerrit: cannot annotate the log: %s" (error-message-string err))))))

(defun magit-gerrit--read-commit-change (prompt)
  "Return the change of the commit at point, or of a commit read with PROMPT.
When the Change-Id matches several changes (cherry-picks on other
branches), let the user choose."
  (let* ((rev (or (magit-commit-at-point) (magit-read-branch-or-commit prompt)))
         (changes (or (magit-gerrit--commit-changes rev)
                      (user-error "No Gerrit change for %s" rev))))
    (if (length= changes 1)
        (car changes)
      (let ((choices (mapcar (lambda (c) (cons (format "%s %s (%s)" (magit-gerrit-change-number c)
                                                       (magit-gerrit-change-branch c)
                                                       (magit-gerrit-change-status c))
                                               c))
                             changes)))
        (cdr (assoc (completing-read "Change: " choices nil t) choices))))))

(defun magit-gerrit-show-commit-change (change)
  "Show the Gerrit CHANGE of the commit at point."
  (interactive (list (magit-gerrit--read-commit-change "Show change of commit")))
  (magit-gerrit-show-change (magit-gerrit-change-number change)))

(defun magit-gerrit-browse-commit (change)
  "Browse the Gerrit CHANGE of the commit at point."
  (interactive (list (magit-gerrit--read-commit-change "Browse change of commit")))
  (browse-url (magit-gerrit-change-url change)))


;;; Change buffer

(defvar-keymap magit-gerrit-patchset-section-map
  :doc "Keymap for the patchset sections of a Gerrit change buffer."
  "<remap> <magit-visit-thing>" #'magit-gerrit-show-patchset)

(defvar-keymap magit-gerrit-file-section-map
  :doc "Keymap for the file sections of a Gerrit change buffer."
  "<remap> <magit-visit-thing>" #'magit-gerrit-show-patchset-file)

(defvar-keymap magit-gerrit-comment-section-map
  :doc "Keymap for the inline comment sections of a Gerrit change buffer."
  "<remap> <magit-visit-thing>" #'magit-gerrit-show-patchset-file)

(defvar-keymap magit-gerrit-dependency-section-map
  :doc "Keymap for the dependency sections of a Gerrit change buffer."
  "<remap> <magit-visit-thing>" #'magit-gerrit-show-change)

(defvar-keymap magit-gerrit-change-mode-map
  :doc "Keymap for `magit-gerrit-change-mode'."
  :parent magit-mode-map)

(define-derived-mode magit-gerrit-change-mode magit-mode "Gerrit Change"
  "Mode for buffers showing a Gerrit change.

\\<magit-gerrit-change-mode-map>\
Type \\[magit-visit-thing] on a patchset to show it, on a file or an
inline comment to show the diff of that file, on a dependency to show
that change.  Type \\[magit-refresh] to fetch the change again.

\\{magit-gerrit-change-mode-map}"
  :interactive nil
  :group 'magit-gerrit
  ;; Make the Gerrit prefix key available, it is bound in `magit-gerrit-mode-map'
  (set-keymap-parent magit-gerrit-change-mode-map
                     (make-composed-keymap magit-gerrit-mode-map magit-mode-map)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-gerrit-change-mode))
  "Return the number of the change shown in a buffer in MAJOR-MODE."
  magit-gerrit-buffer-change-number)

(defconst magit-gerrit--change-query-options
  '("--current-patch-set" "--patch-sets" "--all-approvals" "--files" "--comments"
    "--commit-message" "--dependencies" "--submit-records" "--all-reviewers")
  "Options of the query fetching a change for the change buffer.")

(defun magit-gerrit--change-number-at-point ()
  "Return the number of the change or dependency at point, or nil."
  (or (magit-section-value-if 'gerrit-dependency)
      (when-let* ((change (magit-section-value-if 'gerrit-change)))
        (magit-gerrit-change-number change))))

;;;###autoload
(defun magit-gerrit-show-change (number)
  "Show the Gerrit change NUMBER in a dedicated buffer.
Interactively, show the change at point or read its number."
  (interactive
   (list (or (magit-gerrit--change-number-at-point)
             (read-number "Change number: "))))
  (let ((creds magit-gerrit-ssh-creds)
        (port magit-gerrit-port)
        (remote magit-gerrit-remote))
    (magit-setup-buffer #'magit-gerrit-change-mode t
      (magit-gerrit-buffer-change-number number)
      (magit-gerrit-ssh-creds creds)
      (magit-gerrit-port port)
      (magit-gerrit-remote remote))))

(defun magit-gerrit-change-refresh-buffer ()
  "Refresh the Gerrit change buffer."
  (magit-set-header-line-format (format "Gerrit change %s" magit-gerrit-buffer-change-number))
  (magit-insert-section (gerrit-change-buffer)
    (condition-case err
        (let ((change (or (car (magit-gerrit--query-changes
                                (format "change:%s" magit-gerrit-buffer-change-number)
                                magit-gerrit--change-query-options))
                          (user-error "Change %s not found" magit-gerrit-buffer-change-number))))
          (setq magit-gerrit-buffer-change change)
          (magit-set-header-line-format
           (format "Gerrit change %s: %s" magit-gerrit-buffer-change-number
                   (magit-gerrit-change-subject change)))
          (magit-run-section-hook 'magit-gerrit-change-sections-hook))
      (error
       (setq magit-gerrit-buffer-change nil)
       (insert (propertize (error-message-string err) 'font-lock-face 'error) "\n")))))

(defun magit-gerrit--format-time (time)
  "Format TIME, in seconds since the epoch, as a date and an age."
  (when time
    (format "%s (%s ago)" (format-time-string "%F %R" time)
            (string-trim (magit-gerrit-format-short-timestring
                          (time-to-seconds (time-since time)))))))

(defun magit-gerrit--format-account (account)
  "Format ACCOUNT, an account attribute of Gerrit's JSON."
  (let ((name (or (alist-get 'name account) (alist-get 'username account) "?"))
        (email (alist-get 'email account)))
    (concat (propertize name 'font-lock-face 'magit-log-author)
            (when email (format " <%s>" email)))))

(defun magit-gerrit--format-vote (value)
  "Format the vote VALUE (a string or a number) with a face."
  (let ((value (if (stringp value) (string-to-number value) value)))
    (propertize (format "%+d" value)
                'font-lock-face (cond ((> value 0) 'success)
                                      ((< value 0) 'error)
                                      (t 'shadow)))))

(defun magit-gerrit--insert-header (name value &optional face)
  "Insert the header NAME with VALUE, propertized with FACE."
  (when (and value (not (equal value "")))
    (insert (format "%-11s" (concat name ":"))
            (if face (propertize (format "%s" value) 'font-lock-face face) (format "%s" value))
            "\n")))

(defun magit-gerrit-insert-change-headers ()
  "Insert the headers of the change."
  (let ((change magit-gerrit-buffer-change))
    (magit-insert-section (gerrit-headers)
      (magit-insert-heading
        (propertize (format "%s %s" (magit-gerrit-change-number change)
                            (magit-gerrit-change-subject change))
                    'font-lock-face 'magit-section-heading))
      (magit-gerrit--insert-header
       "Status" (string-join
                 (delq nil (list (magit-gerrit-change-status change)
                                 (and (magit-gerrit-change-wip-p change) "WIP")
                                 (and (eq (alist-get 'private change) t) "PRIVATE")
                                 (when-let* ((record (car (alist-get 'submitRecords change))))
                                   (format "(submit: %s)" (alist-get 'status record)))))
                 " ")
       'bold)
      (magit-gerrit--insert-header "Project" (magit-gerrit-change-project change))
      (magit-gerrit--insert-header "Branch" (magit-gerrit-change-branch change) 'magit-branch-remote)
      (magit-gerrit--insert-header "Topic" (magit-gerrit-change-topic change) 'magit-tag)
      (magit-gerrit--insert-header "Hashtags" (string-join (alist-get 'hashtags change) ", ") 'magit-tag)
      (magit-gerrit--insert-header "Owner" (magit-gerrit--format-account (alist-get 'owner change)))
      (magit-gerrit--insert-header "Created" (magit-gerrit--format-time (alist-get 'createdOn change)))
      (magit-gerrit--insert-header "Updated" (magit-gerrit--format-time (magit-gerrit-change-last-updated change)))
      (magit-gerrit--insert-header "Change-Id" (magit-gerrit-change-id change) 'magit-hash)
      (magit-gerrit--insert-header "URL" (magit-gerrit-change-url change) 'link)
      (insert "\n"))))

(defun magit-gerrit-insert-change-message ()
  "Insert the commit message of the current patchset."
  (when-let* ((message (magit-gerrit-change-commit-message magit-gerrit-buffer-change)))
    (magit-insert-section (gerrit-commit-message)
      (magit-insert-heading "Commit message")
      (insert (string-trim-right message) "\n\n"))))

(defun magit-gerrit--change-labels (change)
  "Return the names of the labels relevant for CHANGE."
  (seq-uniq
   (append (mapcar #'car magit-gerrit-review-labels)
           (mapcar (lambda (label) (alist-get 'label label))
                   (alist-get 'labels (car (alist-get 'submitRecords change))))
           (mapcar (lambda (approval) (alist-get 'type approval))
                   (magit-gerrit-change-approvals change)))))

(defun magit-gerrit-insert-change-labels ()
  "Insert the votes on the current patchset and the submit requirements."
  (let* ((change magit-gerrit-buffer-change)
         (record (car (alist-get 'submitRecords change)))
         (statuses (alist-get 'labels record))
         (approvals (magit-gerrit-change-approvals change)))
    (magit-insert-section (gerrit-labels)
      (magit-insert-heading "Labels")
      (dolist (label (magit-gerrit--change-labels change))
        (let ((status (alist-get 'status (seq-find (lambda (s) (equal (alist-get 'label s) label))
                                                   statuses)))
              (votes (seq-filter (lambda (a) (equal (alist-get 'type a) label)) approvals)))
          (insert (format "%-16s" label)
                  (propertize (format "%-8s" (or status ""))
                              'font-lock-face (pcase status
                                                ("OK" 'success) ((or "REJECT" "IMPOSSIBLE") 'error)
                                                ("NEED" 'warning) (_ 'shadow)))
                  (mapconcat (lambda (a)
                               (concat (magit-gerrit--format-vote (alist-get 'value a)) " "
                                       (magit-gerrit--format-account (alist-get 'by a))))
                             votes ", ")
                  "\n")))
      (dolist (requirement (alist-get 'requirements record))
        (insert (propertize "Requirement: " 'font-lock-face 'warning)
                (or (alist-get 'fallbackText requirement) "") "\n"))
      (insert "\n"))))

(defun magit-gerrit-insert-change-reviewers ()
  "Insert the reviewers of the change."
  (when-let* ((reviewers (alist-get 'allReviewers magit-gerrit-buffer-change)))
    (magit-insert-section (gerrit-reviewers)
      (magit-insert-heading (format "Reviewers (%d)" (length reviewers)))
      (dolist (reviewer reviewers)
        (insert (magit-gerrit--format-account reviewer) "\n"))
      (insert "\n"))))

(defun magit-gerrit-insert-change-dependencies ()
  "Insert the dependencies and the dependents of the change."
  (let ((change magit-gerrit-buffer-change))
    (pcase-dolist (`(,key . ,title) '((dependsOn . "Depends on") (neededBy . "Needed by")))
      (when-let* ((deps (alist-get key change)))
        (magit-insert-section (gerrit-dependencies key)
          (magit-insert-heading title)
          (dolist (dep deps)
            (magit-insert-section (gerrit-dependency (alist-get 'number dep))
              (insert (propertize (format "%-8s" (alist-get 'number dep)) 'font-lock-face 'magit-hash)
                      (or (alist-get 'ref dep) "")
                      (if (eq (alist-get 'isCurrentPatchSet dep) t)
                          ""
                        (propertize "  (outdated patchset)" 'font-lock-face 'warning))
                      "\n")))
          (insert "\n"))))))

(defun magit-gerrit-insert-change-files ()
  "Insert the files modified by the current patchset."
  (let* ((patchset (magit-gerrit-change-patchset magit-gerrit-buffer-change))
         (files (seq-remove (lambda (f) (string-prefix-p "/" (alist-get 'file f)))
                            (alist-get 'files patchset))))
    (when files
      (magit-insert-section (gerrit-files)
        (magit-insert-heading (format "Files (%d)" (length files)))
        (dolist (file files)
          (magit-insert-section (gerrit-file (cons (alist-get 'file file) patchset))
            (insert (format "%-9s" (downcase (or (alist-get 'type file) "")))
                    (propertize (format "%6s" (format "+%d" (or (alist-get 'insertions file) 0)))
                                'font-lock-face 'diff-added)
                    " "
                    (propertize (format "%-6s" (format "-%d" (abs (or (alist-get 'deletions file) 0))))
                                'font-lock-face 'diff-removed)
                    (propertize (alist-get 'file file) 'font-lock-face 'magit-filename)
                    (if-let* ((old (alist-get 'fileOld file))) (format " (from %s)" old) "")
                    "\n")))
        (insert "\n")))))

(defun magit-gerrit-insert-change-patchsets ()
  "Insert the patchsets of the change."
  (when-let* ((patchsets (alist-get 'patchSets magit-gerrit-buffer-change)))
    (magit-insert-section (gerrit-patchsets)
      (magit-insert-heading (format "Patchsets (%d)" (length patchsets)))
      (dolist (patchset (reverse patchsets))
        (magit-insert-section (gerrit-patchset patchset)
          (insert (propertize (format "%-4s" (alist-get 'number patchset)) 'font-lock-face 'magit-hash)
                  (propertize (substring (or (alist-get 'revision patchset) "") 0 10)
                              'font-lock-face 'magit-hash)
                  " "
                  (propertize (format "%7s" (format "+%d" (or (alist-get 'sizeInsertions patchset) 0)))
                              'font-lock-face 'diff-added)
                  " "
                  (propertize (format "%-7s" (format "-%d" (abs (or (alist-get 'sizeDeletions patchset) 0))))
                              'font-lock-face 'diff-removed)
                  (format "%-17s" (or (alist-get 'kind patchset) ""))
                  (magit-gerrit--format-account (alist-get 'uploader patchset))
                  "  "
                  (propertize (format-time-string "%F %R" (or (alist-get 'createdOn patchset) 0))
                              'font-lock-face 'magit-log-date)
                  "\n")))
      (insert "\n"))))

(defun magit-gerrit-insert-change-messages ()
  "Insert the messages posted on the change."
  (when-let* ((messages (alist-get 'comments magit-gerrit-buffer-change)))
    (magit-insert-section (gerrit-messages)
      (magit-insert-heading (format "Messages (%d)" (length messages)))
      (dolist (message messages)
        (magit-insert-section (gerrit-message message)
          (magit-insert-heading
            (magit-gerrit--format-account (alist-get 'reviewer message))
            "  "
            (propertize (format-time-string "%F %R" (or (alist-get 'timestamp message) 0))
                        'font-lock-face 'magit-log-date))
          (insert (replace-regexp-in-string
                   "^" "  " (string-trim-right (or (alist-get 'message message) "")))
                  "\n")))
      (insert "\n"))))

(defun magit-gerrit-insert-change-comments ()
  "Insert the inline comments of all the patchsets."
  (let ((patchsets (seq-filter (lambda (ps) (alist-get 'comments ps))
                               (alist-get 'patchSets magit-gerrit-buffer-change))))
    (when patchsets
      (magit-insert-section (gerrit-comments)
        (magit-insert-heading "Inline comments")
        (dolist (patchset patchsets)
          (magit-insert-section (gerrit-patchset-comments (alist-get 'number patchset))
            (magit-insert-heading (format "Patchset %s" (alist-get 'number patchset)))
            (dolist (comment (alist-get 'comments patchset))
              (magit-insert-section (gerrit-comment (cons (alist-get 'file comment) patchset))
                (magit-insert-heading
                  (propertize (format "%s:%s" (alist-get 'file comment) (or (alist-get 'line comment) ""))
                              'font-lock-face 'magit-filename)
                  "  "
                  (magit-gerrit--format-account (alist-get 'reviewer comment)))
                (insert (replace-regexp-in-string
                         "^" "  " (string-trim-right (or (alist-get 'message comment) "")))
                        "\n")))))
        (insert "\n")))))

(defun magit-gerrit--fetch-revision (patchset)
  "Fetch PATCHSET if needed and return its revision."
  (let ((revision (alist-get 'revision patchset)))
    (unless (magit-commit-p revision)
      (magit-gerrit-fetch-patchset (alist-get 'ref patchset)))
    revision))

(defun magit-gerrit-show-patchset (patchset)
  "Show the commit of PATCHSET, fetching it if needed."
  (interactive (list (or (magit-gerrit-patchset-at-point)
                         (magit-gerrit-change-patchset
                          (magit-gerrit--change-at-point-or-error)))))
  (magit-show-commit (magit-gerrit--fetch-revision patchset)))

(defun magit-gerrit-show-patchset-file (file patchset)
  "Show the diff of FILE in PATCHSET, fetching it if needed."
  (interactive (let ((value (or (magit-section-value-if '(gerrit-file gerrit-comment))
                                (user-error "No file at point"))))
                 (list (car value) (cdr value))))
  (let ((revision (magit-gerrit--fetch-revision patchset)))
    (magit-diff-range (format "%s^..%s" revision revision) nil (list file))))


;;; Transient

;;;###autoload (autoload 'magit-gerrit-dispatch "magit-gerrit" nil t)
(transient-define-prefix magit-gerrit-dispatch ()
  "Transient menu for `magit-gerrit' commands."
  ["Arguments"
   ("-m" "Message"                         "--message=")
   ("-n" "Notify"                          "--notify="
    :choices ("NONE" "OWNER" "OWNER_REVIEWERS" "ALL"))
   ("-s" "Submit after voting"             "--submit")]
  [["Change"
    ("RET" "Show"                          magit-gerrit-show-change)
    ("b" "Browse"                          magit-gerrit-browse-review)
    ("v" "Vote"                            magit-gerrit-vote)
    ("S" "Submit"                          magit-gerrit-submit-review)
    ("u" "Rebase"                          magit-gerrit-rebase-review)
    ("M" "Move to branch"                  magit-gerrit-move-review)
    ("B" "Abandon"                         magit-gerrit-abandon-review)
    ("R" "Restore"                         magit-gerrit-restore-review)]
   ["Reviewers & state"
    ("A" "Add reviewers"                   magit-gerrit-add-reviewer)
    ("K" "Remove reviewers"                magit-gerrit-remove-reviewer)
    ("c" "Add CC"                          magit-gerrit-add-cc)
    ("T" "Set topic"                       magit-gerrit-set-topic)
    ("w" "Mark work in progress"           magit-gerrit-mark-wip)
    ("W" "Mark ready for review"           magit-gerrit-mark-ready)]
   ["Patchset"
    ("d" "View diff"                       magit-gerrit-view-patchset-diff)
    ("D" "Download (checkout)"             magit-gerrit-download-patchset)
    ("F" "Cherry-pick"                     magit-gerrit-cherry-pick-patchset)]]
  [["Others"
    ("P" "Push for review"                 magit-gerrit-push)
    ("/" "Query"                           magit-gerrit-query)
    ("H" "Install commit-msg hook"         magit-gerrit-install-commit-msg-hook)]
   ["Commit at point"
    ("o" "Show its change"                 magit-gerrit-show-commit-change)
    ("O" "Browse its change"               magit-gerrit-browse-commit)]
   [("y" "Copy review URL"                 magit-gerrit-copy-review-url)
    ("Y" "Copy review URL and subject"     magit-gerrit-copy-review-url-commit-message)
    ("r" "Copy review reference"           magit-gerrit-copy-ref)]])


;;; Minor mode

;;;###autoload
(define-minor-mode magit-gerrit-mode
  "Gerrit support for Magit.

Add the Gerrit reviews section to the status buffer and bind
`magit-gerrit-dispatch' to `magit-gerrit-popup-prefix'.

\\{magit-gerrit-mode-map}"
  :lighter " Gerrit"
  :keymap magit-gerrit-mode-map
  :group 'magit-gerrit
  (if (not magit-gerrit-mode)
      (progn
        (remove-hook 'magit-status-sections-hook #'magit-gerrit-insert-reviews-section t)
        (remove-hook 'magit-refresh-buffer-hook #'magit-gerrit-annotate-log t)
        (remove-overlays (point-min) (point-max) 'magit-gerrit-annotation t))
    (when-let* ((err (cond
                      ((not (derived-mode-p 'magit-mode))
                       "This mode only makes sense with magit")
                      ((not (magit-gerrit-get-remote-url))
                       "You can set `magit-gerrit-remote' to a valid Gerrit remote")
                      ((not (magit-gerrit-detect-ssh-creds (magit-gerrit-get-remote-url)))
                       "You can set `magit-gerrit-ssh-creds' to enable magit-gerrit-mode"))))
      (setq magit-gerrit-mode nil)
      (user-error "%s" err))
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-gerrit-insert-reviews-section
                            'magit-insert-stashes t t)
    (when (derived-mode-p 'magit-log-mode)
      (add-hook 'magit-refresh-buffer-hook #'magit-gerrit-annotate-log nil t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;; Show the Gerrit transient in `magit-dispatch' when the mode is enabled
(unless (ignore-errors (transient-get-suffix 'magit-dispatch 'magit-gerrit-dispatch))
  (ignore-errors
    (transient-append-suffix 'magit-dispatch "z"
      `(,(key-description (magit-gerrit--key magit-gerrit-popup-prefix)) "Gerrit" magit-gerrit-dispatch
        :if-non-nil magit-gerrit-mode))))

(defun magit-gerrit-check-enable ()
  "Enable `magit-gerrit-mode' when the repository is hosted on Gerrit."
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
               (or magit-gerrit-force-enable
                   (and (magit-gerrit-detect-ssh-creds remote-url)
                        (string-search magit-gerrit-ssh-creds remote-url))))
      (magit-gerrit-mode 1))))

;;;###autoload
(defun magit-gerrit-auto-enable ()
  "Try to auto enable magit-gerrit in the `magit-status' buffer."
  (interactive)
  (add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
  (add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t)
  (add-hook 'magit-revision-mode-hook #'magit-gerrit-check-enable t))

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
