;;; magit-gerrit.el --- Magit plugin for Gerrit Code Review  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Brian Fransioli
;; Copyright (C) 2025-2026 Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa
;; Author: Brian Fransioli
;; URL: https://github.com/abougouffa/magit-gerrit
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (transient "0.5.0"))
;; Package-Version: 0.3.0

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

;; Magit plugin for Gerrit Code Review: review, vote, comment, push and
;; manage changes from Emacs, through Gerrit's SSH interface or its REST
;; API.
;;
;; - A "Reviews" section in `magit-status', with query presets.
;; - A buffer showing the details of a change: labels, submit
;;   requirements, reviewers, dependencies, files, patchsets, messages
;;   and inline comments (`magit-gerrit-show-change').
;; - Votes on any label, submit, abandon, restore, rebase, move, and
;;   management of reviewers, CCs, topics and the WIP state.
;; - Inline comments on the lines of a patchset; with the REST API,
;;   replies, resolution of comment threads and draft comments.
;; - With the REST API: hashtags, stars, private changes, edition of
;;   the commit message, reverts and cherry-picks on the server.
;; - A push menu with Gerrit's push options (`magit-gerrit-push').
;; - Viewing, checking out and cherry-picking patchsets.
;; - The change of commits in revision and log buffers.
;; - Installation of the commit-msg hook and browsing in Gitiles.
;;
;; Setup:
;;
;;   (with-eval-after-load 'magit
;;     (require 'magit-gerrit)
;;     (magit-gerrit-auto-enable))
;;
;; This enables `magit-gerrit-mode' in the Magit buffers of
;; repositories whose remote ("origin" or "gerrit", see
;; `magit-gerrit-remote') is an SSH URL on Gerrit's port, such as
;; ssh://john@review.example.com:29418/project, or an HTTPS URL of a
;; Gerrit server listed in `magit-gerrit-rest-hosts'.  Otherwise, set
;; `magit-gerrit-ssh-creds' or `magit-gerrit-rest-url' (and
;; `magit-gerrit-force-enable') in a .dir-locals.el file.
;;
;; Backends: the frontend (this file) talks to Gerrit through a backend,
;; chosen with `magit-gerrit-backend'.  The SSH backend
;; (magit-gerrit-ssh.el) runs Gerrit's SSH commands, the REST backend
;; (magit-gerrit-rest.el) uses its REST API, which offers more features
;; (replies to comments, drafts, hashtags...).  The commands needing a
;; feature the backend lacks are shown as unavailable in the menus.
;;
;; Press "_" (`magit-gerrit-popup-prefix') in a Magit buffer to open the
;; Gerrit menu.  A typical workflow:
;;
;; 1) commit ("_ H" installs the hook adding Change-Ids to commits),
;; 2) "_ P p" pushes the commit at point for review,
;; 3) "_ A" adds reviewers,
;; 4) RET on a change in the Reviews section shows its comments,
;;    "_ C" comments on the line at point of a patchset diff and "_ a"
;;    replies to the comment at point,
;; 5) "_ v" votes and "_ S" submits.
;;
;; See the README for the full documentation.

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

(defcustom magit-gerrit-backend 'auto
  "The backend used to talk to Gerrit.
`ssh' runs the commands of Gerrit's SSH daemon (see
magit-gerrit-ssh.el), `rest' uses Gerrit's REST API over HTTP(S),
which offers more features (see magit-gerrit-rest.el).  `auto' uses
the first backend of `magit-gerrit-auto-backends' that matches the
remote of the repository.  Other backends can be added with
`magit-gerrit-register-backend'."
  :group 'magit-gerrit
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "SSH commands" ssh)
                 (const :tag "REST API" rest)
                 (symbol :tag "Other backend")))
(put 'magit-gerrit-backend 'safe-local-variable #'symbolp)

(defcustom magit-gerrit-auto-backends '(rest ssh)
  "Backends tried, in order, when `magit-gerrit-backend' is `auto'.
The first one serving the remote of the repository is used, the REST
backend is only used when it is configured (see
`magit-gerrit-rest-url' and `magit-gerrit-rest-hosts')."
  :group 'magit-gerrit
  :type '(repeat symbol))

(defcustom magit-gerrit-read-message-function #'magit-gerrit-read-message-in-buffer
  "Function reading the text of comments and of commit messages.
It is called with a PROMPT, a CONTEXT shown above the text (or nil),
the INITIAL text (or nil) and a CALLBACK, which it calls with the text
when it is done (it doesn't call it when the input is cancelled)."
  :group 'magit-gerrit
  :type '(choice (function-item magit-gerrit-read-message-in-buffer)
                 (function-item magit-gerrit-read-message-in-minibuffer)
                 function))

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
scp protocol, Gerrit doesn't support SFTP).  The methods are
implemented by the functions of `magit-gerrit-commit-msg-hook-downloaders'."
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
`magit-gerrit-default-label-values'.  The REST backend gets the values
of the labels from the server, they take precedence."
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
    magit-gerrit-insert-change-comments
    magit-gerrit-insert-change-drafts)
  "Hook run to insert sections into a Gerrit change buffer."
  :group 'magit-gerrit
  :type 'hook)


;;; Per-repository variables

(defvar-local magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\").")

(defvar magit-gerrit-fallback-remote "gerrit"
  "Fallback remote name to use for gerrit.")

(defvar-local magit-gerrit-force-enable nil
  "Force enabling `magit-gerrit' for the project.")

;; https://gerrit-review.googlesource.com/Documentation/user-search.html
;; For example we can set it to: "(status:open OR (status:merged AND -age:6months))"
(defvar-local magit-gerrit-query-filters nil
  "Query used to list the reviews, defaults to \"status:open\" when nil.
See https://gerrit-review.googlesource.com/Documentation/user-search.html")

(defvar-local magit-gerrit-query-all-projects nil
  "When non-nil, list the reviews of all projects, not only the current one.")

(put 'magit-gerrit-remote 'safe-local-variable #'stringp)
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
  "Name of the buffer logging the Gerrit commands and requests.")

(defvar magit-gerrit-reviewer-history nil
  "Minibuffer history of reviewers and CCs.")
(add-to-list 'savehist-additional-variables 'magit-gerrit-reviewer-history)


;;; Logging and JSON

(defun magit-gerrit--log (command status &optional details)
  "Log the Gerrit COMMAND (a string), its STATUS and its DETAILS.
STATUS is the exit code of a command or the HTTP status of a request,
DETAILS is the error output or the error message, if any.  The log
is in `magit-gerrit-log-buffer-name'."
  (with-current-buffer (get-buffer-create magit-gerrit-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] %s => %s\n" (format-time-string "%F %T") command status))
      (when (and details (not (string-empty-p details)))
        (insert details "\n")))))

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

(defun magit-gerrit--json-encode (object)
  "Encode OBJECT as compact JSON with `json-encode'.
Alists are encoded as objects, vectors as arrays, t as true,
`:json-false' as false and an empty hash table as {}."
  (let ((json-encoding-pretty-print nil))
    (json-encode object)))


;;; Backends

;; The frontend talks to Gerrit through a backend object, whose type
;; derives from `magit-gerrit-backend'.  The backends implement the
;; generic functions below (the `magit-gerrit-backend-' ones) and
;; declare what they support with `magit-gerrit-backend-features'.  The
;; operations a backend doesn't implement signal a `user-error'; the
;; frontend checks the features before offering a command (see
;; `magit-gerrit--supports-p'), so that the backends can have different
;; capabilities.
;;
;; The backends exchange changes with the frontend in the format of
;; the JSON of "gerrit query" (see "Change data model" below).

(cl-defstruct (magit-gerrit-backend (:constructor nil) (:copier nil))
  "Base type of the backends talking to a Gerrit server.")

(defvar magit-gerrit-backends nil
  "The available backends, as an alist of (NAME . CONSTRUCTOR).
CONSTRUCTOR is called with the URL of the Gerrit remote (or nil) and
EXPLICIT, non-nil when the backend was chosen with
`magit-gerrit-backend'.  It returns a backend, or nil when it can't be
used for the repository.  See `magit-gerrit-register-backend'.")

(defvar magit-gerrit-backend-variables '(magit-gerrit-backend magit-gerrit-remote)
  "Variables configuring the backends.
Their values are passed on to the change buffers, and the backend of a
buffer is made again when one of them changes.")

(defun magit-gerrit-register-backend (name constructor &rest variables)
  "Register the backend NAME, made by CONSTRUCTOR.
See `magit-gerrit-backends' for CONSTRUCTOR.  VARIABLES are the
variables configuring the backend, see
`magit-gerrit-backend-variables'."
  (setf (alist-get name magit-gerrit-backends) constructor)
  (dolist (var variables)
    (add-to-list 'magit-gerrit-backend-variables var t)))

(defconst magit-gerrit-query-options
  '(:current-patch-set :patch-sets :approvals :files :comments :commit-message
    :dependencies :submit-records :reviewers)
  "The options of queries, telling which data the changes include.
:current-patch-set  the current patchset, with its votes and size,
:patch-sets         all the patchsets,
:approvals          the votes on all the patchsets,
:files              the files modified by the patchsets,
:comments           the messages and the inline comments,
:commit-message     the commit message of the current patchset,
:dependencies       the changes it depends on and its dependents,
:submit-records     whether the change can be submitted,
:reviewers          the reviewers.")

(defconst magit-gerrit-features
  '((query . "queries")
    (async . "asynchronous queries")
    (vote . "votes")
    (submit . "submitting changes")
    (abandon . "abandoning changes")
    (restore . "restoring changes")
    (rebase . "rebasing changes")
    (move . "moving changes")
    (topic . "topics")
    (reviewers . "reviewers")
    (cc . "CCs")
    (wip . "the work in progress state")
    (comment . "inline comments")
    (reply . "replies to comments")
    (drafts . "draft comments")
    (label-values . "the values of the labels")
    (suggest-accounts . "suggestions of accounts")
    (hashtags . "hashtags")
    (star . "starred changes")
    (private . "private changes")
    (edit-message . "editing commit messages")
    (revert . "reverting changes")
    (cherry-pick . "cherry-picking changes on the server"))
  "The features a backend can support, with their descriptions.
See `magit-gerrit-backend-features'.")

(defun magit-gerrit--unsupported (backend feature)
  "Signal that BACKEND doesn't support FEATURE."
  (user-error "The %s backend doesn't support %s"
              (magit-gerrit-backend-name backend)
              (alist-get feature magit-gerrit-features feature)))

(cl-defgeneric magit-gerrit-backend-name (backend)
  "Return the name of BACKEND, for messages."
  (symbol-name (type-of backend)))

(cl-defgeneric magit-gerrit-backend-key (backend)
  "Return a string identifying the server of BACKEND, for caches."
  (format "%S" backend))

(cl-defgeneric magit-gerrit-backend-features (_backend)
  "Return the list of the features of BACKEND.
The features are the keys of `magit-gerrit-features'."
  nil)

(cl-defgeneric magit-gerrit-backend-matches-remote-p (_backend _remote-url)
  "Return non-nil when BACKEND is the server of the Git REMOTE-URL.
This decides whether `magit-gerrit-mode' is enabled automatically."
  nil)

(cl-defgeneric magit-gerrit-backend-web-url (_backend)
  "Return the base URL of the web interface of BACKEND, or nil."
  nil)

(cl-defgeneric magit-gerrit-backend-query (backend query options)
  "Run QUERY on BACKEND, return the list of matching change objects.
QUERY is a string or a list of query terms, see
https://gerrit-review.googlesource.com/Documentation/user-search.html
OPTIONS are keywords of `magit-gerrit-query-options', telling which
data the changes must include."
  (:method (backend _query _options) (magit-gerrit--unsupported backend 'query)))

(cl-defgeneric magit-gerrit-backend-query-async (backend query options callback)
  "Run QUERY with OPTIONS on BACKEND in the background.
See `magit-gerrit-backend-query' for QUERY and OPTIONS.  CALLBACK is
called with the list of changes and nil, or with nil and an error
message.  The default method runs the query when Emacs is idle."
  (run-at-time 0 nil
               (lambda ()
                 (condition-case err
                     (funcall callback (magit-gerrit-backend-query backend query options) nil)
                   (error (funcall callback nil (error-message-string err))))))
  nil)

(cl-defgeneric magit-gerrit-backend-get-change (backend number)
  "Return the change NUMBER with all its details from BACKEND, or nil.
The default method queries it with all the
`magit-gerrit-query-options'."
  (car (magit-gerrit-backend-query backend (format "change:%s" number)
                                   magit-gerrit-query-options)))

(cl-defgeneric magit-gerrit-backend-vote (backend change votes options)
  "Vote VOTES on the current patchset of CHANGE with BACKEND.
VOTES is a list of \"LABEL=VALUE\" strings.  OPTIONS is a plist with
the properties :message (posted with the votes), :notify (who to
notify) and :submit (submit after voting)."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'vote)))

(cl-defgeneric magit-gerrit-backend-submit (backend change options)
  "Submit CHANGE with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'submit)))

(cl-defgeneric magit-gerrit-backend-abandon (backend change options)
  "Abandon CHANGE with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'abandon)))

(cl-defgeneric magit-gerrit-backend-restore (backend change options)
  "Restore the abandoned CHANGE with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'restore)))

(cl-defgeneric magit-gerrit-backend-rebase (backend change options)
  "Rebase CHANGE on Gerrit with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'rebase)))

(cl-defgeneric magit-gerrit-backend-move (backend change branch options)
  "Move CHANGE to BRANCH with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'move)))

(cl-defgeneric magit-gerrit-backend-set-topic (backend change topic)
  "Set the TOPIC of CHANGE with BACKEND, remove it when TOPIC is nil."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'topic)))

(cl-defgeneric magit-gerrit-backend-add-reviewers (backend change accounts &optional cc)
  "Add ACCOUNTS as reviewers of CHANGE with BACKEND.
When CC is non-nil, add them in CC instead."
  (:method (backend _change _accounts &optional cc)
           (magit-gerrit--unsupported backend (if cc 'cc 'reviewers))))

(cl-defgeneric magit-gerrit-backend-remove-reviewers (backend change accounts)
  "Remove ACCOUNTS from the reviewers (or CCs) of CHANGE with BACKEND."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'reviewers)))

(cl-defgeneric magit-gerrit-backend-set-wip (backend change wip options)
  "Mark CHANGE as work in progress when WIP is non-nil, as ready otherwise.
BACKEND is the backend, OPTIONS a plist with the property :message."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'wip)))

(cl-defgeneric magit-gerrit-backend-post-comments (backend change revision comments options)
  "Post inline COMMENTS on REVISION of CHANGE with BACKEND.
REVISION is a commit hash.  COMMENTS is a list of plists with the
properties :file, :line (nil for a file comment), :side
\(\"REVISION\" or \"PARENT\"), :message, :unresolved and :in-reply-to
\(the id of the comment replied to).  OPTIONS is a plist with the
properties :message (a message posted with the comments), :notify
and :draft (save the comments as drafts instead of posting them)."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'comment)))

(cl-defgeneric magit-gerrit-backend-update-draft (backend change draft message unresolved)
  "Set the MESSAGE and the UNRESOLVED state of the DRAFT of CHANGE.
BACKEND is the backend, DRAFT a draft comment of the change."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'drafts)))

(cl-defgeneric magit-gerrit-backend-delete-draft (backend change draft)
  "Delete the DRAFT comment of CHANGE with BACKEND."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'drafts)))

(cl-defgeneric magit-gerrit-backend-publish-drafts (backend change options)
  "Publish the draft comments of CHANGE with BACKEND.
OPTIONS is a plist with the properties :message and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'drafts)))

(cl-defgeneric magit-gerrit-backend-set-hashtags (backend change add remove)
  "Add the hashtags ADD to CHANGE and remove the hashtags REMOVE.
BACKEND is the backend."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'hashtags)))

(cl-defgeneric magit-gerrit-backend-set-starred (backend change starred)
  "Star CHANGE with BACKEND when STARRED is non-nil, unstar it otherwise."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'star)))

(cl-defgeneric magit-gerrit-backend-set-private (backend change private options)
  "Mark CHANGE as private when PRIVATE is non-nil, as public otherwise.
BACKEND is the backend, OPTIONS a plist with the property :message."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'private)))

(cl-defgeneric magit-gerrit-backend-set-commit-message (backend change message options)
  "Set the commit MESSAGE of CHANGE with BACKEND, creating a patchset.
OPTIONS is a plist with the property :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'edit-message)))

(cl-defgeneric magit-gerrit-backend-revert (backend change options)
  "Create a change reverting CHANGE with BACKEND and return it.
OPTIONS is a plist with the properties :message (the commit message
of the revert) and :notify."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'revert)))

(cl-defgeneric magit-gerrit-backend-cherry-pick (backend change revision branch options)
  "Cherry-pick REVISION of CHANGE to BRANCH on the server with BACKEND.
Return the created change.  OPTIONS is a plist with the property
:message (the commit message of the cherry-pick)."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'cherry-pick)))

(cl-defgeneric magit-gerrit-backend-suggest-accounts (backend query &optional change)
  "Return the accounts matching QUERY, as strings, using BACKEND.
When CHANGE is non-nil, suggest reviewers for it."
  (:method (backend &rest _) (magit-gerrit--unsupported backend 'suggest-accounts)))

(defvar-local magit-gerrit--backend nil
  "The backend of the buffer, as (FINGERPRINT . BACKEND).
See `magit-gerrit-current-backend'.")

(defun magit-gerrit--backend-fingerprint ()
  "Return the values the backend of the current buffer depends on."
  (cons default-directory
        (mapcar (lambda (var) (and (boundp var) (symbol-value var)))
                magit-gerrit-backend-variables)))

(defun magit-gerrit--make-backend (name remote-url explicit)
  "Make the backend NAME for REMOTE-URL, return nil if it can't be used.
EXPLICIT is non-nil when the backend was chosen explicitly."
  (if-let* ((constructor (alist-get name magit-gerrit-backends)))
      (funcall constructor remote-url explicit)
    (when explicit
      (user-error "Unknown Gerrit backend `%s', see `magit-gerrit-backend'" name))))

(defun magit-gerrit--find-backend ()
  "Return the backend for the current repository, see `magit-gerrit-backend'.
In `auto' mode, the first backend matching the remote is used, or
else the first one that can be used."
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (if (memq magit-gerrit-backend '(nil auto))
        (let ((backends (delq nil (mapcar (lambda (name) (magit-gerrit--make-backend name remote-url nil))
                                          magit-gerrit-auto-backends))))
          (or (seq-find (lambda (backend) (magit-gerrit-backend-matches-remote-p backend remote-url))
                        backends)
              (car backends)))
      (magit-gerrit--make-backend magit-gerrit-backend remote-url t))))

(defun magit-gerrit-current-backend (&optional noerror)
  "Return the Gerrit backend of the current buffer.
The backend is chosen with `magit-gerrit-backend', it is cached in the
buffer.  When there is none, signal a `user-error', or return nil
when NOERROR is non-nil."
  (or (and (equal (car magit-gerrit--backend) (magit-gerrit--backend-fingerprint))
           (cdr magit-gerrit--backend))
      (when-let* ((backend (magit-gerrit--find-backend)))
        ;; The constructor may have set variables, take them into account
        (setq-local magit-gerrit--backend (cons (magit-gerrit--backend-fingerprint) backend))
        backend)
      (unless noerror
        (user-error "No Gerrit backend for this repository, see `magit-gerrit-backend'"))))

(defun magit-gerrit--supports-p (feature)
  "Return non-nil when the backend of the current buffer supports FEATURE."
  (when-let* ((backend (ignore-errors (magit-gerrit-current-backend t))))
    (memq feature (magit-gerrit-backend-features backend))))

(defun magit-gerrit--require (feature)
  "Return the backend of the current buffer, check that it supports FEATURE."
  (let ((backend (magit-gerrit-current-backend)))
    (unless (memq feature (magit-gerrit-backend-features backend))
      (magit-gerrit--unsupported backend feature))
    backend))

(defun magit-gerrit--inherited-values ()
  "Return the values of `magit-gerrit-backend-variables' as an alist.
The backend of the current buffer is included, so that the buffers
showing Gerrit data share it."
  (cons (cons 'magit-gerrit--backend magit-gerrit--backend)
        (seq-keep (lambda (var) (when (boundp var) (cons var (symbol-value var))))
                  magit-gerrit-backend-variables)))

(defun magit-gerrit--inherit-values (values)
  "Set the buffer-local VALUES, as returned by `magit-gerrit--inherited-values'."
  (pcase-dolist (`(,var . ,value) values)
    (set (make-local-variable var) value)))


;;; Backend inputs

(defun magit-gerrit--review-options (args &rest extra)
  "Return the plist of options of the transient ARGS, plus EXTRA.
These are the :message and :notify properties."
  (append
   (when-let* ((msg (transient-arg-value "--message=" args)))
     (list :message msg))
   (when-let* ((notify (transient-arg-value "--notify=" args)))
     (list :notify notify))
   extra))

(defun magit-gerrit--reviewers-input (accounts state)
  "Return a ReviewInput adding ACCOUNTS as reviewers in STATE.
STATE is \"REVIEWER\" or \"CC\"."
  `((reviewers . ,(vconcat (mapcar (lambda (account) `((reviewer . ,account) (state . ,state)))
                                   accounts)))))

(defun magit-gerrit--comment-input (comment)
  "Return the CommentInput entity of COMMENT, a plist.
See `magit-gerrit-backend-post-comments' for its properties."
  (append
   (when-let* ((line (plist-get comment :line))) `((line . ,line)))
   (when (equal (plist-get comment :side) "PARENT") '((side . "PARENT")))
   (when-let* ((id (plist-get comment :in-reply-to))) `((in_reply_to . ,id)))
   `((message . ,(plist-get comment :message))
     (unresolved . ,(if (plist-get comment :unresolved) t :json-false)))))

(defun magit-gerrit--comments-input (comments)
  "Return the \"comments\" map of a ReviewInput entity for COMMENTS.
COMMENTS are plists, see `magit-gerrit-backend-post-comments'."
  (let (map)
    (dolist (comment comments)
      (push (magit-gerrit--comment-input comment)
            (alist-get (plist-get comment :file) map nil nil #'equal)))
    (mapcar (lambda (entry) (cons (car entry) (vconcat (nreverse (cdr entry)))))
            (nreverse map))))


;;; Queries

(defvar magit-gerrit--known-accounts (make-hash-table :test #'equal)
  "Accounts seen in query results, offered when reading reviewers.")

(defun magit-gerrit--account-string (account)
  "Return the string identifying ACCOUNT for Gerrit, or nil."
  (or (alist-get 'email account) (alist-get 'username account)
      (when-let* ((id (alist-get 'accountId account))) (format "%s" id))))

(defun magit-gerrit--remember-accounts (change)
  "Remember the accounts (owner, reviewers, voters) involved in CHANGE."
  (dolist (account (append (list (alist-get 'owner change))
                           (alist-get 'allReviewers change)
                           (alist-get 'ccs change)
                           (mapcar (lambda (a) (alist-get 'by a))
                                   (magit-gerrit--get change 'currentPatchSet 'approvals))))
    (when-let* ((id (or (alist-get 'email account) (alist-get 'username account))))
      (puthash id t magit-gerrit--known-accounts))))

(defun magit-gerrit--query-changes (query &rest options)
  "Run the Gerrit QUERY with OPTIONS, return the matching change list.
QUERY is a string or a list of query terms, OPTIONS are keywords of
`magit-gerrit-query-options' such as :current-patch-set."
  (let ((changes (magit-gerrit-backend-query (magit-gerrit--require 'query) query options)))
    (mapc #'magit-gerrit--remember-accounts changes)
    changes))

(defun magit-gerrit--get-change (number)
  "Return the change NUMBER with all its details, or nil."
  (when-let* ((change (magit-gerrit-backend-get-change (magit-gerrit--require 'query) number)))
    (magit-gerrit--remember-accounts change)
    change))

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

(defconst magit-gerrit--reviews-query-options '(:current-patch-set :submit-records)
  "Options of the query listing the reviews.")

(defun magit-gerrit--fetch-reviews ()
  "Fetch the reviews of the current project."
  (when-let* ((terms (magit-gerrit--query-terms)))
    (apply #'magit-gerrit--query-changes terms magit-gerrit--reviews-query-options)))

(defvar magit-gerrit--async-cache (make-hash-table :test #'equal)
  "Reviews fetched asynchronously.
Keys are (TOPLEVEL SERVER QUERY-TERMS), values are plists with the
properties :time (of the last completed fetch), :changes, :error and
:pending (non-nil while a fetch is running).")

(defun magit-gerrit--async-refresh-status (toplevel)
  "Refresh the status buffers of the repository TOPLEVEL.
Synchronous requests (such as those of the REST backend) process the
events while waiting, so this can be called while a buffer is being
refreshed: the refresh is then postponed."
  (if (or (bound-and-true-p magit-insert-section--parent)
          (bound-and-true-p magit--refreshing-buffer-p))
      (run-at-time 0.1 nil #'magit-gerrit--async-refresh-status toplevel)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'magit-status-mode)
                   (equal (magit-toplevel) toplevel))
          (magit-refresh-buffer))))))

(defun magit-gerrit--async-fetch (backend key terms)
  "Fetch the reviews matching TERMS with BACKEND in the background.
Store them in `magit-gerrit--async-cache' at KEY."
  (puthash key (plist-put (copy-sequence (gethash key magit-gerrit--async-cache)) :pending t)
           magit-gerrit--async-cache)
  (condition-case err
      (magit-gerrit-backend-query-async
       backend terms magit-gerrit--reviews-query-options
       (lambda (changes error)
         (let ((entry (gethash key magit-gerrit--async-cache)))
           (mapc #'magit-gerrit--remember-accounts changes)
           (puthash key (list :time (current-time)
                              :changes (if error (plist-get entry :changes) changes)
                              :error error)
                    magit-gerrit--async-cache)
           (magit-gerrit--async-refresh-status (car key)))))
    (error
     (puthash key (list :time (current-time) :error (error-message-string err))
              magit-gerrit--async-cache))))

(defun magit-gerrit--async-reviews ()
  "Return the cached reviews, fetching them in the background if needed.
Return a plist with the :changes, :error and :pending properties."
  (when-let* ((terms (magit-gerrit--query-terms)))
    (let* ((backend (magit-gerrit-current-backend))
           (key (list (magit-toplevel) (magit-gerrit-backend-key backend) terms))
           (entry (gethash key magit-gerrit--async-cache))
           (time (plist-get entry :time)))
      (unless (or (plist-get entry :pending)
                  (and time (< (float-time (time-since time)) magit-gerrit-async-min-interval)))
        (if (memq 'async (magit-gerrit-backend-features backend))
            (magit-gerrit--async-fetch backend key terms)
          (puthash key (condition-case err
                           (list :time (current-time) :changes (magit-gerrit--fetch-reviews))
                         (error (list :time (current-time) :changes (plist-get entry :changes)
                                      :error (error-message-string err))))
                   magit-gerrit--async-cache)))
      (gethash key magit-gerrit--async-cache))))

(defun magit-gerrit-reset-backend ()
  "Forget the backend of the current buffer and the cached data.
The backend is made again, from the current configuration, the next
time it is needed."
  (interactive)
  (kill-local-variable 'magit-gerrit--backend)
  (clrhash magit-gerrit--change-id-cache)
  (clrhash magit-gerrit--async-cache)
  (when (derived-mode-p 'magit-mode)
    (magit-refresh)))


;;; Change data model

;; The backends return changes in the format of the objects returned by
;; "gerrit query --format=JSON", parsed as alists (symbol keys, arrays
;; as lists, null and false as nil, see `magit-gerrit--json-parse').
;; Their structure is documented in
;; https://gerrit-review.googlesource.com/Documentation/json.html
;; Any attribute may be missing, the frontend shows what is available.
;;
;; Backends can provide more data with these optional attributes:
;;
;; - In the comments of the patchsets (patchSets[].comments): `id',
;;   `inReplyTo' (the id of the comment replied to), `unresolved',
;;   `updated' (seconds since the epoch), `side' ("PARENT" for the base
;;   of the patchset) and `patchSet' (its number).
;; - `drafts': the draft comments of the user, like the comments above,
;;   with `patchSet' and `revision'.
;; - `labelRanges': the values of the labels, an alist of
;;   (LABEL MIN . MAX) with LABEL a string.
;; - `permittedLabels': the votes the user can give, an alist of
;;   (LABEL VALUE...).
;; - `submitRequirements': a list of alists with `name' and `status'.
;; - `ccs': the accounts in CC, `starred': non-nil when the user starred
;;   the change.
;; - Accounts may have an `accountId'.

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

(defun magit-gerrit-change-hashtags (change)
  "Return the hashtags of CHANGE."
  (magit-gerrit--get change 'hashtags))

(defun magit-gerrit-change-private-p (change)
  "Return non-nil when CHANGE is private."
  (eq (magit-gerrit--get change 'private) t))

(defun magit-gerrit-change-starred-p (change)
  "Return non-nil when the user starred CHANGE (if the backend knows it)."
  (eq (magit-gerrit--get change 'starred) t))

(defun magit-gerrit-change-drafts (change)
  "Return the draft comments of the user on CHANGE."
  (magit-gerrit--get change 'drafts))

(defun magit-gerrit-change-find-patchset (change revision)
  "Return the patchset of CHANGE whose commit is REVISION, or nil."
  (seq-find (lambda (ps) (equal (alist-get 'revision ps) revision))
            (cons (magit-gerrit-change-patchset change) (alist-get 'patchSets change))))

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

(defun magit-gerrit-comment-at-point ()
  "Return the inline comment at point in a change buffer, or nil.
The comment has a `patchset' attribute, the patchset it is on."
  (magit-section-value-if 'gerrit-comment))

(defun magit-gerrit-draft-at-point ()
  "Return the draft comment at point in a change buffer, or nil."
  (magit-section-value-if 'gerrit-draft))

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
  "Get the Gerrit project name for the current repo.
For HTTP(S) remotes, the \"/a/\" prefix of authenticated URLs is
removed, so is the path of `magit-gerrit-web-url' when Gerrit is not
at the root of its host."
  (when-let* ((url (magit-gerrit-get-remote-url))
              (url (url-generic-parse-url url))
              (prj (url-filename url)))
    (when (member (url-type url) '("http" "https"))
      (when-let* ((base (and magit-gerrit-web-url
                             (url-filename (url-generic-parse-url magit-gerrit-web-url))))
                  (base (string-trim-right base "/"))
                  ((not (string-empty-p base)))
                  ((string-prefix-p (concat base "/") prj)))
        (setq prj (substring prj (length base))))
      (setq prj (string-remove-prefix "/a/" prj)))
    (string-trim-right (string-trim-left prj "/") "\\.git")))

(defun magit-gerrit--remote-host (remote-url)
  "Return the host of REMOTE-URL, also for scp-like URLs (user@host:path)."
  (when remote-url
    (or (url-host (url-generic-parse-url remote-url))
        (and (string-match "\\`\\(?:[^@/]+@\\)?\\([^:/]+\\):" remote-url)
             (match-string 1 remote-url)))))


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
                      :current-patch-set))
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

(defun magit-gerrit--gitiles-base-url ()
  "Return the base URL of Gitiles, see `magit-gerrit-gitiles-base-url'."
  (or magit-gerrit-gitiles-base-url
      (when-let* ((remote-url (magit-gerrit-get-remote-url))
                  (host (url-host (url-generic-parse-url remote-url))))
        (concat "https://" host "/plugins/gitiles"))))

(defun magit-gerrit--gitiles-branch ()
  "Return the branch to browse in Gitiles.
This is the upstream branch of the current branch when it is on the
Gerrit remote, the default branch of the remote, or \"master\"."
  (or (magit-gerrit--default-target-branch)
      (when-let* ((head (magit-git-string "symbolic-ref" "--short"
                                          (format "refs/remotes/%s/HEAD" magit-gerrit-remote))))
        (string-remove-prefix (concat magit-gerrit-remote "/") head))
      "master"))

(defun magit-gerrit--gitiles-url (rev &optional path line)
  "Return the Gitiles URL of REV, or of PATH at REV, at LINE."
  (let ((base (or (magit-gerrit--gitiles-base-url) (user-error "Cannot deduce the Gitiles URL")))
        (project (or (magit-gerrit-get-project) (user-error "No Gerrit project"))))
    (concat (string-trim-right base "/") "/" project "/+/" rev
            (when path
              (concat "/" (mapconcat #'url-hexify-string (split-string path "/") "/")))
            (when line (format "#%d" line)))))

(defun magit-gerrit--get-gitiles-link (&optional permalink)
  "Return the Gitiles URL of the thing at point.
In a file buffer, this is the file at the current line (or the first
line of the region).  In Magit buffers, this is the commit at point,
the file at point or the repository.  Files and the repository are
shown at the branch from `magit-gerrit--gitiles-branch', or at the
HEAD commit when PERMALINK is non-nil."
  (magit-gerrit-get-remote-url)  ; Make sure `magit-gerrit-remote' is set
  (let ((rev (lambda () (if permalink
                            (magit-rev-parse "HEAD")
                          (concat "refs/heads/" (magit-gerrit--gitiles-branch))))))
    (cond
     ((and buffer-file-name (magit-toplevel))
      (magit-gerrit--gitiles-url
       (funcall rev)
       (file-relative-name buffer-file-name (magit-toplevel))
       (line-number-at-pos (if (use-region-p) (region-beginning) (point)))))
     ((when-let* ((commit (and (derived-mode-p 'magit-mode) (magit-commit-at-point))))
        (magit-gerrit--gitiles-url (magit-rev-parse commit))))
     ((when-let* ((file (and (derived-mode-p 'magit-mode) (magit-file-at-point))))
        (magit-gerrit--gitiles-url (funcall rev) file)))
     (t (magit-gerrit--gitiles-url (funcall rev))))))

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
   (magit-gerrit-wash-approvals-oneline (magit-gerrit-change-approvals change) change)
   (if (magit-gerrit-change-wip-p change) 'magit-dimmed 'magit-filename)))

(defun magit-gerrit--label-range (label &optional change)
  "Return the range of the values of LABEL, as (MIN . MAX).
The range given by the backend for CHANGE takes precedence over
`magit-gerrit-label-values'."
  (or (cdr (assoc label (alist-get 'labelRanges change)))
      (cdr (assoc label magit-gerrit-label-values))
      magit-gerrit-default-label-values))

(defun magit-gerrit-trans-score (type value-list &optional change)
  "Format the votes VALUE-LIST of label TYPE as a single character.
CHANGE is the change voted on, see `magit-gerrit--label-range'."
  (if (and value-list (length> value-list 0))
      (let* ((min-v (apply #'min value-list))
             (max-v (apply #'max value-list))
             (range (magit-gerrit--label-range type change))
             (t-min-v (car range))
             (t-max-v (cdr range)))
        (cond
         ((<= min-v t-min-v) (propertize "x" 'font-lock-face 'magit-signature-bad))
         ((>= max-v t-max-v) (propertize "✓" 'font-lock-face 'magit-signature-good))
         ((> min-v 0) (propertize (format "+%d" min-v) 'font-lock-face 'magit-signature-good))
         (t (propertize (format "%d" min-v) 'font-lock-face 'magit-signature-bad))))
    " "))

(defun magit-gerrit-wash-approvals-oneline (approvals &optional change)
  "Format APPROVALS on one line, one column per review label.
CHANGE is the change voted on, see `magit-gerrit--label-range'."
  (let* (type-values)
    (seq-doseq (approval approvals)
      (let ((type (alist-get 'type approval))
            (value (string-to-number (alist-get 'value approval))))
        (push value (alist-get type type-values nil nil #'string-equal))))
    (mapconcat
     (lambda (elem)
       (let* ((long (car elem))
              (score (magit-gerrit-trans-score
                      long (alist-get long type-values nil nil #'string-equal) change)))
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
          (let ((entry (condition-case err
                           (magit-gerrit--async-reviews)
                         (error (list :error (error-message-string err))))))
            (mapc #'magit-gerrit--insert-review (plist-get entry :changes))
            (when-let* ((err (plist-get entry :error)))
              (insert (propertize (format "%s\n" err) 'font-lock-face 'error)))
            (when (and (not (plist-get entry :time)) (plist-get entry :pending))
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

(defun magit-gerrit-gitiles-browse (&optional permalink)
  "Browse the thing at point in Gitiles.
In a file buffer, browse the file at the current line.  In Magit
buffers, browse the commit at point, the file at point or the
repository.  With a prefix argument PERMALINK, browse files at the
HEAD commit instead of at the branch."
  (interactive "P")
  (browse-url (magit-gerrit--get-gitiles-link permalink)))

(define-obsolete-function-alias 'magit-gerrit-gitiles-browse-url
  #'magit-gerrit-gitiles-browse "0.2.0")

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

(defcustom magit-gerrit-suggest-accounts-min-length 2
  "Minimum length of the input before asking the server for accounts.
The REST backend suggests the accounts matching the input when
reading reviewers and CCs.  Nil disables the suggestions."
  :group 'magit-gerrit
  :type '(choice (const :tag "Never suggest" nil) integer))

(defun magit-gerrit--account-table (&optional change)
  "Return the completion table of accounts, for reviewers of CHANGE.
It has the accounts of `magit-gerrit--account-candidates' and, when
the backend can suggest accounts, the ones matching the input."
  (let ((static (magit-gerrit--account-candidates))
        (backend (magit-gerrit-current-backend t)))
    (if (and magit-gerrit-suggest-accounts-min-length
             (magit-gerrit--supports-p 'suggest-accounts))
        (completion-table-with-cache
         (lambda (input)
           (seq-uniq
            (append (when (>= (length input) magit-gerrit-suggest-accounts-min-length)
                      (ignore-errors (magit-gerrit-backend-suggest-accounts backend input change)))
                    static))))
      static)))

(defun magit-gerrit--read-account-list (prompt &optional change)
  "Read a non-empty list of accounts with PROMPT, for CHANGE."
  (or (completing-read-multiple prompt (magit-gerrit--account-table change)
                                nil nil nil 'magit-gerrit-reviewer-history)
      (user-error "No account")))

(defun magit-gerrit--change-reviewers (change)
  "Return the reviewers and CCs of CHANGE, querying them if needed."
  (or (append (alist-get 'allReviewers change) (alist-get 'ccs change))
      (let ((change (car (magit-gerrit--query-changes
                          (format "change:%s" (magit-gerrit-change-number change))
                          :reviewers))))
        (append (alist-get 'allReviewers change) (alist-get 'ccs change)))))

(defun magit-gerrit--refresh-buffer (buffer)
  "Refresh BUFFER, a Magit buffer, if it is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (magit-refresh))))

(defun magit-gerrit--refresh-change-buffers (number)
  "Refresh the other buffers showing the change NUMBER."
  (dolist (buffer (buffer-list))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'magit-gerrit-change-mode)
                   (equal magit-gerrit-buffer-change-number number))
          (magit-refresh))))))

(defmacro magit-gerrit--with-change (feature bindings &rest body)
  "Run BODY on the change at point with the backend supporting FEATURE.
BINDINGS is a list of two symbols, bound to the change and to the
backend.  The current buffer is refreshed after BODY."
  (declare (indent 2) (debug (symbolp (symbolp symbolp) body)))
  `(let ((,(car bindings) (magit-gerrit--change-at-point-or-error))
         (,(cadr bindings) (magit-gerrit--require ',feature)))
     (prog1 (progn ,@body)
       (magit-refresh))))

(defun magit-gerrit-add-reviewer (accounts)
  "Add ACCOUNTS as reviewers to the change at point."
  (interactive (list (magit-gerrit--read-account-list "Add reviewers: " (magit-gerrit-change-at-point))))
  (magit-gerrit--with-change reviewers (change backend)
    (magit-gerrit-backend-add-reviewers backend change accounts)))

(defun magit-gerrit-remove-reviewer (accounts)
  "Remove ACCOUNTS from the reviewers of the change at point."
  (interactive
   (let ((change (magit-gerrit--change-at-point-or-error)))
     (list (or (completing-read-multiple
                "Remove reviewers: "
                (seq-keep #'magit-gerrit--account-string (magit-gerrit--change-reviewers change)))
               (user-error "No account")))))
  (magit-gerrit--with-change reviewers (change backend)
    (magit-gerrit-backend-remove-reviewers backend change accounts)))

(defun magit-gerrit-add-cc (accounts)
  "Add ACCOUNTS in CC of the change at point."
  (interactive (list (magit-gerrit--read-account-list "Add CC: " (magit-gerrit-change-at-point))))
  (magit-gerrit--with-change cc (change backend)
    (magit-gerrit-backend-add-reviewers backend change accounts t)))

(defun magit-gerrit-mark-wip (args)
  "Mark the change at point as work in progress, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change wip (change backend)
    (magit-gerrit-backend-set-wip backend change t (magit-gerrit--review-options args))))

(defun magit-gerrit-mark-ready (args)
  "Mark the change at point as ready for review, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change wip (change backend)
    (magit-gerrit-backend-set-wip backend change nil (magit-gerrit--review-options args))))

(defun magit-gerrit-set-topic (topic)
  "Set the TOPIC of the change at point (an empty topic to remove it)."
  (interactive
   (let ((change (magit-gerrit--change-at-point-or-error)))
     (list (read-string "Topic (empty to remove): " (magit-gerrit-change-topic change)))))
  (magit-gerrit--with-change topic (change backend)
    (let ((topic (string-trim topic)))
      (magit-gerrit-backend-set-topic backend change (unless (string-empty-p topic) topic)))))

(defun magit-gerrit-rebase-review (&optional args)
  "Rebase the change at point on Gerrit, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change rebase (change backend)
    (magit-gerrit-backend-rebase backend change (magit-gerrit--review-options args))))

(defun magit-gerrit-move-review (branch args)
  "Move the change at point to BRANCH, using transient ARGS."
  (interactive (list (magit-gerrit--read-target-branch "Move to branch: ")
                     (magit-gerrit-arguments)))
  (magit-gerrit--with-change move (change backend)
    (magit-gerrit-backend-move backend change branch (magit-gerrit--review-options args))))

(defun magit-gerrit-arguments ()
  "Return the arguments of the `magit-gerrit-dispatch' transient."
  (transient-args 'magit-gerrit-dispatch))

(defun magit-gerrit--label-votes (label &optional change)
  "Return the possible votes for LABEL, as \"LABEL=VALUE\" strings.
When the backend tells which votes the user can give on CHANGE, only
these are returned."
  (let ((values (or (cdr (assoc label (alist-get 'permittedLabels change)))
                    (let ((range (magit-gerrit--label-range label change)))
                      (number-sequence (cdr range) (car range) -1)))))
    (mapcar (lambda (value)
              (format "%s=%s" label (if (> value 0) (format "+%d" value) value)))
            (sort (copy-sequence values) #'>))))

(defun magit-gerrit--read-votes (change &optional label)
  "Read votes on CHANGE, as a list of \"LABEL=VALUE\" strings.
When LABEL is non-nil, read a single vote for this label."
  (if label
      (list (completing-read "Vote: " (magit-gerrit--label-votes label change) nil t))
    (completing-read-multiple "Votes (LABEL=VALUE, comma separated): "
                              (mapcan (lambda (label) (magit-gerrit--label-votes label change))
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
  (let ((submit (and (member "--submit" args) t)))
    (magit-gerrit--with-change vote (change backend)
      (magit-gerrit-backend-vote backend change votes
                                 (magit-gerrit--review-options args :submit submit))
      (when submit
        (magit-fetch-all-no-prune)))))

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
  (magit-gerrit--with-change submit (change backend)
    (magit-gerrit-backend-submit backend change (magit-gerrit--review-options args))
    (magit-fetch-all-no-prune)))

(defun magit-gerrit-abandon-review (args)
  "Abandon the review at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change abandon (change backend)
    (magit-gerrit-backend-abandon backend change (magit-gerrit--review-options args))))

(defun magit-gerrit-restore-review (args)
  "Restore the abandoned review at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change restore (change backend)
    (magit-gerrit-backend-restore backend change (magit-gerrit--review-options args))))


;;; Messages

(defvar-local magit-gerrit--message-callback nil
  "Function called with the text of a `magit-gerrit-message-mode' buffer.")

(defvar-local magit-gerrit--message-start nil
  "Marker of the start of the text in a `magit-gerrit-message-mode' buffer.")

(defvar-keymap magit-gerrit-message-mode-map
  :doc "Keymap of `magit-gerrit-message-mode'."
  "C-c C-c" #'magit-gerrit-message-send
  "C-c C-k" #'magit-gerrit-message-cancel)

(define-derived-mode magit-gerrit-message-mode text-mode "Gerrit-Message"
  "Mode for editing a Gerrit comment or message.

\\<magit-gerrit-message-mode-map>\
Type \\[magit-gerrit-message-send] to send the text, \
\\[magit-gerrit-message-cancel] to cancel.

\\{magit-gerrit-message-mode-map}"
  :interactive nil
  :group 'magit-gerrit)

(defun magit-gerrit-read-message-in-buffer (prompt context initial callback)
  "Edit a message in a `magit-gerrit-message-mode' buffer.
PROMPT and CONTEXT are shown above the message, INITIAL is its initial
text.  CALLBACK is called with the text when it is sent."
  (let ((buffer (generate-new-buffer "*magit-gerrit-message*"))
        (dir default-directory))
    (with-current-buffer buffer
      (magit-gerrit-message-mode)
      (setq default-directory dir)
      (insert (propertize
               (concat (when context
                         (concat (string-trim-right context) "\n\n"))
                       (substitute-command-keys
                        (format "%s (\\[magit-gerrit-message-send] to send, \\[magit-gerrit-message-cancel] to cancel):\n"
                                prompt)))
               'read-only t 'front-sticky t 'rear-nonsticky t
               'font-lock-face 'font-lock-comment-face 'face 'font-lock-comment-face))
      (setq magit-gerrit--message-start (point-marker))
      (when initial (insert initial))
      (setq magit-gerrit--message-callback callback)
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)))

(defun magit-gerrit-read-message-in-minibuffer (prompt _context initial callback)
  "Read a message with PROMPT and INITIAL text in the minibuffer.
CALLBACK is called with the message."
  (funcall callback (read-string (concat prompt ": ") initial)))

(defun magit-gerrit-message-send ()
  "Send the message of the current `magit-gerrit-message-mode' buffer."
  (interactive nil magit-gerrit-message-mode)
  (let ((message (string-trim (buffer-substring-no-properties magit-gerrit--message-start
                                                              (point-max))))
        (callback magit-gerrit--message-callback))
    (when (string-empty-p message)
      (user-error "Empty message, type %s to cancel"
                  (substitute-command-keys "\\[magit-gerrit-message-cancel]")))
    (quit-window t)
    (funcall callback message)))

(defun magit-gerrit-message-cancel ()
  "Cancel the message of the current `magit-gerrit-message-mode' buffer."
  (interactive nil magit-gerrit-message-mode)
  (quit-window t)
  (message "Cancelled"))

(defun magit-gerrit--read-message (prompt context initial callback)
  "Read a message with PROMPT, CONTEXT and INITIAL text, then call CALLBACK.
The message is read by `magit-gerrit-read-message-function'.
CALLBACK is called with the message in the current buffer, which is
refreshed afterwards."
  (let ((buffer (current-buffer)))
    (funcall magit-gerrit-read-message-function prompt context initial
             (lambda (message)
               (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
                 (funcall callback message)
                 (when (derived-mode-p 'magit-mode)
                   (magit-refresh)))))))


;;; Inline comments

(defvar-local magit-gerrit--diff-change nil
  "The change of the patchset shown in a diff buffer, as (REVISION . CHANGE).")

(defun magit-gerrit--diff-revision ()
  "Return the commit shown in the current diff or revision buffer, or nil.
For a range, this is the end of the range."
  (let ((rev (cond ((derived-mode-p 'magit-revision-mode)
                    (bound-and-true-p magit-buffer-revision))
                   ((derived-mode-p 'magit-diff-mode)
                    (when-let* ((range (seq-some (lambda (var) (and (boundp var) (symbol-value var)))
                                                 '(magit-buffer-diff-range magit-buffer-range)))
                                ((string-match "\\.\\.\\.?\\(.+\\)\\'" range)))
                      (match-string 1 range))))))
    (and rev (magit-rev-parse rev))))

(defun magit-gerrit--hunk-position (hunk)
  "Return the position of point in the HUNK section, as (LINE . SIDE).
LINE is the line number of the line at point in the new version of the
file, SIDE is \"REVISION\"; for a removed line, LINE is in the old
version and SIDE is \"PARENT\"."
  (save-excursion
    (let ((target (line-beginning-position))
          old new)
      (goto-char (oref hunk start))
      (unless (looking-at "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)")
        (user-error "Cannot comment on this hunk"))
      (setq old (string-to-number (match-string 1))
            new (string-to-number (match-string 2)))
      (forward-line 1)
      (while (< (point) target)
        (pcase (char-after)
          (?+ (cl-incf new))
          (?- (cl-incf old))
          (?\s (cl-incf old) (cl-incf new)))
        (forward-line 1))
      (if (and (> target (oref hunk start)) (eq (char-after) ?-))
          (cons old "PARENT")
        (cons new "REVISION")))))

(defun magit-gerrit--diff-position ()
  "Return the position at point in a diff buffer as (FILE LINE SIDE), or nil.
LINE is nil on the heading of a file.  See `magit-gerrit--hunk-position'."
  (when-let* ((section (magit-current-section)))
    (pcase (oref section type)
      ('file (list (oref section value) nil "REVISION"))
      ('hunk (let ((pos (magit-gerrit--hunk-position section)))
               (list (oref (oref section parent) value) (car pos) (cdr pos)))))))

(defun magit-gerrit--read-file (patchset)
  "Read the path of a file of PATCHSET."
  (let ((files (seq-remove (lambda (file) (string-prefix-p "/" file))
                           (mapcar (lambda (f) (alist-get 'file f))
                                   (alist-get 'files patchset)))))
    (completing-read "File: " files nil (and files t))))

(defun magit-gerrit--read-line ()
  "Read a line number, nil for a comment on the whole file."
  (let ((line (string-trim (read-string "Line (empty for the whole file): "))))
    (unless (string-empty-p line)
      (or (and (string-match-p "\\`[0-9]+\\'" line) (string-to-number line))
          (user-error "Invalid line number: %s" line)))))

(defun magit-gerrit--comment-target ()
  "Return the target of a new inline comment at point.
The target is a plist with the properties :change, :revision, :file,
:line and :side.  In a diff or revision buffer, this is the line at
point; in a change buffer, the file or the comment at point, the line
being read; elsewhere, the change at point, the file and line being
read."
  (cond
   ((derived-mode-p 'magit-diff-mode 'magit-revision-mode)
    (let* ((position (or (magit-gerrit--diff-position) (user-error "No file at point")))
           (revision (or (magit-gerrit--diff-revision) (user-error "No commit in this buffer")))
           (change (if (equal (car magit-gerrit--diff-change) revision)
                       (cdr magit-gerrit--diff-change)
                     (magit-gerrit--read-commit-change "Change" revision))))
      (list :change change :revision revision
            :file (nth 0 position) :line (nth 1 position) :side (nth 2 position))))
   ((when-let* ((comment (or (magit-gerrit-comment-at-point) (magit-gerrit-draft-at-point))))
      (list :change (magit-gerrit--change-at-point-or-error)
            :revision (alist-get 'revision (alist-get 'patchset comment))
            :file (alist-get 'file comment)
            :line (alist-get 'line comment)
            :side (or (alist-get 'side comment) "REVISION"))))
   (t
    (let* ((change (magit-gerrit--change-at-point-or-error))
           (file-at-point (magit-section-value-if 'gerrit-file))
           (patchset (or (cdr file-at-point) (magit-gerrit-patchset-at-point)
                         (magit-gerrit-change-patchset change)))
           (file (or (car file-at-point) (magit-gerrit--read-file patchset))))
      (list :change change :revision (alist-get 'revision patchset)
            :file file :line (magit-gerrit--read-line) :side "REVISION")))))

(defun magit-gerrit--comment-unresolved (args default)
  "Return whether a comment is unresolved according to the transient ARGS.
Return DEFAULT when ARGS have neither \"--unresolved\" nor
\"--resolved\"."
  (cond ((member "--unresolved" args) t)
        ((member "--resolved" args) nil)
        (t default)))

(defun magit-gerrit--post-comment (change revision comment args)
  "Post COMMENT on REVISION of CHANGE, using the transient ARGS.
COMMENT is a plist, see `magit-gerrit-backend-post-comments'.  With
the \"--draft\" argument, it is saved as a draft."
  (let* ((draft (and (member "--draft" args) t))
         (backend (magit-gerrit--require (if draft 'drafts 'comment))))
    (magit-gerrit-backend-post-comments
     backend change revision (list comment)
     (if draft '(:draft t) (magit-gerrit--review-options args)))
    (magit-gerrit--refresh-change-buffers (magit-gerrit-change-number change))
    (message (if draft "Draft saved" "Comment posted"))))

(defun magit-gerrit--format-location (file line)
  "Format the location of a comment on FILE at LINE."
  (concat (pcase file
            ("/COMMIT_MSG" "Commit message")
            ("/PATCHSET_LEVEL" "Patchset")
            ("/MERGE_LIST" "Merge list")
            (_ file))
          (if line (format ":%s" line) "")))

(defun magit-gerrit-comment (args)
  "Add an inline comment, using transient ARGS.
In a diff of a patchset (see `magit-gerrit-show-patchset-file'), or
in a revision buffer, comment on the line at point (on its old version
for a removed line).  In a change buffer, comment on the file or on
the line of the comment at point.  With the \"--draft\" argument, save
the comment as a draft (if the backend supports drafts).  New comments
are unresolved, unless the \"--resolved\" argument is given."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--require (if (member "--draft" args) 'drafts 'comment))
  (let* ((target (magit-gerrit--comment-target))
         (change (plist-get target :change)))
    (magit-gerrit--read-message
     (format "Comment on %s" (magit-gerrit--format-location (plist-get target :file)
                                                            (plist-get target :line)))
     (format "Change %s: %s" (magit-gerrit-change-number change) (magit-gerrit-change-subject change))
     nil
     (lambda (message)
       (magit-gerrit--post-comment
        change (plist-get target :revision)
        (list :file (plist-get target :file) :line (plist-get target :line)
              :side (plist-get target :side) :message message
              :unresolved (magit-gerrit--comment-unresolved args t))
        args)))))

(defun magit-gerrit--comment-at-point-or-error ()
  "Return the published comment at point, check that it can be replied to."
  (let ((comment (or (magit-gerrit-comment-at-point) (user-error "No comment at point"))))
    (magit-gerrit--require 'reply)
    (unless (alist-get 'id comment)
      (user-error "This comment has no identifier, it can't be replied to"))
    comment))

(defun magit-gerrit--reply (comment message unresolved args)
  "Reply MESSAGE to COMMENT, UNRESOLVED or not, using the transient ARGS."
  (magit-gerrit--post-comment
   (magit-gerrit--change-at-point-or-error)
   (alist-get 'revision (alist-get 'patchset comment))
   (list :file (alist-get 'file comment) :line (alist-get 'line comment)
         :side (alist-get 'side comment) :in-reply-to (alist-get 'id comment)
         :message message :unresolved unresolved)
   args))

(defun magit-gerrit-reply (args)
  "Reply to the comment at point, using transient ARGS.
The thread stays unresolved if the comment is, unless the
\"--resolved\" or \"--unresolved\" argument is given.  With the
\"--draft\" argument, save the reply as a draft."
  (interactive (list (magit-gerrit-arguments)))
  (when (member "--draft" args)
    (magit-gerrit--require 'drafts))
  (let ((comment (magit-gerrit--comment-at-point-or-error)))
    (magit-gerrit--read-message
     (format "Reply to %s" (magit-gerrit--format-account (alist-get 'reviewer comment)))
     (concat (magit-gerrit--format-location (alist-get 'file comment) (alist-get 'line comment))
             "\n"
             (replace-regexp-in-string "^" "> " (or (alist-get 'message comment) "")))
     nil
     (lambda (message)
       (magit-gerrit--reply comment message
                            (magit-gerrit--comment-unresolved
                             args (eq (alist-get 'unresolved comment) t))
                            args)))))

(defun magit-gerrit-reply-done (args)
  "Reply \"Done\" to the comment at point and resolve it, using ARGS.
ARGS are the transient arguments."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--reply (magit-gerrit--comment-at-point-or-error) "Done" nil args)
  (magit-refresh))

(defun magit-gerrit-reply-ack (args)
  "Reply \"Ack\" to the comment at point and resolve it, using ARGS.
ARGS are the transient arguments."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--reply (magit-gerrit--comment-at-point-or-error) "Ack" nil args)
  (magit-refresh))

(defun magit-gerrit--draft-at-point-or-error ()
  "Return the draft comment at point, check that drafts are supported."
  (prog1 (or (magit-gerrit-draft-at-point) (user-error "No draft comment at point"))
    (magit-gerrit--require 'drafts)))

(defun magit-gerrit-edit-draft (draft)
  "Edit the text of the DRAFT comment at point."
  (interactive (list (magit-gerrit--draft-at-point-or-error)))
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--read-message
     (format "Draft on %s" (magit-gerrit--format-location (alist-get 'file draft) (alist-get 'line draft)))
     nil (alist-get 'message draft)
     (lambda (message)
       (magit-gerrit-backend-update-draft (magit-gerrit--require 'drafts) change draft message
                                          (eq (alist-get 'unresolved draft) t))))))

(defun magit-gerrit-delete-draft (draft)
  "Delete the DRAFT comment at point."
  (interactive (list (magit-gerrit--draft-at-point-or-error)))
  (when (y-or-n-p "Delete this draft? ")
    (magit-gerrit--with-change drafts (change backend)
      (magit-gerrit-backend-delete-draft backend change draft))))

(defun magit-gerrit-publish-drafts (args)
  "Publish the draft comments of the change at point, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change drafts (change backend)
    (magit-gerrit-backend-publish-drafts backend change (magit-gerrit--review-options args))))


;;; More actions on changes

(defun magit-gerrit-set-hashtags (hashtags)
  "Set the HASHTAGS of the change at point."
  (interactive
   (let ((change (magit-gerrit--change-at-point-or-error)))
     (magit-gerrit--require 'hashtags)
     (list (completing-read-multiple "Hashtags (comma separated): "
                                     (magit-gerrit-change-hashtags change) nil nil
                                     (string-join (magit-gerrit-change-hashtags change) ",")))))
  (magit-gerrit--with-change hashtags (change backend)
    (let ((old (magit-gerrit-change-hashtags change))
          (new (seq-uniq (mapcar #'string-trim hashtags))))
      (magit-gerrit-backend-set-hashtags backend change
                                         (seq-difference new old) (seq-difference old new)))))

(defun magit-gerrit-toggle-star ()
  "Star the change at point, or unstar it if it is starred."
  (interactive)
  (magit-gerrit--with-change star (change backend)
    (let ((starred (not (magit-gerrit-change-starred-p change))))
      (magit-gerrit-backend-set-starred backend change starred)
      (message (if starred "Starred change %s" "Unstarred change %s")
               (magit-gerrit-change-number change)))))

(defun magit-gerrit-toggle-private (args)
  "Mark the change at point as private, or as public, using transient ARGS."
  (interactive (list (magit-gerrit-arguments)))
  (magit-gerrit--with-change private (change backend)
    (magit-gerrit-backend-set-private backend change (not (magit-gerrit-change-private-p change))
                                      (magit-gerrit--review-options args))))

(defun magit-gerrit-edit-commit-message (args)
  "Edit the commit message of the change at point, using transient ARGS.
Gerrit creates a new patchset with the new message."
  (interactive (list (magit-gerrit-arguments)))
  (let ((change (magit-gerrit--change-at-point-or-error)))
    (magit-gerrit--require 'edit-message)
    (let ((message (or (magit-gerrit-change-commit-message change)
                       (magit-gerrit-change-commit-message
                        (magit-gerrit--get-change (magit-gerrit-change-number change))))))
      (magit-gerrit--read-message
       "Commit message" (format "Change %s" (magit-gerrit-change-number change)) message
       (lambda (new)
         (magit-gerrit-backend-set-commit-message
          (magit-gerrit--require 'edit-message) change (concat (string-trim-right new) "\n")
          (magit-gerrit--review-options args)))))))

(defun magit-gerrit-revert-change (args)
  "Create a change reverting the change at point, using transient ARGS.
The \"--message=\" argument is the commit message of the revert.
Show the created change."
  (interactive (list (magit-gerrit-arguments)))
  (let* ((change (magit-gerrit--change-at-point-or-error))
         (revert (magit-gerrit-backend-revert (magit-gerrit--require 'revert) change
                                              (magit-gerrit--review-options args))))
    (when-let* ((number (magit-gerrit-change-number revert)))
      (message "Created the revert change %s" number)
      (magit-gerrit-show-change number))))

(defun magit-gerrit-cherry-pick-change (branch args)
  "Cherry-pick the change at point to BRANCH on Gerrit, using transient ARGS.
The patchset at point is cherry-picked, or the current one.  The
\"--message=\" argument is the commit message of the cherry-pick.
Show the created change."
  (interactive (progn (magit-gerrit--require 'cherry-pick)
                      (list (magit-gerrit--read-target-branch "Cherry-pick to branch: ")
                            (magit-gerrit-arguments))))
  (let* ((change (magit-gerrit--change-at-point-or-error))
         (revision (or (alist-get 'revision (magit-gerrit-patchset-at-point))
                       (magit-gerrit-change-revision change)))
         (new (magit-gerrit-backend-cherry-pick (magit-gerrit--require 'cherry-pick)
                                                change revision branch
                                                (magit-gerrit--review-options args))))
    (when-let* ((number (magit-gerrit-change-number new)))
      (message "Created the change %s on %s" number branch)
      (magit-gerrit-show-change number))))


;;; Commit-msg hook

(defun magit-gerrit--web-url ()
  "Return the base URL of the Gerrit web interface.
See `magit-gerrit-web-url'.  Otherwise, ask the backend, or use the
host of the remote."
  (or magit-gerrit-web-url
      (when-let* ((backend (magit-gerrit-current-backend t)))
        (ignore-errors (magit-gerrit-backend-web-url backend)))
      (when-let* ((host (magit-gerrit--remote-host (magit-gerrit-get-remote-url))))
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
              (magit-gerrit--log (concat "GET " url) 200)
              t))
        (kill-buffer buffer)))))

(defvar magit-gerrit-commit-msg-hook-downloaders
  (list (cons 'https #'magit-gerrit--download-hook-https))
  "Functions downloading the commit-msg hook, as (METHOD . FUNCTION).
FUNCTION is called with the destination file, it returns non-nil on
success.  See `magit-gerrit-commit-msg-hook-methods'.")

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
                      (when-let* ((fn (alist-get method magit-gerrit-commit-msg-hook-downloaders)))
                        (funcall fn file)))
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

(defun magit-gerrit--read-commit-change (prompt &optional rev)
  "Return the change of REV, the commit at point or a commit read with PROMPT.
When the Change-Id matches several changes (cherry-picks on other
branches), let the user choose."
  (let* ((rev (or rev (magit-commit-at-point) (magit-read-branch-or-commit prompt)))
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

(defvar-keymap magit-gerrit-draft-section-map
  :doc "Keymap for the draft comment sections of a Gerrit change buffer."
  "<remap> <magit-visit-thing>" #'magit-gerrit-edit-draft
  "<remap> <magit-delete-thing>" #'magit-gerrit-delete-draft)

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
that change, on a draft comment to edit it.  Type \\[magit-delete-thing]
on a draft comment to delete it.  Type \\[magit-refresh] to fetch the
change again.  The other commands are in the Gerrit menu, see
`magit-gerrit-dispatch'.

\\{magit-gerrit-change-mode-map}"
  :interactive nil
  :group 'magit-gerrit
  ;; Make the Gerrit prefix key available, it is bound in `magit-gerrit-mode-map'
  (set-keymap-parent magit-gerrit-change-mode-map
                     (make-composed-keymap magit-gerrit-mode-map magit-mode-map)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-gerrit-change-mode))
  "Return the number of the change shown in a buffer in MAJOR-MODE."
  magit-gerrit-buffer-change-number)

(defvar-local magit-gerrit--inherited nil
  "Values of the configuration of the buffer the change buffer was shown from.
See `magit-gerrit--inherited-values'.")

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
  (magit-gerrit-current-backend)
  (let ((inherited (magit-gerrit--inherited-values)))
    (magit-setup-buffer #'magit-gerrit-change-mode t
      (magit-gerrit-buffer-change-number number)
      (magit-gerrit--inherited inherited))))

(defun magit-gerrit-change-refresh-buffer ()
  "Refresh the Gerrit change buffer."
  (magit-gerrit--inherit-values magit-gerrit--inherited)
  (magit-set-header-line-format (format "Gerrit change %s" magit-gerrit-buffer-change-number))
  (magit-insert-section (gerrit-change-buffer)
    (condition-case err
        (let ((change (or (magit-gerrit--get-change magit-gerrit-buffer-change-number)
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
                                 (and (magit-gerrit-change-private-p change) "PRIVATE")
                                 (and (magit-gerrit-change-starred-p change) "★")
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
           (mapcar #'car (alist-get 'labelRanges change))
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
      (when-let* ((requirements (alist-get 'submitRequirements change)))
        (insert (propertize "Submit requirements" 'font-lock-face 'magit-section-secondary-heading) "\n")
        (dolist (requirement requirements)
          (let ((status (or (alist-get 'status requirement) "")))
            (insert (format "  %-22s" (alist-get 'name requirement))
                    (propertize status 'font-lock-face
                                (pcase status
                                  ((or "SATISFIED" "OVERRIDDEN" "FORCED") 'success)
                                  ((or "UNSATISFIED" "ERROR") 'error)
                                  (_ 'shadow)))
                    "\n"))))
      (insert "\n"))))

(defun magit-gerrit-insert-change-reviewers ()
  "Insert the reviewers and the CCs of the change."
  (pcase-dolist (`(,key . ,title) '((allReviewers . "Reviewers") (ccs . "CC")))
    (when-let* ((accounts (alist-get key magit-gerrit-buffer-change)))
      (magit-insert-section (gerrit-reviewers key)
        (magit-insert-heading (format "%s (%d)" title (length accounts)))
        (dolist (account accounts)
          (insert (magit-gerrit--format-account account) "\n"))
        (insert "\n")))))

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
                  (if (alist-get 'sizeInsertions patchset)
                      (concat
                       (propertize (format "%7s" (format "+%d" (alist-get 'sizeInsertions patchset)))
                                   'font-lock-face 'diff-added)
                       " "
                       (propertize (format "%-7s" (format "-%d" (abs (or (alist-get 'sizeDeletions patchset) 0))))
                                   'font-lock-face 'diff-removed))
                    (make-string 15 ?\s))
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

(defun magit-gerrit--comment-threads (comments)
  "Group COMMENTS in threads, using their `id' and `inReplyTo'.
Return a list of threads, lists of comments starting with the first
comment of the thread, the replies being sorted by date.  Comments
without identifier have their own thread."
  (let ((by-id (make-hash-table :test #'equal))
        threads)
    (dolist (comment comments)
      (when-let* ((id (alist-get 'id comment)))
        (puthash id comment by-id)))
    (dolist (comment comments)
      (let ((root comment) (depth 0))
        (while-let (((< depth 100))
                    (parent (gethash (alist-get 'inReplyTo root) by-id)))
          (setq root parent)
          (cl-incf depth))
        (if-let* ((thread (assq root threads)))
            (setcdr thread (cons comment (cdr thread)))
          (push (list root comment) threads))))
    (mapcar (lambda (thread)
              (let ((root (car thread)))
                (cons root (remq root (sort (nreverse (cdr thread))
                                            (lambda (a b) (< (or (alist-get 'updated a) 0)
                                                             (or (alist-get 'updated b) 0))))))))
            (nreverse threads))))

(defun magit-gerrit--thread-unresolved-p (thread)
  "Return non-nil when the last comment of THREAD is unresolved."
  (eq (alist-get 'unresolved (car (last thread))) t))

(defun magit-gerrit--comment-body (comment &optional indent)
  "Return the message of COMMENT, indented with INDENT spaces (2 by default)."
  (concat (replace-regexp-in-string
           "^" (make-string (or indent 2) ?\s)
           (string-trim-right (or (alist-get 'message comment) "")))
          "\n"))

(defun magit-gerrit--comment-date (comment)
  "Return the date of COMMENT, formatted, or an empty string."
  (if-let* ((time (alist-get 'updated comment)))
      (propertize (format-time-string "%F %R" time) 'font-lock-face 'magit-log-date)
    ""))

(defun magit-gerrit--insert-thread (thread patchset)
  "Insert the comments of THREAD, on PATCHSET."
  (let ((root (car thread)))
    (magit-insert-section (gerrit-comment (cons (cons 'patchset patchset) root))
      (magit-insert-heading
        (propertize (magit-gerrit--format-location (alist-get 'file root) (alist-get 'line root))
                    'font-lock-face 'magit-filename)
        (if (equal (alist-get 'side root) "PARENT") " (base)" "")
        "  "
        (magit-gerrit--format-account (alist-get 'reviewer root))
        "  "
        (magit-gerrit--comment-date root)
        (cond ((magit-gerrit--thread-unresolved-p thread)
               (propertize "  UNRESOLVED" 'font-lock-face 'warning))
              ((alist-get 'id root)
               (propertize "  resolved" 'font-lock-face 'shadow))
              (t "")))
      (insert (magit-gerrit--comment-body root))
      (dolist (reply (cdr thread))
        (magit-insert-section (gerrit-comment (cons (cons 'patchset patchset) reply))
          (magit-insert-heading
            "  ↳ "
            (magit-gerrit--format-account (alist-get 'reviewer reply))
            "  "
            (magit-gerrit--comment-date reply))
          (insert (magit-gerrit--comment-body reply 4)))))))

(defun magit-gerrit-insert-change-comments ()
  "Insert the inline comments of all the patchsets, grouped in threads."
  (let* ((patchsets (seq-filter (lambda (ps) (alist-get 'comments ps))
                                (alist-get 'patchSets magit-gerrit-buffer-change)))
         (threads (mapcar (lambda (ps) (cons ps (magit-gerrit--comment-threads (alist-get 'comments ps))))
                          patchsets))
         (count (apply #'+ (mapcar (lambda (ps) (length (alist-get 'comments ps))) patchsets)))
         (unresolved (seq-count #'magit-gerrit--thread-unresolved-p (mapcan #'cdr (copy-tree threads)))))
    (when patchsets
      (magit-insert-section (gerrit-comments)
        (magit-insert-heading
          (format "Inline comments (%d%s)" count
                  (if (> unresolved 0) (format ", %d unresolved" unresolved) "")))
        (pcase-dolist (`(,patchset . ,ps-threads) threads)
          (magit-insert-section (gerrit-patchset-comments (alist-get 'number patchset))
            (magit-insert-heading (format "Patchset %s" (alist-get 'number patchset)))
            (dolist (thread ps-threads)
              (magit-gerrit--insert-thread thread patchset))))
        (insert "\n")))))

(defun magit-gerrit-insert-change-drafts ()
  "Insert the draft comments of the user."
  (let ((change magit-gerrit-buffer-change))
    (when-let* ((drafts (magit-gerrit-change-drafts change)))
      (magit-insert-section (gerrit-drafts)
        (magit-insert-heading (format "Draft comments (%d)" (length drafts)))
        (dolist (draft drafts)
          (let* ((number (alist-get 'patchSet draft))
                 (patchset (or (seq-find (lambda (ps) (equal (alist-get 'number ps) number))
                                         (alist-get 'patchSets change))
                               `((number . ,number) (revision . ,(alist-get 'revision draft))))))
            (magit-insert-section (gerrit-draft (cons (cons 'patchset patchset) draft))
              (magit-insert-heading
                (propertize (magit-gerrit--format-location (alist-get 'file draft) (alist-get 'line draft))
                            'font-lock-face 'magit-filename)
                (format "  patchset %s" number)
                (if (alist-get 'inReplyTo draft) "  (reply)" "")
                (if (eq (alist-get 'unresolved draft) t)
                    (propertize "  UNRESOLVED" 'font-lock-face 'warning)
                  ""))
              (insert (magit-gerrit--comment-body draft)))))
        (insert "\n")))))

(defun magit-gerrit--fetch-revision (patchset)
  "Fetch PATCHSET if needed and return its revision."
  (let ((revision (alist-get 'revision patchset)))
    (unless (magit-commit-p revision)
      (magit-gerrit-fetch-patchset (alist-get 'ref patchset)))
    revision))

(defun magit-gerrit--setup-diff-buffer (buffer mode revision change inherited)
  "Record REVISION of CHANGE in BUFFER, or in the buffer in MODE.
BUFFER displays REVISION.  INHERITED are the values of
`magit-gerrit--inherited-values' of the change buffer.  This lets
`magit-gerrit-comment' comment on the lines of the diff."
  (when-let* ((buffer (if (bufferp buffer) buffer (magit-get-mode-buffer mode))))
    (with-current-buffer buffer
      (magit-gerrit--inherit-values inherited)
      (setq-local magit-gerrit--diff-change (and change (cons revision change))))))

(defun magit-gerrit-show-patchset (patchset)
  "Show the commit of PATCHSET, fetching it if needed."
  (interactive (list (or (magit-gerrit-patchset-at-point)
                         (magit-gerrit-change-patchset
                          (magit-gerrit--change-at-point-or-error)))))
  (let ((change (magit-gerrit-change-at-point))
        (inherited (magit-gerrit--inherited-values))
        (revision (magit-gerrit--fetch-revision patchset)))
    (magit-gerrit--setup-diff-buffer (magit-show-commit revision) 'magit-revision-mode
                                     revision change inherited)))

(defun magit-gerrit--file-at-point ()
  "Return the file at point in a change buffer and its patchset, as a list."
  (if-let* ((value (magit-section-value-if 'gerrit-file)))
      (list (car value) (cdr value))
    (if-let* ((comment (or (magit-gerrit-comment-at-point) (magit-gerrit-draft-at-point))))
        (list (alist-get 'file comment) (alist-get 'patchset comment))
      (user-error "No file at point"))))

(defun magit-gerrit-show-patchset-file (file patchset)
  "Show the diff of FILE in PATCHSET, fetching it if needed.
Comment on its lines with `magit-gerrit-comment'."
  (interactive (magit-gerrit--file-at-point))
  (when (string-prefix-p "/" file)
    (user-error "No diff for %s" (magit-gerrit--format-location file nil)))
  (let ((change (magit-gerrit-change-at-point))
        (inherited (magit-gerrit--inherited-values))
        (revision (magit-gerrit--fetch-revision patchset)))
    (magit-gerrit--setup-diff-buffer
     (magit-diff-range (format "%s^..%s" revision revision) nil (list file))
     'magit-diff-mode revision change inherited)))


;;; Transient

(defmacro magit-gerrit--define-feature-predicates (&rest features)
  "Define the predicates telling whether the backend supports FEATURES.
The predicate of FEATURE is named magit-gerrit--supports-FEATURE-p,
the menus use them to show the unsupported commands as unavailable."
  `(progn
     ,@(mapcar (lambda (feature)
                 `(defun ,(intern (format "magit-gerrit--supports-%s-p" feature)) ()
                    ,(format "Return non-nil when the backend supports the feature `%s'.
See `magit-gerrit-features'." feature)
                    (magit-gerrit--supports-p ',feature)))
               features)))

(magit-gerrit--define-feature-predicates
 vote submit abandon restore rebase move topic reviewers cc wip comment reply drafts
 hashtags star private edit-message revert cherry-pick)

;;;###autoload (autoload 'magit-gerrit-dispatch "magit-gerrit" nil t)
(transient-define-prefix magit-gerrit-dispatch ()
  "Transient menu for `magit-gerrit' commands.
The commands the backend doesn't support are shown as unavailable."
  :incompatible '(("--unresolved" "--resolved"))
  ["Arguments"
   ("-m" "Message"                         "--message=")
   ("-n" "Notify"                          "--notify="
    :choices ("NONE" "OWNER" "OWNER_REVIEWERS" "ALL"))
   ("-s" "Submit after voting"             "--submit")
   ("-d" "Save comments as drafts"         "--draft"
    :inapt-if-not magit-gerrit--supports-drafts-p)
   ("-u" "Unresolved comment"              "--unresolved")
   ("-r" "Resolved comment"                "--resolved")]
  [["Change"
    ("RET" "Show"                          magit-gerrit-show-change)
    ("b" "Browse"                          magit-gerrit-browse-review)
    ("v" "Vote"                            magit-gerrit-vote
     :inapt-if-not magit-gerrit--supports-vote-p)
    ("S" "Submit"                          magit-gerrit-submit-review
     :inapt-if-not magit-gerrit--supports-submit-p)
    ("u" "Rebase"                          magit-gerrit-rebase-review
     :inapt-if-not magit-gerrit--supports-rebase-p)
    ("M" "Move to branch"                  magit-gerrit-move-review
     :inapt-if-not magit-gerrit--supports-move-p)
    ("B" "Abandon"                         magit-gerrit-abandon-review
     :inapt-if-not magit-gerrit--supports-abandon-p)
    ("R" "Restore"                         magit-gerrit-restore-review
     :inapt-if-not magit-gerrit--supports-restore-p)]
   ["Reviewers & state"
    ("A" "Add reviewers"                   magit-gerrit-add-reviewer
     :inapt-if-not magit-gerrit--supports-reviewers-p)
    ("K" "Remove reviewers"                magit-gerrit-remove-reviewer
     :inapt-if-not magit-gerrit--supports-reviewers-p)
    ("c" "Add CC"                          magit-gerrit-add-cc
     :inapt-if-not magit-gerrit--supports-cc-p)
    ("T" "Set topic"                       magit-gerrit-set-topic
     :inapt-if-not magit-gerrit--supports-topic-p)
    ("#" "Set hashtags"                    magit-gerrit-set-hashtags
     :inapt-if-not magit-gerrit--supports-hashtags-p)
    ("w" "Mark work in progress"           magit-gerrit-mark-wip
     :inapt-if-not magit-gerrit--supports-wip-p)
    ("W" "Mark ready for review"           magit-gerrit-mark-ready
     :inapt-if-not magit-gerrit--supports-wip-p)
    ("I" "Toggle private"                  magit-gerrit-toggle-private
     :inapt-if-not magit-gerrit--supports-private-p)
    ("*" "Toggle star"                     magit-gerrit-toggle-star
     :inapt-if-not magit-gerrit--supports-star-p)]
   ["Comments"
    ("C" "Comment"                         magit-gerrit-comment
     :inapt-if-not magit-gerrit--supports-comment-p)
    ("a" "Reply"                           magit-gerrit-reply
     :inapt-if-not magit-gerrit--supports-reply-p)
    ("f" "Reply \"Done\" (resolve)"        magit-gerrit-reply-done
     :inapt-if-not magit-gerrit--supports-reply-p)
    ("k" "Reply \"Ack\" (resolve)"         magit-gerrit-reply-ack
     :inapt-if-not magit-gerrit--supports-reply-p)
    ("p" "Publish drafts"                  magit-gerrit-publish-drafts
     :inapt-if-not magit-gerrit--supports-drafts-p)
    ("e" "Edit draft"                      magit-gerrit-edit-draft
     :inapt-if-not magit-gerrit--supports-drafts-p)
    ("x" "Delete draft"                    magit-gerrit-delete-draft
     :inapt-if-not magit-gerrit--supports-drafts-p)]]
  [["Patchset"
    ("d" "View diff"                       magit-gerrit-view-patchset-diff)
    ("D" "Download (checkout)"             magit-gerrit-download-patchset)
    ("F" "Cherry-pick"                     magit-gerrit-cherry-pick-patchset)]
   ["On the server"
    ("E" "Edit commit message"             magit-gerrit-edit-commit-message
     :inapt-if-not magit-gerrit--supports-edit-message-p)
    ("V" "Revert"                          magit-gerrit-revert-change
     :inapt-if-not magit-gerrit--supports-revert-p)
    ("L" "Cherry-pick to branch"           magit-gerrit-cherry-pick-change
     :inapt-if-not magit-gerrit--supports-cherry-pick-p)]]
  [["Others"
    ("P" "Push for review"                 magit-gerrit-push)
    ("/" "Query"                           magit-gerrit-query)
    ("H" "Install commit-msg hook"         magit-gerrit-install-commit-msg-hook)]
   ["Commit at point"
    ("o" "Show its change"                 magit-gerrit-show-commit-change)
    ("O" "Browse its change"               magit-gerrit-browse-commit)
    ("g" "Browse in Gitiles"               magit-gerrit-gitiles-browse)]
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
                      ((not (magit-gerrit-current-backend t))
                       (concat "No Gerrit backend: set `magit-gerrit-ssh-creds' or "
                               "`magit-gerrit-rest-url' to enable magit-gerrit-mode")))))
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
                   (when-let* ((backend (magit-gerrit-current-backend t)))
                     (magit-gerrit-backend-matches-remote-p backend remote-url))))
      (magit-gerrit-mode 1))))

;;;###autoload
(defun magit-gerrit-auto-enable ()
  "Try to auto enable magit-gerrit in the `magit-status' buffer."
  (interactive)
  (add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
  (add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t)
  (add-hook 'magit-revision-mode-hook #'magit-gerrit-check-enable t))

(provide 'magit-gerrit)

;; The backends require this file, load them once it is provided
(cl-eval-when (load eval)
  (require 'magit-gerrit-ssh)
  (require 'magit-gerrit-rest))

;;; magit-gerrit.el ends here
