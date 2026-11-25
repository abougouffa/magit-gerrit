;;; magit-gerrit-rest-test.el --- Tests of the backends and comments of magit-gerrit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Abdelhak Bougouffa

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; Run with "make test", after magit-gerrit-test.el (which creates the
;; test repositories).
;;
;; The REST backend talks to test/fake-gerrit-rest, a fake Gerrit server
;; started on a free local port.  It records the requests it receives
;; and answers with the JSON fixtures of test/fixtures/rest.  The
;; repository "rest" has an HTTP remote on this server.

;;; Code:

(require 'ert)
(require 'magit-gerrit-test)

(defvar magit-gerrit-rest-test--port nil
  "Port of the fake Gerrit REST server.")

(defun magit-gerrit-rest-test--url ()
  "Return the URL of the fake Gerrit REST server, starting it if needed."
  (unless magit-gerrit-rest-test--port
    (let* ((buffer (generate-new-buffer " *fake-gerrit-rest*"))
           (process (make-process :name "fake-gerrit-rest" :buffer buffer :noquery t
                                  :command (list (expand-file-name "fake-gerrit-rest"
                                                                   magit-gerrit-test--source-dir)))))
      (magit-gerrit-test--wait-for (lambda () (with-current-buffer buffer
                                                (string-match-p "\\`[0-9]+\n" (buffer-string)))))
      (setq magit-gerrit-rest-test--port
            (with-current-buffer buffer (string-to-number (buffer-string))))
      (unless (> magit-gerrit-rest-test--port 0)
        (error "The fake Gerrit REST server didn't start: %S" (process-status process)))))
  (format "http://127.0.0.1:%d" magit-gerrit-rest-test--port))

(defun magit-gerrit-rest-test--auth (host user)
  "Return the credentials of the fake server for HOST and USER."
  (should (equal host "127.0.0.1"))
  (should-not user)
  (cons "tester" "s3cret"))

(defun magit-gerrit-rest-test--parse (json)
  "Parse JSON, null as nil and false as `:false'."
  (json-parse-string json :object-type 'alist :array-type 'list :null-object nil))

(defun magit-gerrit-rest-test--last ()
  "Return the last request received by the fake server, as an alist."
  (magit-gerrit-rest-test--parse (with-temp-buffer
                                   (insert-file-contents (magit-gerrit-test--state "rest-last.json"))
                                   (buffer-string))))

(defun magit-gerrit-rest-test--calls ()
  "Return the requests received by the fake server since the last reset."
  (let ((file (magit-gerrit-test--state "rest-calls.log")))
    (when (file-exists-p file)
      (mapcar #'magit-gerrit-rest-test--parse
              (with-temp-buffer (insert-file-contents file)
                                (split-string (buffer-string) "\n" t))))))

(defun magit-gerrit-rest-test--reset ()
  "Forget the requests received by the fake server."
  (ignore-errors (delete-file (magit-gerrit-test--state "rest-calls.log"))))

(defvar magit-gerrit-rest-test--message "A comment"
  "Message given by the fake `magit-gerrit-read-message-function'.")

(defmacro magit-gerrit-rest-test--with-config (&rest body)
  "Run BODY with the configuration of the fake REST server."
  (declare (indent 0))
  `(let ((url-proxy-services nil)
         (magit-gerrit-rest-auth #'magit-gerrit-rest-test--auth)
         (magit-gerrit-rest-hosts (list (cons "127.0.0.1" (magit-gerrit-rest-test--url))))
         (magit-gerrit--change-id-cache (make-hash-table :test #'equal))
         (magit-gerrit-read-message-function
          (lambda (_prompt _context initial callback)
            (funcall callback (or magit-gerrit-rest-test--message initial)))))
     ,@body))

(defun magit-gerrit-rest-test--backend ()
  "Return a REST backend for the fake server."
  (magit-gerrit-rest-backend-create :url (magit-gerrit-rest-test--url)))

(defun magit-gerrit-rest-test--repo ()
  "Return the directory of the repository on the fake REST server."
  (let ((dir (magit-gerrit-test--dir 'rest)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (magit-gerrit-test--git dir "init" "-q" "-b" "main")
      (magit-gerrit-test--git dir "commit" "-q" "--allow-empty" "-m" "init")
      (magit-gerrit-test--git dir "remote" "add" "origin"
                              (concat (magit-gerrit-rest-test--url) "/a/foo/bar")))
    dir))

(defun magit-gerrit-rest-test--change-buffer ()
  "Show the change 1234 of the fake REST server, return its buffer."
  (let ((default-directory (magit-gerrit-rest-test--repo)))
    (with-current-buffer (magit-status-setup-buffer default-directory)
      (magit-gerrit-show-change 1234))
    (magit-get-mode-buffer 'magit-gerrit-change-mode 1234)))

(defun magit-gerrit-rest-test--goto (regexp)
  "Move to the first match of REGEXP in the current buffer."
  (goto-char (point-min))
  (re-search-forward regexp)
  (goto-char (match-beginning 0)))


;;; Conversions and configuration

(ert-deftest magit-gerrit-rest-test-conversions ()
  (should (= (magit-gerrit-rest--time "2023-11-14 22:13:20.000000000") 1700000000))
  (should-not (magit-gerrit-rest--time nil))
  (should (equal (magit-gerrit-rest--account '((_account_id . 7) (name . "Bob") (email . "b@x")))
                 '((name . "Bob") (email . "b@x") (accountId . 7))))
  (should (equal (magit-gerrit-rest--ref 1234 3) "refs/changes/34/1234/3"))
  (should (equal (magit-gerrit-rest--ref 5 1) "refs/changes/05/5/1"))
  (should (equal (magit-gerrit-rest--parse-body "\n)]}'\n{\"a\":[1,false]}") '((a 1 nil))))
  (should-not (magit-gerrit-rest--parse-body ""))
  (should (equal (magit-gerrit-rest--parse-vote "Code-Review=+2") '("Code-Review" . 2)))
  (should-error (magit-gerrit-rest--parse-vote "Code-Review") :type 'user-error)
  ;; ReviewInput comments, as sent by both backends
  (should (equal (magit-gerrit--json-encode
                  `((comments . ,(magit-gerrit--comments-input
                                  '((:file "a.c" :line 3 :message "x" :unresolved t)
                                    (:file "a.c" :side "PARENT" :line 1 :message "y" :in-reply-to "c1")
                                    (:file "b.c" :message "z"))))))
                 (concat "{\"comments\":{\"a.c\":[{\"line\":3,\"message\":\"x\",\"unresolved\":true},"
                         "{\"line\":1,\"side\":\"PARENT\",\"in_reply_to\":\"c1\",\"message\":\"y\",\"unresolved\":false}],"
                         "\"b.c\":[{\"message\":\"z\",\"unresolved\":false}]}}")))
  (should (equal (magit-gerrit--json-encode (magit-gerrit-rest--body)) "{}")))

(ert-deftest magit-gerrit-rest-test-url-detection ()
  (let ((magit-gerrit-rest-hosts '("review.example.com" ("git.example.org" . "https://example.org/r")))
        (magit-gerrit-web-url nil))
    (should (equal (magit-gerrit-rest--url-for-remote "https://review.example.com/a/p" nil)
                   "https://review.example.com"))
    (should (equal (magit-gerrit-rest--url-for-remote "ssh://me@git.example.org:29418/p" nil)
                   "https://example.org/r"))
    (should (equal (magit-gerrit-rest--url-for-remote "https://android.googlesource.com/platform/x" nil)
                   "https://android-review.googlesource.com"))
    (should-not (magit-gerrit-rest--url-for-remote "https://github.com/a/b" nil))
    (should (equal (magit-gerrit-rest--url-for-remote "http://h.example.com:8080/p" t)
                   "http://h.example.com:8080"))
    (should (equal (magit-gerrit-rest--url-for-remote "me@h.example.com:p.git" t) "https://h.example.com"))
    (let ((magit-gerrit-rest-googlesource nil))
      (should-not (magit-gerrit-rest--url-for-remote "https://android.googlesource.com/x" nil))))
  (should (equal (magit-gerrit--remote-host "me@host.example.com:foo/bar.git") "host.example.com"))
  ;; projects of HTTP remotes
  (cl-letf (((symbol-function 'magit-gerrit-get-remote-url) (lambda () "https://h.example.com/a/foo/bar.git")))
    (should (equal (magit-gerrit-get-project) "foo/bar"))
    (let ((magit-gerrit-web-url "https://h.example.com/r"))
      (cl-letf (((symbol-function 'magit-gerrit-get-remote-url) (lambda () "https://h.example.com/r/a/foo/bar")))
        (should (equal (magit-gerrit-get-project) "foo/bar"))))))

(ert-deftest magit-gerrit-rest-test-backend-selection ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit-web-url nil))
    (with-temp-buffer
      ;; an SSH remote: the SSH backend, unless REST is configured for its host
      (should (magit-gerrit-ssh-backend-p (magit-gerrit-current-backend)))
      (let ((magit-gerrit-rest-url "https://review.example.com/"))
        (let ((backend (magit-gerrit-current-backend)))
          (should (magit-gerrit-rest-backend-p backend))
          (should (equal (magit-gerrit-rest-backend-url backend) "https://review.example.com"))))
      (let ((magit-gerrit-rest-url "https://other.example.com"))
        (should (magit-gerrit-ssh-backend-p (magit-gerrit-current-backend))))
      (let ((magit-gerrit-backend 'rest))
        (should (equal (magit-gerrit-rest-backend-url (magit-gerrit-current-backend))
                       "https://review.example.com")))
      (let ((magit-gerrit-backend 'ssh) (magit-gerrit-rest-url "https://review.example.com"))
        (should (magit-gerrit-ssh-backend-p (magit-gerrit-current-backend))))
      (let ((magit-gerrit-backend 'nope))
        (should-error (magit-gerrit-current-backend) :type 'user-error))
      ;; the backend is cached until its configuration changes
      (should (eq (magit-gerrit-current-backend) (magit-gerrit-current-backend)))))
  ;; no backend for a repository not hosted on Gerrit
  (let ((default-directory magit-gerrit-test--source-dir))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magit-gerrit-get-remote-url) (lambda () "https://github.com/a/b")))
        (should-not (magit-gerrit-current-backend t))
        (should-error (magit-gerrit-current-backend) :type 'user-error)))))


;;; Requests

(ert-deftest magit-gerrit-rest-test-query ()
  (magit-gerrit-rest-test--with-config
    (let ((changes (magit-gerrit-backend-query (magit-gerrit-rest-test--backend)
                                               '("project:foo/bar" "status:open")
                                               '(:current-patch-set :submit-records)))
          (request (magit-gerrit-rest-test--last)))
      (should (equal (alist-get 'path request) "/changes/"))
      (should (equal (alist-get 'user request) "tester"))
      (should (equal (alist-get 'q (alist-get 'query request)) '("project:foo/bar status:open")))
      (should (equal (alist-get 'o (alist-get 'query request))
                     '("DETAILED_ACCOUNTS" "CURRENT_REVISION" "DETAILED_LABELS" "SUBMITTABLE")))
      (should (= (length changes) 2))
      (let ((change (car changes)))
        (should (= (magit-gerrit-change-number change) 1234))
        (should (equal (magit-gerrit-change-revision change) "aaaabbbbccccddddeeeeffff0000111122223333"))
        (should (equal (magit-gerrit-change-ref change) "refs/changes/34/1234/3"))
        (should (= (magit-gerrit-change-patchset-number change) 3))
        (should (= (magit-gerrit-change-deletions change) 7))
        (should (equal (magit-gerrit-change-url change)
                       (concat (magit-gerrit-rest-test--url) "/c/foo/bar/+/1234")))
        (should (equal (mapcar (lambda (a) (list (alist-get 'type a) (alist-get 'value a)))
                               (magit-gerrit-change-approvals change))
                       '(("Code-Review" "2") ("Verified" "-1"))))
        (should (equal (magit-gerrit-change-owner-username change) "jane"))
        ;; same summary as with the SSH backend
        (should (equal (substring-no-properties (magit-gerrit--format-change-summary change))
                       "#1234 NEW CR+2 Ve-1")))
      (should (magit-gerrit-change-wip-p (cadr changes))))))

(ert-deftest magit-gerrit-rest-test-errors-and-anonymous ()
  (magit-gerrit-rest-test--with-config
    (let ((backend (magit-gerrit-rest-test--backend)))
      (should (string-match-p "HTTP 400, bad query"
                              (cadr (should-error (magit-gerrit-backend-query backend "change:bad" nil)
                                                  :type 'user-error))))
      (should (with-current-buffer magit-gerrit-log-buffer-name
                (string-match-p "GET http://127.0.0.1:[0-9]+/a/changes/\\?q=change%3Abad.* => 400\nbad query"
                                (buffer-string))))
      (should-not (magit-gerrit-backend-get-change backend 999)))
    (let* ((magit-gerrit-rest-auth 'anonymous)
           (backend (magit-gerrit-rest-test--backend)))
      (should (= (length (magit-gerrit-backend-query backend "status:open" nil)) 2))
      (should-not (alist-get 'user (magit-gerrit-rest-test--last)))
      (should (string-match-p "HTTP 403, Authentication required"
                              (cadr (should-error (magit-gerrit-backend-rebase
                                                   backend '((number . 1234)) nil)
                                                  :type 'user-error)))))
    ;; wrong password: 401
    (let* ((magit-gerrit-rest-auth (lambda (&rest _) '("tester" . "wrong"))))
      (should (string-match-p "HTTP 401, Unauthorized"
                              (cadr (should-error (magit-gerrit-backend-query
                                                   (magit-gerrit-rest-test--backend) "x" nil)
                                                  :type 'user-error)))))
    ;; auth-source
    (let* ((netrc (make-temp-file "netrc" nil nil "machine 127.0.0.1 login tester password s3cret\n"))
           (auth-sources (list netrc))
           (magit-gerrit-rest-auth 'auth-source))
      (unwind-protect
          (progn (auth-source-forget-all-cached)
                 (magit-gerrit-backend-query (magit-gerrit-rest-test--backend) "status:open" nil)
                 (should (equal (alist-get 'user (magit-gerrit-rest-test--last)) "tester")))
        (delete-file netrc)
        (auth-source-forget-all-cached)))))

(ert-deftest magit-gerrit-rest-test-get-change ()
  (magit-gerrit-rest-test--with-config
    (dolist (old '(nil t))
      (if old
          (with-temp-file (magit-gerrit-test--state "rest-old"))
        (ignore-errors (delete-file (magit-gerrit-test--state "rest-old"))))
      (magit-gerrit-rest-test--reset)
      (unwind-protect
          (let ((change (magit-gerrit-backend-get-change (magit-gerrit-rest-test--backend) 1234))
                (paths (mapcar (lambda (c) (alist-get 'path c)) (magit-gerrit-rest-test--calls))))
            ;; an old server refuses SUBMIT_REQUIREMENTS, the change is fetched again
            (should (equal paths (append (if old '("/changes/1234" "/changes/1234") '("/changes/1234"))
                                         '("/changes/1234/comments" "/changes/1234/drafts"
                                           "/changes/1234/revisions/current/related" "/changes/"))))
            (should (equal (mapcar (lambda (d) (alist-get 'number d)) (alist-get 'dependsOn change)) '(1230)))
            (should (equal (alist-get 'ref (car (alist-get 'dependsOn change))) "refs/changes/30/1230/1"))
            (should-not (alist-get 'isCurrentPatchSet (car (alist-get 'dependsOn change))))
            (should (equal (mapcar (lambda (d) (alist-get 'number d)) (alist-get 'neededBy change)) '(1250)))
            (should (equal (alist-get 'labelRanges change)
                           '(("Code-Review" -2 . 2) ("Verified" -1 . 1) ("Zuul-Gate" 0 . 1))))
            (should (equal (magit-gerrit--label-votes "Code-Review" change)
                           '("Code-Review=+1" "Code-Review=0" "Code-Review=-1")))
            (should (equal (magit-gerrit--label-votes "Zuul-Gate" change) '("Zuul-Gate=+1" "Zuul-Gate=0")))
            (should (equal (mapcar (lambda (ps) (list (alist-get 'number ps) (length (alist-get 'comments ps))))
                                   (alist-get 'patchSets change))
                           '((1 1) (3 4))))
            (should (equal (alist-get 'file (car (alist-get 'drafts change))) "doc/new.md"))
            (should (equal (alist-get 'submitRequirements change)
                           (unless old '(((name . "Code-Review") (status . "SATISFIED"))
                                         ((name . "Verified") (status . "UNSATISFIED"))))))
            (should (string-prefix-p "Add a fancy feature\n" (magit-gerrit-change-commit-message change))))
        (ignore-errors (delete-file (magit-gerrit-test--state "rest-old")))))))


;;; Frontend with the REST backend

(ert-deftest magit-gerrit-rest-test-status-and-change-buffer ()
  (magit-gerrit-rest-test--with-config
    (let ((default-directory (magit-gerrit-rest-test--repo)))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
        (magit-gerrit-auto-enable)
        (unwind-protect
            (with-current-buffer (magit-status-setup-buffer default-directory)
              (should magit-gerrit-mode)
              (should (magit-gerrit-rest-backend-p (magit-gerrit-current-backend)))
              (should (equal (magit-gerrit-get-project) "foo/bar"))
              (should (string-match-p "Reviews \\[status:open\\] (2)" (buffer-string)))
              (should (string-match-p "1234 +\\[3\\] +Add a fancy feature" (buffer-string)))
              (should (equal (magit-gerrit--web-url) (magit-gerrit-rest-test--url))))
          (remove-hook 'magit-status-mode-hook #'magit-gerrit-check-enable)
          (remove-hook 'magit-log-mode-hook #'magit-gerrit-check-enable)
          (remove-hook 'magit-revision-mode-hook #'magit-gerrit-check-enable))))
    (with-current-buffer (magit-gerrit-rest-test--change-buffer)
      (let ((text (buffer-string)))
        (princ text)
        (dolist (re '("^Status: +NEW (submit: NOT_READY)" "^Hashtags: +perf, ui"
                      "^Code-Review +OK +\\+2 Bob <bob@example.com>" "^Verified +REJECT +-1 CI"
                      "^Zuul-Gate +MAY" "^Submit requirements" "^  Verified +UNSATISFIED"
                      "^Reviewers (2)" "^CC (1)" "^Carl <carl@example.com>"
                      "^renamed +\\+0 -0 +src/old_name.c (from src/b.c)"
                      "^Patchsets (2)" "^3   aaaabbbbcc     \\+42 -7" "^1   1111aaaa00 \\{16\\}REWORK"
                      "^Gerrit  2023-11-14" "^Inline comments (5, 1 unresolved)"
                      "^src/a.c:40  Bob <bob@example.com>  2023-11-14 22:15  UNRESOLVED"
                      "^  ↳ Jane Doe <jane@example.com>  2023-11-14 22:30\n    It can't"
                      "^src/a.c:7 (base)  Bob" "^Patchset  Bob" "^src/a.c:12  Bob .* resolved"
                      "^Draft comments (1)" "^doc/new.md:1  patchset 3  UNRESOLVED\n  Mention the option"
                      "^Depends on\n1230 +refs/changes/30/1230/1  (outdated patchset)"
                      "^Needed by\n1250 +refs/changes/50/1250/2$"))
          (should (string-match-p re text))))
      ;; the comment at point knows its patchset
      (magit-gerrit-rest-test--goto "^  ↳ Jane")
      (let ((comment (magit-gerrit-comment-at-point)))
        (should (equal (alist-get 'id comment) "c2"))
        (should (equal (alist-get 'revision (alist-get 'patchset comment))
                       "aaaabbbbccccddddeeeeffff0000111122223333")))
      (magit-gerrit-rest-test--goto "^doc/new.md:1")
      (should (equal (alist-get 'id (magit-gerrit-draft-at-point)) "d1"))
      (should (eq (key-binding (kbd "RET")) 'magit-gerrit-edit-draft))
      (should (eq (key-binding (kbd "k")) 'magit-gerrit-delete-draft)))))

(ert-deftest magit-gerrit-rest-test-change-actions ()
  (magit-gerrit-rest-test--with-config
    (with-current-buffer (magit-gerrit-rest-test--change-buffer)
      (cl-letf (((symbol-function 'magit-fetch-all-no-prune) #'ignore))
        (let ((rev "aaaabbbbccccddddeeeeffff0000111122223333"))
          ;; the refresh after an action re-fetches the change, so look for
          ;; the request of the action among the requests
          (cl-flet ((action (method path body)
                      (should (seq-find
                               (lambda (c) (and (equal (alist-get 'method c) method)
                                                (equal (alist-get 'path c) path)
                                                (equal (alist-get 'body c) body)
                                                (equal (alist-get 'user c) "tester")))
                               (magit-gerrit-rest-test--calls)))
                      (magit-gerrit-rest-test--reset)))
            (magit-gerrit-rest-test--reset)
            (magit-gerrit-rest-test--goto "^Status:")
            (magit-gerrit-vote '("Code-Review=+1") '("--message=LGTM ünï" "--notify=OWNER"))
            (action "POST" (format "/changes/1234/revisions/%s/review" rev)
                    '((labels (Code-Review . 1)) (message . "LGTM ünï") (notify . "OWNER")))
            (magit-gerrit-vote '("Verified=+1") '("--submit"))
            (should (equal (seq-take (mapcar (lambda (c) (alist-get 'path c)) (magit-gerrit-rest-test--calls)) 2)
                           (list (format "/changes/1234/revisions/%s/review" rev) "/changes/1234/submit")))
            (magit-gerrit-rest-test--reset)
            (magit-gerrit-submit-review '("--notify=NONE"))
            (action "POST" "/changes/1234/submit" '((notify . "NONE")))
            (magit-gerrit-abandon-review '("--message=Obsolete"))
            (action "POST" "/changes/1234/abandon" '((message . "Obsolete")))
            (magit-gerrit-restore-review nil)
            (action "POST" "/changes/1234/restore" nil)
            (magit-gerrit-rebase-review)
            (action "POST" "/changes/1234/rebase" nil)
            (magit-gerrit-move-review "stable" '("--message=Backport"))
            (action "POST" "/changes/1234/move" '((destination_branch . "stable") (message . "Backport")))
            (magit-gerrit-set-topic " new-topic ")
            (action "PUT" "/changes/1234/topic" '((topic . "new-topic")))
            (magit-gerrit-set-topic "")
            (action "DELETE" "/changes/1234/topic" nil)
            (magit-gerrit-add-reviewer '("alice@example.com"))
            (action "POST" "/changes/1234/revisions/current/review"
                    '((reviewers ((reviewer . "alice@example.com") (state . "REVIEWER")))))
            (magit-gerrit-add-cc '("dan"))
            (action "POST" "/changes/1234/revisions/current/review"
                    '((reviewers ((reviewer . "dan") (state . "CC")))))
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (_p coll &rest _)
                         (should (equal coll '("bob@example.com" "ci" "carl@example.com")))
                         '("carl@example.com"))))
              (call-interactively #'magit-gerrit-remove-reviewer))
            (action "DELETE" "/changes/1234/reviewers/carl@example.com" nil)
            (magit-gerrit-mark-wip '("--message=Not yet"))
            (action "POST" "/changes/1234/wip" '((message . "Not yet")))
            (magit-gerrit-mark-ready nil)
            (action "POST" "/changes/1234/ready" nil)
            (magit-gerrit-set-hashtags '("perf" "new"))
            (action "POST" "/changes/1234/hashtags" '((add "new") (remove "ui")))
            (magit-gerrit-toggle-star)
            (action "PUT" "/accounts/self/starred.changes/1234" nil)
            (should (string-match-p "^Status: +NEW ★" (buffer-string)))
            (magit-gerrit-toggle-star)
            (action "DELETE" "/accounts/self/starred.changes/1234" nil)
            (magit-gerrit-toggle-private nil)
            (action "POST" "/changes/1234/private" nil)
            (let ((magit-gerrit-rest-test--message "New subject\n\nNew body\n\nChange-Id: I0123"))
              (magit-gerrit-edit-commit-message nil))
            (action "PUT" "/changes/1234/message"
                    '((message . "New subject\n\nNew body\n\nChange-Id: I0123\n")))
            ;; errors of the server are reported
            (should (string-match-p "HTTP 409, Conflict: change is closed"
                                    (cadr (should-error (magit-gerrit-abandon-review '("--message=fail"))
                                                        :type 'user-error))))
            ;; revert and cherry-pick show the new change
            (let (shown)
              (cl-letf (((symbol-function 'magit-gerrit-show-change) (lambda (n) (push n shown))))
                (magit-gerrit-revert-change '("--message=Revert it"))
                (action "POST" "/changes/1234/revert" '((message . "Revert it")))
                (magit-gerrit-rest-test--goto "^1   1111aaaa00")
                (magit-gerrit-cherry-pick-change "stable" nil)
                (action "POST" "/changes/1234/revisions/1111aaaa0000000000000000000000000000aaaa/cherrypick"
                        '((destination . "stable"))))
              (should (equal shown '(1301 1300))))))))))

(ert-deftest magit-gerrit-rest-test-comments ()
  (magit-gerrit-rest-test--with-config
    (with-current-buffer (magit-gerrit-rest-test--change-buffer)
      (let ((rev "aaaabbbbccccddddeeeeffff0000111122223333")
            (body (lambda () (alist-get 'body (seq-find (lambda (c) (not (equal (alist-get 'method c) "GET")))
                                                       (magit-gerrit-rest-test--calls))))))
        ;; new comment on a file of the change, line read
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^modified +\\+40 -7 +src/a.c")
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "41")))
          (magit-gerrit-comment nil))
        (should (equal (funcall body)
                       '((comments (src/a.c ((line . 41) (message . "A comment") (unresolved . t))))
                         (drafts . "KEEP"))))
        ;; as a draft, resolved, on the whole file
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^modified +\\+40 -7 +src/a.c")
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
          (magit-gerrit-comment '("--draft" "--resolved")))
        (let ((request (seq-find (lambda (c) (equal (alist-get 'method c) "PUT")) (magit-gerrit-rest-test--calls))))
          (should (equal (alist-get 'path request) (format "/changes/1234/revisions/%s/drafts" rev)))
          (should (equal (alist-get 'body request)
                         '((path . "src/a.c") (message . "A comment") (unresolved . :false)))))
        ;; reply to the comment at point: in_reply_to, the thread stays unresolved
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^src/a.c:40")
        (let ((magit-gerrit-rest-test--message "Fixed in the next patchset"))
          (magit-gerrit-reply nil))
        (should (equal (funcall body)
                       '((comments (src/a.c ((line . 40) (in_reply_to . "c1")
                                             (message . "Fixed in the next patchset") (unresolved . t))))
                         (drafts . "KEEP"))))
        ;; Done and Ack resolve the thread, on the side of the comment
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^src/a.c:7 (base)")
        (magit-gerrit-reply-done nil)
        (should (equal (funcall body)
                       '((comments (src/a.c ((line . 7) (side . "PARENT") (in_reply_to . "c3")
                                             (message . "Done") (unresolved . :false))))
                         (drafts . "KEEP"))))
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^Patchset  Bob")
        (magit-gerrit-reply-ack nil)
        (should (equal (funcall body)
                       '((comments (/PATCHSET_LEVEL ((in_reply_to . "c4") (message . "Ack")
                                                     (unresolved . :false))))
                         (drafts . "KEEP"))))
        ;; drafts: edit, delete, publish
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^doc/new.md:1")
        (let ((magit-gerrit-rest-test--message "Mention the --foo option"))
          (call-interactively #'magit-gerrit-edit-draft))
        (let ((request (seq-find (lambda (c) (equal (alist-get 'method c) "PUT")) (magit-gerrit-rest-test--calls))))
          (should (equal (alist-get 'path request) (format "/changes/1234/revisions/%s/drafts/d1" rev)))
          (should (equal (alist-get 'body request)
                         '((path . "doc/new.md") (line . 1) (message . "Mention the --foo option")
                           (unresolved . t)))))
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-rest-test--goto "^doc/new.md:1")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (call-interactively #'magit-gerrit-delete-draft))
        (should (equal (alist-get 'path (car (magit-gerrit-rest-test--calls)))
                       (format "/changes/1234/revisions/%s/drafts/d1" rev)))
        (magit-gerrit-rest-test--reset)
        (magit-gerrit-publish-drafts '("--message=Round 2"))
        (should (equal (funcall body) '((drafts . "PUBLISH_ALL_REVISIONS") (message . "Round 2"))))
        ;; no reply on something else than a comment
        (goto-char (point-min))
        (should-error (magit-gerrit-reply nil) :type 'user-error)))))

(ert-deftest magit-gerrit-rest-test-suggest-accounts ()
  (magit-gerrit-rest-test--with-config
    (let ((backend (magit-gerrit-rest-test--backend)))
      (should (equal (magit-gerrit-backend-suggest-accounts backend "al" '((number . 1234)))
                     '("alice@example.com")))
      (should (equal (alist-get 'path (magit-gerrit-rest-test--last)) "/changes/1234/suggest_reviewers"))
      (should (equal (magit-gerrit-backend-suggest-accounts backend "al") '("alice@example.com")))
      (should (equal (alist-get 'query (magit-gerrit-rest-test--last))
                     '((suggest "") (q "al") (n "10")))))
    (let ((default-directory (magit-gerrit-rest-test--repo)))
      (with-temp-buffer
        (let ((table (magit-gerrit--account-table '((number . 1234)))))
          (should (member "alice@example.com" (all-completions "al" table)))
          (should-not (member "alice@example.com" (all-completions "a" table))))))))

(ert-deftest magit-gerrit-rest-test-async-reviews ()
  (magit-gerrit-rest-test--with-config
    (let ((default-directory (magit-gerrit-rest-test--repo))
          (magit-gerrit-async-reviews t)
          (magit-gerrit--async-cache (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
        (with-current-buffer (magit-status-setup-buffer default-directory)
          (magit-gerrit-mode 1)
          (magit-refresh)
          (should (magit-gerrit-test--wait-for
                   (lambda () (string-match-p "Add a fancy feature" (buffer-string)))))
          (should-not (string-match-p "Loading" (buffer-string)))
          (let ((magit-gerrit-async-min-interval 0) (magit-gerrit-query-filters "change:bad"))
            (magit-refresh)
            (should (magit-gerrit-test--wait-for
                     (lambda () (string-match-p "HTTP 400, bad query" (buffer-string))))))
          (magit-gerrit-mode -1))))))

(ert-deftest magit-gerrit-rest-test-async-refresh-postponed ()
  ;; Synchronous REST requests process the events: a fetch in the
  ;; background can complete while a buffer is being refreshed, the
  ;; refresh of the status buffer must then wait
  (let ((default-directory (magit-gerrit-rest-test--repo))
        refreshed)
    (with-current-buffer (magit-status-setup-buffer default-directory)
      (cl-letf (((symbol-function 'magit-refresh-buffer) (lambda (&rest _) (push (current-buffer) refreshed))))
        (let ((magit-insert-section--parent t))
          (magit-gerrit--async-refresh-status (magit-toplevel)))
        (should-not refreshed)
        (should (magit-gerrit-test--wait-for (lambda () refreshed)))
        (should (equal refreshed (list (current-buffer))))))))

(ert-deftest magit-gerrit-rest-test-commit-msg-hook ()
  (magit-gerrit-rest-test--with-config
    (let* ((default-directory (magit-gerrit-rest-test--repo))
           (magit-gerrit-commit-msg-hook-methods '(scp https))
           (hook (expand-file-name ".git/hooks/commit-msg" default-directory)))
      (with-temp-buffer
        ;; no SSH: scp is skipped, the hook comes from the REST server
        (should (equal (magit-gerrit-install-commit-msg-hook t) hook))
        (should (string-match-p "(rest)" (with-temp-buffer (insert-file-contents hook) (buffer-string))))))))


;;; Comments on diffs, and the features of the backends

(ert-deftest magit-gerrit-rest-test-diff-comment-position ()
  (let* ((default-directory (magit-gerrit-test--dir 'local))
         (base (magit-gerrit-test--git default-directory "rev-parse" "HEAD")))
    (with-temp-file (expand-file-name "c" default-directory) (insert "C\nd\n"))
    (magit-gerrit-test--git default-directory "commit" "-q" "-a" "-m" "Edit c")
    (unwind-protect
        (let ((rev (magit-gerrit-test--git default-directory "rev-parse" "HEAD"))
              (magit-gerrit-ssh-creds "me@host"))
          (with-current-buffer (magit-diff-range (format "%s^..%s" rev rev) nil '("c"))
            (should (equal (magit-gerrit--diff-revision) rev))
            (magit-gerrit-rest-test--goto "^-c$")
            (should (equal (magit-gerrit--diff-position) '("c" 1 "PARENT")))
            (magit-gerrit-rest-test--goto "^\\+d$")
            (should (equal (magit-gerrit--diff-position) '("c" 2 "REVISION")))
            (magit-gerrit-rest-test--goto "^@@")
            (should (equal (magit-gerrit--diff-position) '("c" 1 "REVISION")))
            (magit-gerrit-rest-test--goto "^modified +c")
            (should (equal (magit-gerrit--diff-position) '("c" nil "REVISION")))
            ;; comment with the SSH backend: "gerrit review --json" with the comment
            (setq-local magit-gerrit--diff-change
                        (cons rev '((number . 1234) (project . "foo/bar")
                                    (currentPatchSet (revision . "current")))))
            (magit-gerrit-rest-test--goto "^-c$")
            (let ((magit-gerrit-read-message-function (lambda (_p _c _i callback) (funcall callback "Why?"))))
              (magit-gerrit-comment '("--message=See inline")))
            (should (equal (magit-gerrit-test--last-argv)
                           (list "gerrit" "review" "--project" "foo/bar" "--json" rev)))
            (should (equal (magit-gerrit-test--last-stdin)
                           '((comments (c ((line . 1) (side . "PARENT") (message . "Why?") (unresolved . t))))
                             (message . "See inline"))))))
      (magit-gerrit-test--git default-directory "reset" "-q" "--hard" base))))

(ert-deftest magit-gerrit-rest-test-features ()
  (let ((ssh (magit-gerrit-ssh-backend-create :creds "me@host"))
        (rest (magit-gerrit-rest-backend-create :url "https://h"))
        (change '((number . 1) (project . "p") (currentPatchSet (revision . "r")))))
    ;; every feature is known
    (dolist (backend (list ssh rest))
      (dolist (feature (magit-gerrit-backend-features backend))
        (should (assq feature magit-gerrit-features))))
    (should (memq 'comment (magit-gerrit-backend-features ssh)))
    (dolist (feature '(reply drafts hashtags star private edit-message revert cherry-pick suggest-accounts))
      (should-not (memq feature (magit-gerrit-backend-features ssh)))
      (should (memq feature (magit-gerrit-backend-features rest))))
    ;; unsupported operations signal a clear user-error
    (should (equal (cadr (should-error (magit-gerrit-backend-set-hashtags ssh change '("a") nil)
                                       :type 'user-error))
                   "The SSH backend doesn't support hashtags"))
    (should (string-match-p "doesn't support draft comments"
                            (cadr (should-error (magit-gerrit-backend-post-comments
                                                 ssh change "r" '((:file "a" :message "m")) '(:draft t))
                                                :type 'user-error))))
    (should (string-match-p "doesn't support replies"
                            (cadr (should-error (magit-gerrit-backend-post-comments
                                                 ssh change "r" '((:file "a" :message "m" :in-reply-to "x")) nil)
                                                :type 'user-error)))))
  ;; the frontend: commands and menu predicates follow the backend of the buffer
  (let ((default-directory (magit-gerrit-test--dir 'gerrit)))
    (with-temp-buffer
      (should (magit-gerrit--supports-comment-p))
      (should-not (magit-gerrit--supports-reply-p))
      (should-not (magit-gerrit--supports-drafts-p))
      (cl-letf (((symbol-function 'magit-gerrit-change-at-point)
                 (lambda () '((number . 1234) (project . "foo/bar")))))
        (should (string-match-p "SSH backend doesn't support starred changes"
                                (cadr (should-error (magit-gerrit-toggle-star) :type 'user-error))))
        (should-error (magit-gerrit-comment '("--draft")) :type 'user-error))
      (let ((magit-gerrit-rest-url "https://review.example.com"))
        (should (magit-gerrit--supports-reply-p))
        (should (magit-gerrit--supports-star-p)))))
  ;; the menu shows the unsupported commands as unavailable
  (should (eq (plist-get (cdr (transient-get-suffix 'magit-gerrit-dispatch "a")) :inapt-if-not)
              'magit-gerrit--supports-reply-p)))

(ert-deftest magit-gerrit-rest-test-message-buffer ()
  (let (result)
    (magit-gerrit-read-message-in-buffer "Reply to Bob" "> Can this overflow?" "Init"
                                         (lambda (message) (setq result message)))
    (with-current-buffer (window-buffer (selected-window))
      (should (derived-mode-p 'magit-gerrit-message-mode))
      (should (string-match-p "\\`> Can this overflow\\?\n\nReply to Bob (C-c C-c to send, C-c C-k to cancel):\nInit\\'"
                              (buffer-string)))
      ;; the header is read-only
      (goto-char (point-min))
      (should-error (insert "x") :type 'text-read-only)
      (goto-char (point-max))
      (insert " text \n")
      (magit-gerrit-message-send))
    (should (equal result "Init text"))
    (should-not (seq-find (lambda (b) (string-prefix-p "*magit-gerrit-message*" (buffer-name b)))
                          (buffer-list)))))

(provide 'magit-gerrit-rest-test)

;;; magit-gerrit-rest-test.el ends here
