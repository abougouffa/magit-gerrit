;;; magit-gerrit-test.el --- Tests for magit-gerrit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Abdelhak Bougouffa

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; Run with "make test".
;;
;; There is no Gerrit server: `magit-gerrit-ssh-program' is set to
;; test/fake-gerrit-ssh, which parses the command line like Gerrit's SSH
;; daemon, records it and answers queries with the JSON fixtures of
;; test/fixtures.  The Git repositories used by the tests are created in a
;; temporary directory:
;;
;; - "gerrit": a repository whose origin is a Gerrit SSH URL,
;; - "bare": a bare repository standing for the Gerrit server, with a
;;   change at refs/changes/34/1234/3,
;; - "local": a clone of "bare", its commits have Change-Ids.
;;
;; The tests need Git, Python 3 and a POSIX shell.

;;; Code:

(require 'ert)
(require 'magit-gerrit)

(defconst magit-gerrit-test--source-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the tests.")

(defconst magit-gerrit-test--root (make-temp-file "magit-gerrit-test-" t)
  "Temporary directory of the test repositories and state.")

(defconst magit-gerrit-test--scp
  (expand-file-name "fake-scp" magit-gerrit-test--source-dir)
  "Fake scp program.")

(defvar magit-gerrit-test--change-commit nil
  "Commit of the change 1234 (patchset 3) in the bare repository.")

(defvar magit-gerrit-test--last-url nil
  "Last URL retrieved by `magit-gerrit-test--fake-url-retrieve'.")

(defun magit-gerrit-test--dir (name)
  "Return the directory of the test repository NAME."
  (file-name-as-directory
   (expand-file-name (if (eq name 'bare) "bare.git" (symbol-name name))
                     magit-gerrit-test--root)))

(defun magit-gerrit-test--state (file)
  "Return the path of the state FILE of the fake programs."
  (expand-file-name file (expand-file-name "state" magit-gerrit-test--root)))

(defun magit-gerrit-test--git (dir &rest args)
  "Run git with ARGS in DIR, signal an error on failure.  Return its output."
  (with-temp-buffer
    (let ((default-directory dir))
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "Git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun magit-gerrit-test--setup ()
  "Create the test repositories, see the commentary."
  (make-directory (magit-gerrit-test--state "") t)
  (setenv "MAGIT_GERRIT_TEST_STATE" (magit-gerrit-test--state ""))
  (dolist (var '("GIT_AUTHOR_NAME=Tester" "GIT_AUTHOR_EMAIL=tester@example.com"
                 "GIT_COMMITTER_NAME=Tester" "GIT_COMMITTER_EMAIL=tester@example.com"
                 "GIT_CONFIG_NOSYSTEM=1"))
    (apply #'setenv (split-string var "=")))
  (setenv "GIT_CONFIG_GLOBAL" (magit-gerrit-test--state "gitconfig"))
  (let ((gerrit (magit-gerrit-test--dir 'gerrit))
        (bare (magit-gerrit-test--dir 'bare))
        (seed (expand-file-name "seed/" magit-gerrit-test--root))
        (local (magit-gerrit-test--dir 'local)))
    (make-directory gerrit t)
    (magit-gerrit-test--git gerrit "init" "-q" "-b" "main")
    (magit-gerrit-test--git gerrit "commit" "-q" "--allow-empty" "-m" "init")
    (magit-gerrit-test--git gerrit "remote" "add" "origin"
                            "ssh://me@review.example.com:29418/foo/bar.git")
    (magit-gerrit-test--git magit-gerrit-test--root "init" "-q" "--bare" "-b" "main" bare)
    (magit-gerrit-test--git magit-gerrit-test--root "clone" "-q" bare seed)
    (with-temp-file (expand-file-name "a" seed) (insert "a\n"))
    (magit-gerrit-test--git seed "add" "a")
    (magit-gerrit-test--git seed "commit" "-q" "-m" "base")
    (magit-gerrit-test--git seed "push" "-q" "origin" "main")
    (with-temp-file (expand-file-name "a" seed) (insert "a\nb\n"))
    (magit-gerrit-test--git seed "commit" "-q" "-a" "-m" "Change one" "-m"
                            "Change-Id: I1111111111111111111111111111111111111111")
    (magit-gerrit-test--git seed "push" "-q" "origin" "HEAD:refs/changes/34/1234/3")
    (setq magit-gerrit-test--change-commit (magit-gerrit-test--git seed "rev-parse" "HEAD"))
    (magit-gerrit-test--git magit-gerrit-test--root "clone" "-q" bare local)
    (with-temp-file (expand-file-name "c" local) (insert "c\n"))
    (magit-gerrit-test--git local "add" "c")
    (magit-gerrit-test--git local "commit" "-q" "-m" "Local work" "-m"
                            "Change-Id: I2222222222222222222222222222222222222222
Change-Id: I3333333333333333333333333333333333333333")))

(magit-gerrit-test--setup)
(setq magit-gerrit-ssh-program
      (expand-file-name "fake-gerrit-ssh" magit-gerrit-test--source-dir))

(defun magit-gerrit-test--read-json (file)
  "Read the JSON state FILE."
  (json-parse-string (with-temp-buffer
                       (insert-file-contents (magit-gerrit-test--state file))
                       (buffer-string))
                     :object-type 'alist :array-type 'list))

(defun magit-gerrit-test--last-argv ()
  "Return the last command line received by the fake Gerrit, as parsed by it."
  (magit-gerrit-test--read-json "last.json"))

(defun magit-gerrit-test--last-stdin ()
  "Return the JSON received on the standard input of the last command."
  (magit-gerrit-test--read-json "stdin.json"))

(defun magit-gerrit-test--wait-for (predicate)
  "Wait for PREDICATE to return non-nil (10 seconds at most), return its value."
  (let ((n 0))
    (while (and (not (funcall predicate)) (< n 200))
      (accept-process-output nil 0.05)
      (cl-incf n))
    (funcall predicate)))

(defun magit-gerrit-test--decode-message (token)
  "Decode the push option message TOKEN like Gerrit does.
Gerrit replaces \"_\" by spaces, then decodes it with Java's
URLDecoder, which also replaces \"+\" by spaces."
  (decode-coding-string
   (url-unhex-string (replace-regexp-in-string "[_+]" " " token))
   'utf-8))

(defun magit-gerrit-test--fake-url-retrieve (url &rest _)
  "Fake `url-retrieve-synchronously' serving the commit-msg hook under /r/."
  (setq magit-gerrit-test--last-url url)
  (let ((buffer (generate-new-buffer " *fake-http*")))
    (with-current-buffer buffer
      (if (string-prefix-p "https://review.example.com/r/" url)
          (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"
                  "#!/bin/sh\n# gerrit hook (https)\n")
        (insert "HTTP/1.1 404 Not Found\r\n\r\nNot found\n")))
    buffer))

(ert-deftest magit-gerrit-test-quoting-roundtrip ()
  (let ((magit-gerrit-ssh-creds "me@host"))
    (dolist (s '("simple" "with space" "it's" "\"dq\"" "back\\slash" "(status:open OR (status:merged AND -age:6months))" "tab\there" "mix 'a' \"b\" \\c" "ünï 日本"))
      (magit-gerrit--ssh-call "review" "--message" s "rev")
      (should (equal (magit-gerrit-test--last-argv) (list "gerrit" "review" "--message" s "rev"))))))
(ert-deftest magit-gerrit-test-nil-args-dropped ()
  (let ((magit-gerrit-ssh-creds "me@host"))
    (magit-gerrit--ssh-call "review" nil '("--a" nil ("--b" "x")) 12)
    (should (equal (magit-gerrit-test--last-argv) '("gerrit" "review" "--a" "--b" "x" "12")))))
(ert-deftest magit-gerrit-test-failure-signals-user-error ()
  (let ((magit-gerrit-ssh-creds "me@host"))
    (should (string-match-p "not a valid option"
                            (cadr (should-error (magit-gerrit--ssh-call "review" "fail") :type 'user-error))))
    (should (with-current-buffer magit-gerrit-log-buffer-name (string-match-p "=> 1" (buffer-string))))))
(ert-deftest magit-gerrit-test-query-parses ()
  (let* ((magit-gerrit-ssh-creds "me@host")
         (changes (magit-gerrit--query-changes '("project:foo/bar" "status:open") "--current-patch-set")))
    (should (= (length changes) 2))
    (should (equal (alist-get 'number (car changes)) 1234))
    (should (equal (magit-gerrit-test--last-argv) '("gerrit" "query" "--format=JSON" "--current-patch-set" "project:foo/bar" "status:open")))
    (should-error (magit-gerrit--query-changes "change:bad") :type 'user-error)))
(ert-deftest magit-gerrit-test-query-terms ()
  (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "foo/bar")))
    (let ((magit-gerrit-extra-options "is:watched is:owner") (magit-gerrit-query-filters nil))
      (should (equal (magit-gerrit--query-terms) '("project:foo/bar" "is:watched" "is:owner" "status:open"))))
    (let ((magit-gerrit-extra-options '("is:starred")) (magit-gerrit-query-filters "(a OR b)"))
      (should (equal (magit-gerrit--query-terms) '("project:foo/bar" "is:starred" "(a OR b)"))))))
(ert-deftest magit-gerrit-test-section-renders ()
  (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "foo/bar"))
            ((symbol-function 'window-width) (lambda (&rest _) 140)))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t) (magit-gerrit-ssh-creds "me@host"))
        (magit-insert-section (status)
          (magit-gerrit-insert-reviews-section)))
      (princ (buffer-string))
      (should (string-match-p "1234 +\\[3\\]" (buffer-string)))
      (goto-char (point-min)) (search-forward "1240")
      (should (equal (alist-get 'number (magit-gerrit-change-at-point)) 1240)))))
(ert-deftest magit-gerrit-test-section-shows-error ()
  (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "fail")))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t) (magit-gerrit-ssh-creds "me@host")
            (magit-gerrit-query-filters "fail"))
        (magit-insert-section (status) (magit-gerrit-insert-reviews-section)))
      (should (string-match-p "failed: fatal" (buffer-string))))))

(ert-deftest magit-gerrit-test-accessors ()
  (let* ((magit-gerrit-ssh-creds "me@host")
         (c (car (magit-gerrit--query-changes "x"))))
    (should (equal (magit-gerrit-change-revision c) "aaaabbbbccccddddeeeeffff0000111122223333"))
    (should (equal (magit-gerrit-change-ref c) "refs/changes/34/1234/3"))
    (should (= (magit-gerrit-change-deletions c) 7))
    (should (equal (magit-gerrit-change-owner-username c) "jane"))
    (should-not (magit-gerrit-change-wip-p c))
    (should (magit-gerrit-change-wip-p (cadr (magit-gerrit--query-changes "x"))))))
(ert-deftest magit-gerrit-test-review-command-args ()
  (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "foo/bar"))
            ((symbol-function 'magit-refresh) #'ignore)
            ((symbol-function 'window-width) (lambda (&rest _) 140)))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t) (magit-gerrit-ssh-creds "me@host"))
        (magit-insert-section (status) (magit-gerrit-insert-reviews-section))
        (goto-char (point-min)) (search-forward "1234")
        (magit-gerrit-abandon-review '("--message=Not needed, it's obsolete"))
        (should (equal (magit-gerrit-test--last-argv) '("gerrit" "review" "--project" "foo/bar" "--abandon" "--message" "Not needed, it's obsolete" "aaaabbbbccccddddeeeeffff0000111122223333")))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Code-Review=-1")))
          (magit-gerrit-code-review nil))
        (should (equal (magit-gerrit-test--last-argv) '("gerrit" "review" "--project" "foo/bar" "--label" "Code-Review=-1" "aaaabbbbccccddddeeeeffff0000111122223333")))
        (goto-char (point-min))
        (should-error (magit-gerrit-restore-review nil) :type 'user-error)))))

(ert-deftest magit-gerrit-test-mode-integration ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit)))
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
      (magit-gerrit-auto-enable)
      (unwind-protect
          (with-current-buffer (magit-status-setup-buffer (magit-gerrit-test--dir 'gerrit))
            (should magit-gerrit-mode)
            (should (equal magit-gerrit-ssh-creds "me@review.example.com"))
            (should (equal (magit-gerrit-get-project) "foo/bar"))
            (should (string-match-p "Reviews \\[status:open\\] (2)" (buffer-string)))
            (should (eq (key-binding "_") 'magit-gerrit-dispatch))
            (should-not (eq (lookup-key magit-mode-map "_") 'magit-gerrit-dispatch))
            ;; RET on a change browses it, RET elsewhere is untouched
            (goto-char (point-min)) (search-forward "1240")
            (should (equal (magit-gerrit-change-number (magit-gerrit-change-at-point)) 1240))
            (should (eq (key-binding (kbd "RET")) 'magit-gerrit-show-change))
            (goto-char (point-min))
            (should-not (eq (key-binding (kbd "RET")) 'magit-gerrit-browse-review))
            ;; magit-dispatch entry
            (should (transient-get-suffix 'magit-dispatch 'magit-gerrit-dispatch))
            ;; disabling removes the section and binding
            (magit-gerrit-mode -1)
            (magit-refresh)
            (should-not (string-match-p "Reviews" (buffer-string)))
            (should-not (eq (key-binding "_") 'magit-gerrit-dispatch))
            ;; re-enabling works
            (magit-gerrit-mode 1) (magit-refresh)
            (should (string-match-p "Reviews \\[status:open\\] (2)" (buffer-string))))
          ;; outside of Magit: error and the mode stays disabled
          (with-temp-buffer
            (should-error (magit-gerrit-mode 1) :type 'user-error)
            (should-not magit-gerrit-mode))
        (remove-hook 'magit-status-mode-hook #'magit-gerrit-check-enable)
        (remove-hook 'magit-log-mode-hook #'magit-gerrit-check-enable)))))
(ert-deftest magit-gerrit-test-prefix-customization ()
  (setopt magit-gerrit-popup-prefix "C-c g")
  (should (eq (keymap-lookup magit-gerrit-mode-map "C-c g") 'magit-gerrit-dispatch))
  (should-not (keymap-lookup magit-gerrit-mode-map "_"))
  (setopt magit-gerrit-popup-prefix "_")
  (should-not (keymap-lookup magit-gerrit-mode-map "C-c g")))

(ert-deftest magit-gerrit-test-format-time ()
  (should (equal (magit-gerrit-format-short-timestring 30) "just now"))
  (should (equal (magit-gerrit-format-short-timestring 60) " 1 minute"))
  (should (equal (magit-gerrit-format-short-timestring 7200) " 2 hours"))
  (should (equal (magit-gerrit-format-short-timestring (* 400 86400)) " 1 year"))
  (should (equal (magit-gerrit-format-short-timestring (* 3 365 86400)) " 3 years")))
(ert-deftest magit-gerrit-test-format-alignment ()
  (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
    (let* ((magit-gerrit-ssh-creds "me@host")
           (changes (magit-gerrit--query-changes "x"))
           (lines (mapcar #'magit-gerrit--format-review changes))
           (header (magit-gerrit-pretty-print-review-header)))
      (should (string-match-p "\\+42 -7 " (car lines)))
      (dolist (l (cons header lines))
        (should (= (string-width (string-trim-right l "\n")) 139))))))
(ert-deftest magit-gerrit-test-json-fallback ()
  (let ((line "{\"a\":1,\"b\":[1,2],\"c\":null,\"d\":false,\"e\":{\"f\":\"g\"}}"))
    (should (equal (magit-gerrit--json-parse line) '((a . 1) (b 1 2) (c) (d) (e (f . "g")))))
    (cl-letf (((symbol-function 'json-available-p) #'ignore))
      (should (equal (magit-gerrit--json-parse line) '((a . 1) (b 1 2) (c) (d) (e (f . "g"))))))))
(ert-deftest magit-gerrit-test-change-id-and-git-ops ()
  (let* ((default-directory (magit-gerrit-test--dir 'local))
         (change '((number . 1234) (project . "p") (topic . "feat") (owner (username . "jane"))
                   (currentPatchSet (number . 3) (ref . "refs/changes/34/1234/3")))))
    (with-temp-buffer
      (should (equal (magit-gerrit-commit-change-id "HEAD") "I2222222222222222222222222222222222222222"))
      (should-not (magit-gerrit-commit-change-id "HEAD~1"))
      (cl-letf (((symbol-function 'magit-gerrit-change-at-point) (lambda () change))
                ((symbol-function 'magit-refresh) #'ignore))
        (setq-local magit-gerrit-remote "origin")
        (should (equal (magit-gerrit-fetch-patchset) "refs/changes/34/1234/3"))
        (should (equal (magit-git-string "log" "-1" "--format=%s" "FETCH_HEAD") "Change one"))
        (magit-gerrit-copy-ref)
        (should (equal (current-kill 0) "refs/changes/34/1234/3"))
        (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 2)))
          (should (equal (magit-gerrit-get-ref t) "refs/changes/34/1234/2")))
        (magit-gerrit-download-patchset)
        (should (equal (magit-get-current-branch) "review/jane/feat-1234/3"))
        (should (equal (magit-git-string "log" "-1" "--format=%s") "Change one"))
        (magit-call-git "checkout" "-q" "main")
        (should-error (magit-gerrit-fetch-patchset "refs/changes/99/9999/1") :type 'user-error)))))

(ert-deftest magit-gerrit-test-copy-url-and-message-without-commit-message ()
  (let ((change '((number . 7) (subject . "Fix it") (url . "https://g/c/p/+/7"))))
    (cl-letf (((symbol-function 'magit-gerrit-change-at-point) (lambda () change)))
      (magit-gerrit-copy-review-url-commit-message)
      (should (equal (current-kill 0) "7: Fix it | https://g/c/p/+/7")))))

(ert-deftest magit-gerrit-test-change-buffer ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit)))
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
      (with-current-buffer (magit-status-setup-buffer (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit-mode 1) (magit-refresh)
        (goto-char (point-min)) (search-forward "1234")
        (should (eq (key-binding (kbd "RET")) 'magit-gerrit-show-change))
        (call-interactively #'magit-gerrit-show-change))
      (with-current-buffer (magit-get-mode-buffer 'magit-gerrit-change-mode 1234)
        (let ((text (buffer-string)))
          (princ text)
          (should (equal (magit-gerrit-test--last-argv)
                         '("gerrit" "query" "--format=JSON" "--current-patch-set" "--patch-sets" "--all-approvals" "--files" "--comments" "--commit-message" "--dependencies" "--submit-records" "--all-reviewers" "change:1234")))
          (dolist (re '("^Status: +NEW (submit: NOT_READY)" "^Hashtags: +perf, ui" "^Code-Review +OK +\\+2 Bob"
                        "^Verified +REJECT +-1 CI" "^Change-Id: +I0123" "Requirement: Verified is rejected" "^Files (2)" "modified +\\+40 -7 +src/a.c"
                        "^Patchsets (2)" "^Messages (2)" "^1240 +refs/changes/40/1240/1  (outdated patchset)" "Can this overflow"))
            (should (string-match-p re text)))
          (should-not (string-match-p "COMMIT_MSG" text)))
        (should (equal magit-gerrit-ssh-creds "me@review.example.com"))
        (should (eq (key-binding "_") 'magit-gerrit-dispatch))
        ;; the change at point is the buffer's change, also outside of its root section
        (goto-char (point-max)) (forward-line -2)
        (should (equal (magit-gerrit-change-number (magit-gerrit-change-at-point)) 1234))
        ;; patchset at point gives its ref
        (goto-char (point-min)) (re-search-forward "^1   1111aaaa")
        (should (equal (magit-gerrit-get-ref) "refs/changes/34/1234/1"))
        (should (eq (key-binding (kbd "RET")) 'magit-gerrit-show-patchset))
        ;; dependency
        (goto-char (point-min)) (re-search-forward "^1230")
        (should (eq (key-binding (kbd "RET")) 'magit-gerrit-show-change))
        (call-interactively #'magit-gerrit-show-change)
        (should (member "change:1230" (magit-gerrit-test--last-argv)))))))
(ert-deftest magit-gerrit-test-change-buffer-error ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit)))
    (with-current-buffer (magit-status-setup-buffer (magit-gerrit-test--dir 'gerrit))
      (magit-gerrit-mode 1)
      (cl-letf (((symbol-function 'magit-gerrit--query-changes) (lambda (&rest _) nil)))
        (magit-gerrit-show-change 999))
      (with-current-buffer (magit-get-mode-buffer 'magit-gerrit-change-mode 999)
        (should (string-match-p "Change 999 not found" (buffer-string)))))))
(ert-deftest magit-gerrit-test-show-patchset-file ()
  (let ((default-directory (magit-gerrit-test--dir 'local))
        (ps `((number . 3) (revision . ,magit-gerrit-test--change-commit) (ref . "refs/changes/34/1234/3"))))
    (with-temp-buffer
      (setq-local magit-gerrit-remote "origin")
      (ignore-errors (delete-file (expand-file-name ".git/FETCH_HEAD")))
      (magit-gerrit-show-patchset-file "a" ps)
      (with-current-buffer (magit-get-mode-buffer 'magit-diff-mode)
        (should (string-match-p "^\\+b" (buffer-string)))))))

(ert-deftest magit-gerrit-test-vote ()
  (let* ((magit-gerrit-ssh-creds "me@host")
         (magit-gerrit-label-values '(("Code-Review" -2 . 2) ("Verified" -1 . 1) ("Zuul-Gate" -1 . 1)))
         (change (car (magit-gerrit--query-changes "change:1234" "--patch-sets"))))
    (should (equal (magit-gerrit--label-votes "Code-Review")
                   '("Code-Review=+2" "Code-Review=+1" "Code-Review=0" "Code-Review=-1" "Code-Review=-2")))
    (should (equal (magit-gerrit--label-votes "Unknown") '("Unknown=+1" "Unknown=0" "Unknown=-1")))
    (cl-letf (((symbol-function 'magit-gerrit-change-at-point) (lambda () change))
              ((symbol-function 'magit-refresh) #'ignore)
              ((symbol-function 'magit-fetch-all-no-prune) #'ignore))
      (magit-gerrit-vote '("Code-Review=+2" "Zuul-Gate=+1") '("--message=LGTM, thanks!" "--notify=OWNER" "--submit"))
      (should (equal (magit-gerrit-test--last-argv)
                     '("gerrit" "review" "--project" "foo/bar" "--label" "Code-Review=+2" "--label" "Zuul-Gate=+1"
                       "--message" "LGTM, thanks!" "--notify" "OWNER" "--submit" "aaaabbbbccccddddeeeeffff0000111122223333")))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (should (member "Verified=-1" coll)) "Verified=-1")))
        (magit-gerrit-verify-review nil))
      (should (equal (magit-gerrit-test--last-argv) '("gerrit" "review" "--project" "foo/bar" "--label" "Verified=-1" "aaaabbbbccccddddeeeeffff0000111122223333")))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_p coll &rest _) (should (member "Code-Review=-2" coll)) (should (member "Verified=+1" coll)) '("Verified=+1"))))
        (call-interactively #'magit-gerrit-vote))
      (should (member "Verified=+1" (magit-gerrit-test--last-argv)))
      (should-error (magit-gerrit-vote nil nil) :type 'user-error))))
(ert-deftest magit-gerrit-test-score-uses-label-ranges ()
  (let ((magit-gerrit-label-values '(("Zuul-Gate" -1 . 1) ("Code-Review" -2 . 2))))
    (should (equal (substring-no-properties (magit-gerrit-trans-score "Zuul-Gate" '(1))) "✓"))
    (should (equal (substring-no-properties (magit-gerrit-trans-score "Code-Review" '(1))) "+1"))
    (should (equal (substring-no-properties (magit-gerrit-trans-score "Code-Review" '(-2 1))) "x"))))

(ert-deftest magit-gerrit-test-push-message-encoding ()
  (dolist (m '("Rebased on main" "snake_case + 100% sure, ok?" "ünï 日本 — dash" "a=b&c/d"))
    (let ((enc (magit-gerrit--encode-push-message m)))
      (should-not (string-match-p "[ ,_+]" enc))
      (should (equal (magit-gerrit-test--decode-message enc) m)))))
(ert-deftest magit-gerrit-test-push-refspec ()
  (should (equal (magit-gerrit--push-refspec "abc" "main" nil) "abc:refs/for/main"))
  (should (equal (magit-gerrit--push-refspec
                  "abc" "stable/1.0"
                  '("--topic=feat-x" "--hashtag=perf, ui" "--reviewers=bob@x.com,alice" "--cc=carl"
                    "--wip" "--private" "--notify=OWNER" "--message=Ready now" "--publish-comments"))
                 "abc:refs/for/stable/1.0%topic=feat-x,t=perf,t=ui,r=bob@x.com,r=alice,cc=carl,wip,private,publish-comments,notify=OWNER,m=Ready%20now"))
  (should-error (magit-gerrit--push-refspec "abc" "main" '("--topic=a b")) :type 'user-error)
  (should-error (magit-gerrit--push-refspec "abc" "main" '("--hashtag=a%b")) :type 'user-error))
(ert-deftest magit-gerrit-test-push-command ()
  (let ((default-directory (magit-gerrit-test--dir 'local)))
    (with-temp-buffer
      (setq-local magit-gerrit-remote "origin")
      (magit-call-git "branch" "-q" "--set-upstream-to=origin/main" "main")
      (should (equal (magit-gerrit--default-target-branch) "main"))
      (let (pushed)
        (cl-letf (((symbol-function 'magit-run-git-async) (lambda (&rest a) (setq pushed (flatten-tree a)))))
          (magit-gerrit-push-for-review '("--topic=t1" "--signed"))
          (should (equal pushed (list "push" "-v" "--signed" "origin"
                                      (concat (magit-rev-parse "HEAD") ":refs/for/main%topic=t1"))))
          (magit-gerrit-push-wip-for-review '("--ready" "--branch=dev"))
          (should (string-suffix-p ":refs/for/dev%wip" (car (last pushed))))))
      ;; git accepts the refspec syntax
      (should (zerop (magit-call-git "push" "-q" "origin"
                                     (magit-gerrit--push-refspec "HEAD" "main" '("--topic=x" "--message=Hi there"))))))
    (should (equal (magit-git-string (concat "--git-dir=" (magit-gerrit-test--dir 'bare)) "for-each-ref" "--format=%(refname)" "refs/for/")
                   "refs/for/main%topic=x,m=Hi%20there"))
    (magit-call-git (concat "--git-dir=" (magit-gerrit-test--dir 'bare)) "update-ref" "-d" "refs/for/main%topic=x,m=Hi%20there")))

(ert-deftest magit-gerrit-test-queries ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit)))
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
      (with-current-buffer (magit-status-setup-buffer (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit-mode 1) (magit-refresh)
        (should (string-match-p "^Reviews \\[status:open\\] (2)" (buffer-string)))
        (magit-gerrit-query-incoming)
        (should (equal (magit-gerrit-test--last-argv) '("gerrit" "query" "--format=JSON" "--current-patch-set" "--submit-records"
                                     "project:foo/bar" "reviewer:self -owner:self -is:wip status:open")))
        (should (string-match-p "^Reviews \\[reviewer:self -owner:self -is:wip status:open\\]" (buffer-string)))
        ;; topic of the change at point, quoted with braces
        (goto-char (point-min)) (search-forward "1234")
        (cl-letf (((symbol-function 'read-string) (lambda (_p _i _h default) default)))
          (call-interactively #'magit-gerrit-query-topic))
        (should (equal (car (last (magit-gerrit-test--last-argv))) "topic:{feat}"))
        (magit-gerrit-toggle-all-projects)
        (should-not (seq-some (lambda (a) (string-prefix-p "project:" a)) (magit-gerrit-test--last-argv)))
        (should (string-match-p "all projects\\]" (buffer-string)))
        (magit-gerrit-toggle-all-projects)
        (let ((magit-gerrit-saved-queries '(("Team" . "ownerin:{My Team} status:open"))))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Team")))
            (call-interactively #'magit-gerrit-query-saved)))
        (should (equal (car (last (magit-gerrit-test--last-argv))) "ownerin:{My Team} status:open"))
        ;; empty edit resets; the global value is untouched by local presets
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
          (magit-gerrit-edit-filters nil))
        (should (equal (car (last (magit-gerrit-test--last-argv))) "status:open"))
        (should-not (default-value 'magit-gerrit-query-filters))
        (magit-gerrit-query-merged t)
        (should (equal (default-value 'magit-gerrit-query-filters) "status:merged -age:1week"))
        (magit-gerrit-query-open t)))))

(ert-deftest magit-gerrit-test-change-actions ()
  (let* ((magit-gerrit-ssh-creds "me@host")
         (change (car (magit-gerrit--query-changes "change:1234" "--patch-sets" "--all-reviewers")))
         (rev "aaaabbbbccccddddeeeeffff0000111122223333"))
    (should (member "bob@example.com" (magit-gerrit--account-candidates)))
    (cl-letf (((symbol-function 'magit-gerrit-change-at-point) (lambda () change))
              ((symbol-function 'magit-refresh) #'ignore))
      (magit-gerrit-add-reviewer '("bob@example.com" "alice"))
      (should (equal (magit-gerrit-test--last-argv) '("gerrit" "set-reviewers" "--project" "foo/bar" "--add" "bob@example.com" "--add" "alice" "I0123456789abcdef0123456789abcdef01234567")))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_p coll &rest _) (should (equal coll '("bob@example.com" "ci"))) '("ci"))))
        (call-interactively #'magit-gerrit-remove-reviewer))
      (should (equal (magit-gerrit-test--last-argv) '("gerrit" "set-reviewers" "--project" "foo/bar" "--remove" "ci" "I0123456789abcdef0123456789abcdef01234567")))
      (magit-gerrit-add-cc '("carl@x.com" "dan"))
      (should (equal (magit-gerrit-test--last-argv) (list "gerrit" "review" "--project" "foo/bar" "--json" rev)))
      (should (equal (magit-gerrit-test--last-stdin) '((reviewers ((reviewer . "carl@x.com") (state . "CC")) ((reviewer . "dan") (state . "CC"))))))
      (magit-gerrit-mark-wip '("--message=Not yet — wait"))
      (should (equal (magit-gerrit-test--last-stdin) '((work_in_progress . t) (message . "Not yet — wait"))))
      (magit-gerrit-mark-ready nil)
      (should (equal (magit-gerrit-test--last-stdin) '((ready . t))))
      (magit-gerrit-set-topic " new-topic ")
      (should (equal (magit-gerrit-test--last-argv) '("gerrit" "set-topic" "foo/bar~main~I0123456789abcdef0123456789abcdef01234567" "--topic" "new-topic")))
      (magit-gerrit-set-topic "")
      (should (equal (magit-gerrit-test--last-argv) '("gerrit" "set-topic" "foo/bar~main~I0123456789abcdef0123456789abcdef01234567")))
      (magit-gerrit-rebase-review)
      (should (equal (magit-gerrit-test--last-argv) (list "gerrit" "review" "--project" "foo/bar" "--rebase" rev)))
      (magit-gerrit-move-review "stable" '("--message=Backport"))
      (should (equal (magit-gerrit-test--last-argv) (list "gerrit" "review" "--project" "foo/bar" "--move" "stable" "--message" "Backport" rev))))
    ;; reviewers queried when missing from the change
    (should (equal (length (magit-gerrit--change-reviewers '((number . 1234)))) 2))))

(ert-deftest magit-gerrit-test-change-id-batch-and-cache ()
  (let ((magit-gerrit-ssh-creds "me@host")
        (magit-gerrit--change-id-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "foo/bar")))
      (let ((res (magit-gerrit--get-change-ids-info
                  '("I0123456789abcdef0123456789abcdef01234567" "Inone" "I0123456789abcdef0123456789abcdef01234567"))))
        (should (equal (magit-gerrit-test--last-argv) '("gerrit" "query" "--format=JSON" "--current-patch-set" "project:foo/bar"
                                     "(change:I0123456789abcdef0123456789abcdef01234567 OR change:Inone)")))
        (should (= (length res) 2))
        (should (= (magit-gerrit-change-number (cadr (assoc "I0123456789abcdef0123456789abcdef01234567" res))) 1234))
        (should-not (cdr (assoc "Inone" res))))
      ;; cached: no new query, misses cached too
      (with-temp-file (magit-gerrit-test--state "last.json") (insert "[]"))
      (magit-gerrit--get-change-id-info "Inone")
      (should (equal (magit-gerrit-test--last-argv) nil))
      ;; expired
      (let ((magit-gerrit-change-cache-ttl 0))
        (magit-gerrit--get-change-id-info "Inone")
        (should (member "(change:Inone)" (magit-gerrit-test--last-argv)))))))
(ert-deftest magit-gerrit-test-change-summary ()
  (let ((magit-gerrit-ssh-creds "me@host"))
    (should (equal (substring-no-properties
                    (magit-gerrit--format-change-summary (car (magit-gerrit--query-changes "x"))))
                   "#1234 NEW CR+2 Ve-1"))))
(ert-deftest magit-gerrit-test-revision-and-log ()
  ;; a commit carrying the Change-Id of change 1234 in the Gerrit-like repo
  (let ((default-directory (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit--change-id-cache (make-hash-table :test #'equal))
        (magit-gerrit--rev-change-id-cache (make-hash-table :test #'equal))
        (magit-gerrit-annotate-log t))
    (magit-call-git "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "Feature"
                    "-m" "Change-Id: I0123456789abcdef0123456789abcdef01234567")
    (unwind-protect
        (progn
          (magit-gerrit-auto-enable)
          (with-current-buffer (magit-revision-setup-buffer "HEAD" nil nil)
            (should magit-gerrit-mode)
            (should (string-match-p "^Gerrit: +#1234 NEW CR\\+2 Ve-1 main https://review" (buffer-string)))
            (goto-char (point-min)) (search-forward "#1234")
            (should (equal (magit-gerrit-change-number (magit-gerrit-change-at-point)) 1234)))
          (with-current-buffer (magit-log-setup-buffer (list "HEAD") nil nil)
            (should magit-gerrit-mode)
            (let ((ovs (seq-filter (lambda (o) (overlay-get o 'magit-gerrit-annotation))
                                   (overlays-in (point-min) (point-max)))))
              (should (= (length ovs) 1))
              (should (equal (substring-no-properties (overlay-get (car ovs) 'after-string)) " #1234 NEW CR+2 Ve-1"))
              (goto-char (overlay-start (car ovs)))
              (should (string-match-p "Feature" (buffer-substring (line-beginning-position) (point)))))
            ;; refresh does not duplicate annotations
            (magit-refresh)
            (should (= 1 (length (seq-filter (lambda (o) (overlay-get o 'magit-gerrit-annotation))
                                             (overlays-in (point-min) (point-max))))))
            (goto-char (point-min)) (search-forward "Feature")
            (cl-letf (((symbol-function 'magit-gerrit-show-change) (lambda (n) (should (= n 1234)) 'shown)))
              (should (eq (call-interactively #'magit-gerrit-show-commit-change) 'shown)))))
      (remove-hook 'magit-status-mode-hook #'magit-gerrit-check-enable)
      (remove-hook 'magit-log-mode-hook #'magit-gerrit-check-enable)
      (remove-hook 'magit-revision-mode-hook #'magit-gerrit-check-enable)
      (magit-call-git "reset" "-q" "--hard" "HEAD~1"))))

(ert-deftest magit-gerrit-test-commit-msg-hook ()
  (let* ((default-directory (magit-gerrit-test--dir 'gerrit))
         (magit-gerrit-scp-program magit-gerrit-test--scp)
         (magit-gerrit-ssh-creds "me@review.example.com")
         (hook (expand-file-name ".git/hooks/commit-msg" (magit-gerrit-test--dir 'gerrit)))
         (read (lambda () (with-temp-buffer (insert-file-contents hook) (buffer-string)))))
    (with-temp-buffer
      (unwind-protect
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     #'magit-gerrit-test--fake-url-retrieve))
            ;; web URL deduced from a change URL
            (should (equal (magit-gerrit--web-url) "https://review.example.com"))
            (let ((magit-gerrit-web-url "https://review.example.com/r/"))
              (should (equal (magit-gerrit-install-commit-msg-hook t) hook))
              (should (equal magit-gerrit-test--last-url "https://review.example.com/r/tools/hooks/commit-msg"))
              (should (string-match-p "(https)" (funcall read)))
              (should (= (file-modes hook) #o755)))
            ;; https unavailable -> scp with -O
            (let ((magit-gerrit-web-url "https://review.example.com/missing"))
              (ignore-errors (delete-file (magit-gerrit-test--state "scp.log")))
              (magit-gerrit-install-commit-msg-hook t)
              (should (string-match-p "(scp)" (funcall read)))
              (should (equal (with-temp-buffer (insert-file-contents (magit-gerrit-test--state "scp.log")) (car (split-string (buffer-string) " ")))
                             "-O")))
            ;; old scp without -O
            (let ((magit-gerrit-commit-msg-hook-methods '(scp)) (process-environment (cons "FAKE_SCP_NO_O=1" process-environment)))
              (delete-file hook)
              (magit-gerrit-install-commit-msg-hook)
              (should (string-match-p "(scp)" (funcall read))))
            ;; failure
            (let ((magit-gerrit-commit-msg-hook-methods '(https)) (magit-gerrit-web-url "https://review.example.com/missing"))
              (should-error (magit-gerrit-install-commit-msg-hook t) :type 'user-error)))
        (ignore-errors (delete-file hook))))))
(ert-deftest magit-gerrit-test-push-change-id-check ()
  (let ((default-directory (magit-gerrit-test--dir 'local)) pushed asked)
    (with-temp-buffer
      (setq-local magit-gerrit-remote "origin")
      (magit-call-git "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "No id")
      (unwind-protect
          (cl-letf (((symbol-function 'magit-run-git-async) (lambda (&rest _) (setq pushed t)))
                    ((symbol-function 'magit-gerrit-install-commit-msg-hook) (lambda (&rest _) (setq asked 'installed))))
            ;; no hook: offered, installing aborts the push
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (should (string-match-p "install it" p)) t)))
              (magit-gerrit-push-for-review '("--branch=main")))
            (should (eq asked 'installed)) (should-not pushed)
            ;; declining both prompts pushes nothing; accepting "push anyway" pushes
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (string-match-p "anyway" p))))
              (magit-gerrit-push-for-review '("--branch=main")))
            (should pushed)
            (setq pushed nil)
            (let ((magit-gerrit-check-change-id nil))
              (magit-gerrit-push-for-review '("--branch=main")))
            (should pushed))
        (magit-call-git "reset" "-q" "--hard" "HEAD~1")))))

(ert-deftest magit-gerrit-test-async-reviews ()
  (let ((default-directory (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit-async-reviews t)
        (magit-gerrit--async-cache (make-hash-table :test #'equal))
        (process-environment (cons "FAKE_SSH_DELAY=0.5" process-environment)))
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 140)))
      (with-current-buffer (magit-status-setup-buffer (magit-gerrit-test--dir 'gerrit))
        (magit-gerrit-mode 1)
        (let ((start (float-time)))
          (magit-refresh)
          ;; the refresh doesn't wait for ssh
          (should (< (- (float-time) start) 0.4)))
        (should (string-match-p "Loading\\.\\.\\." (buffer-string)))
        (should (magit-gerrit-test--wait-for (lambda () (string-match-p "Add a fancy feature" (buffer-string)))))
        (should-not (string-match-p "Loading" (buffer-string)))
        ;; refreshes within the minimum interval reuse the result, no new process
        (ignore-errors (delete-file (magit-gerrit-test--state "calls.log")))
        (magit-refresh) (magit-refresh)
        (accept-process-output nil 0.8)
        (should-not (file-exists-p (magit-gerrit-test--state "calls.log")))
        (should (string-match-p "Add a fancy feature" (buffer-string)))
        ;; after the interval, refetch in the background while showing the old result
        (let ((magit-gerrit-async-min-interval 0))
          (magit-refresh)
          (should (string-match-p "Add a fancy feature" (buffer-string)))
          (should (magit-gerrit-test--wait-for (lambda () (file-exists-p (magit-gerrit-test--state "calls.log"))))))
        ;; errors are shown, keeping the previous changes
        (let ((magit-gerrit-async-min-interval 0) (magit-gerrit-query-filters "fail"))
          (accept-process-output nil 0.8)
          (magit-refresh)
          (should (magit-gerrit-test--wait-for (lambda () (string-match-p "failed: fatal" (buffer-string))))))
        (magit-gerrit-mode -1)))))

(ert-deftest magit-gerrit-test-gitiles ()
  (let ((default-directory (magit-gerrit-test--dir 'local))
        (magit-gerrit-gitiles-base-url "https://g.example.com/plugins/gitiles/")
        (head (magit-git-string "-C" (magit-gerrit-test--dir 'local) "rev-parse" "HEAD")))
    (cl-letf (((symbol-function 'magit-gerrit-get-project) (lambda () "foo/bar")))
      (with-temp-buffer
        (setq-local magit-gerrit-remote "origin")
        ;; without upstream: the remote's default branch
        (magit-call-git "branch" "-q" "--unset-upstream" "main")
        (should (equal (magit-gerrit--gitiles-branch) "main"))
        (should (equal (magit-gerrit--gitiles-url "refs/heads/main" "dir/a b+c.el" 3)
                       "https://g.example.com/plugins/gitiles/foo/bar/+/refs/heads/main/dir/a%20b%2Bc.el#3")))
      ;; file buffer, region start line, permalink
      (with-current-buffer (find-file-noselect (expand-file-name "a" (magit-gerrit-test--dir 'local)))
        (setq-local magit-gerrit-remote "origin")
        (erase-buffer) (insert "1\n2\n3\n4\n")
        (goto-char (point-min)) (forward-line 1) (set-mark (point)) (forward-line 2)
        (transient-mark-mode 1) (activate-mark)
        (should (equal (magit-gerrit--get-gitiles-link)
                       "https://g.example.com/plugins/gitiles/foo/bar/+/refs/heads/main/a#2"))
        (should (equal (magit-gerrit--get-gitiles-link t)
                       (format "https://g.example.com/plugins/gitiles/foo/bar/+/%s/a#2" head)))
        (set-buffer-modified-p nil) (kill-buffer))
      ;; commit at point in a log buffer
      (with-current-buffer (magit-log-setup-buffer (list "HEAD") nil nil)
        (setq-local magit-gerrit-remote "origin")
        (goto-char (point-min)) (search-forward "Local work")
        (should (equal (magit-gerrit--get-gitiles-link)
                       (format "https://g.example.com/plugins/gitiles/foo/bar/+/%s" head)))))))


(provide 'magit-gerrit-test)

;;; magit-gerrit-test.el ends here
