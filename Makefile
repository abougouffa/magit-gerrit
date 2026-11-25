# Development tasks for magit-gerrit.
#
#   make deps     install the dependencies in $(ELPA_DIR) (needs network)
#   make all      byte-compile, checkdoc, package-lint and tests
#
# To use dependencies installed elsewhere, override INIT, e.g.:
#   make test INIT="-L /path/to/magit/lisp -L /path/to/transient/lisp ..."

EMACS    ?= emacs
ELPA_DIR ?= $(CURDIR)/.elpa
INIT     ?= --eval '(progn (setq package-user-dir "$(ELPA_DIR)") (package-initialize))'
BATCH     = $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" $(INIT) -L .

MAIN = magit-gerrit.el
EL   = $(MAIN) magit-gerrit-ssh.el magit-gerrit-rest.el
ELC  = $(EL:.el=.elc)
TESTS = test/magit-gerrit-test.el test/magit-gerrit-rest-test.el

.PHONY: all deps compile checkdoc lint test clean

all: compile checkdoc lint test

deps:
	$(EMACS) -Q --batch \
	  --eval '(setq package-user-dir "$(ELPA_DIR)")' \
	  --eval "(require 'package)" \
	  --eval '(add-to-list (quote package-archives) (quote ("melpa" . "https://melpa.org/packages/")) t)' \
	  --eval '(package-initialize)' \
	  --eval '(package-refresh-contents)' \
	  --eval "(dolist (p '(magit transient package-lint)) (package-install p))"

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

checkdoc:
	@$(BATCH) --eval "(require 'checkdoc)" \
	  $(foreach f,$(EL),--eval '(checkdoc-file "$(f)")') 2>&1 | tee checkdoc.log
	@if grep -q "^Warning\|^[^ ]*\.el:[0-9]" checkdoc.log; then rm -f checkdoc.log; exit 1; fi
	@rm -f checkdoc.log

# The secondary files are linted against the package headers of $(MAIN)
lint:
	$(BATCH) --eval "(require 'package-lint)" -f package-lint-batch-and-exit $(MAIN)
	$(BATCH) --eval "(require 'package-lint)" \
	  --eval '(setq package-lint-main-file "$(MAIN)")' \
	  -f package-lint-batch-and-exit $(filter-out $(MAIN),$(EL))

test:
	$(BATCH) -L test $(foreach f,$(TESTS),-l $(f)) -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELC) test/*.elc
