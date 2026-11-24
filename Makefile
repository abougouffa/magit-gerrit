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
BATCH     = $(EMACS) -Q --batch $(INIT) -L .

EL  = magit-gerrit.el
ELC = $(EL:.el=.elc)

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
	  --eval '(checkdoc-file "$(EL)")' 2>&1 | tee checkdoc.log
	@if grep -q "^Warning" checkdoc.log; then rm -f checkdoc.log; exit 1; fi
	@rm -f checkdoc.log

lint:
	$(BATCH) --eval "(require 'package-lint)" -f package-lint-batch-and-exit $(EL)

test:
	$(BATCH) -L test -l test/magit-gerrit-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELC) test/*.elc
