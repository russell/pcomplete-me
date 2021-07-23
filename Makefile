-include config.mk
include default.mk

## ###################################################################

.PHONY: lisp \
	install install-lisp \
	test test-interactive pcomplete-me \
	clean clean-lisp clean-archives \
	stats bump-version melpa-post-release \
	dist versionlib pcomplete-me-$(VERSION).tar.gz

all: lisp


## Build #############################################################

lisp:
	@$(MAKE) -C lisp lisp

## Install ###########################################################

install: install-lisp

install-lisp: lisp
	@$(MAKE) -C lisp install

## Test ##############################################################

test:
	@$(BATCH) --eval "(progn\
	$$suppress_warnings\
	(load-file \"t/pcomplete-me-test.el\")\
	(load-file \"t/pcmpl-argo-test.el\")\
	(load-file \"t/pcmpl-kubectl-test.el\")\
	(ert-run-tests-batch-and-exit))"

## Clean #############################################################

clean: clean-lisp

clean-lisp:
	@$(MAKE) -C lisp clean

clean-all: clean

dist: pcomplete-me-$(VERSION).tar.gz

versionlib:
	@$(MAKE) -C lisp versionlib

DIST_ROOT_FILES = LICENSE default.mk Makefile README.md
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) pcomplete-me-version.el Makefile)

pcomplete-me-$(VERSION).tar.gz: lisp versionlib info
	@printf "Packing $@\n"
	@$(MKDIR) pcomplete-me-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) pcomplete-me-$(VERSION)
	@$(MKDIR) pcomplete-me-$(VERSION)/lisp
	@$(CP) $(DIST_LISP_FILES) pcomplete-me-$(VERSION)/lisp
	@$(TAR) cz --mtime=./pcomplete-me-$(VERSION) -f pcomplete-me-$(VERSION).tar.gz pcomplete-me-$(VERSION)
	@$(RMDIR) pcomplete-me-$(VERSION)

define set_package_requires
(with-temp-file "lisp/pcomplete-me-pkg.el"
  (insert (format
"(define-package \"pcomplete-me\" \"$(VERSION)\"\
  \"A simpler pcomplete.\"
  '((emacs %S)
    (bash-completion %S))
  :homepage \"https://github.com/russell/pcomplete-me\"
  :keywords '(\"tools\"))
"   emacs-version
    bash-completion-version))
  (goto-char (point-min))
  (re-search-forward " \"A")
  (goto-char (match-beginning 0))
  (insert "\n "))
endef
export set_package_requires

bump-versions: bump-versions-1 texi
bump-versions-1:
	@$(BATCH) --eval "(let (\
	(emacs-version \"$(EMACS_VERSION)\")\
        (bash-completion-version \"$(BASH_COMPLETION_VERSION)\")\
        $$set_package_requires)"

bump-snapshots:
	@$(BATCH) --eval "(let (\
	(emacs-version \"$(EMACS_VERSION)\")\
        (bash-completion-version \"$(BASH_COMPLETION_MELPA_SNAPSHOT)\")\
        $$set_package_requires)"
	@git commit -a --gpg-sign -m "Reset Package-Requires for Melpa"
