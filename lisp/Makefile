-include ../config.mk
include ../default.mk

## ###################################################################

.PHONY: pcomplete-me-version.el install

all: lisp

pcomplete-me.elc:

## Build #############################################################

lisp:       $(ELCS) loaddefs
loaddefs:   $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	$$suppress_warnings)" \
	-f batch-byte-compile $<

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(BATCH) --eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

$(PKG)-version.el:
	@printf "Generating $@\n"
	@printf "%s" "$$VERSIONLIB_TMPL" > $@


## Clean #############################################################

clean:
	@printf "Cleaning lisp/*...\n"
	@$(RM) *.elc $(ELGS)
