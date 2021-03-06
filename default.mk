TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/magit
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/magit
statsdir ?= $(TOP)/Documentation/stats

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo

BUILD_MAGIT_LIBGIT ?= true

## Files #############################################################

PKG       = pcomplete-me
PACKAGES  = pcomplete-me

ELS  = pcomplete-me.el
ELS += pcmpl-kubectl.el
ELS += bash-completion-export-utils.el
ELCS = $(ELS:.el=.elc)
ELGS = pcomplete-me-autoloads.el

## Versions ##########################################################

VERSION ?= $(shell \
  test -e $(TOP).git && \
  git describe --tags --abbrev=0 --always | cut -c2-)

DASH_VERSION          = 2.18.1
GIT_COMMIT_VERSION    = 3.1.0
LIBGIT_VERSION        = 0
MAGIT_LIBGIT_VERSION  = 0
MAGIT_SECTION_VERSION = 3.1.0
TRANSIENT_VERSION     = 0.3.6
WITH_EDITOR_VERSION   = 3.0.4

DASH_MELPA_SNAPSHOT          = 20210330
GIT_COMMIT_MELPA_SNAPSHOT    = 20210701
LIBGIT_MELPA_SNAPSHOT        = 0
MAGIT_LIBGIT_MELPA_SNAPSHOT  = 0
MAGIT_SECTION_MELPA_SNAPSHOT = 20210701
TRANSIENT_MELPA_SNAPSHOT     = 20210701
WITH_EDITOR_MELPA_SNAPSHOT   = 20210524

EMACS_VERSION = 25.1

LIBGIT_EMACS_VERSION = 26.1
LIBGIT_MAGIT_VERSION = $(VERSION)

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

ifndef LOAD_PATH

USER_EMACS_DIR = $(HOME)/.emacs.d
ifeq "$(wildcard $(USER_EMACS_DIR))" ""
  XDG_CONFIG_DIR = $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
  ifneq "$(wildcard $(XDG_CONFIG_DIR)/emacs)" ""
    USER_EMACS_DIR = $(XDG_CONFIG_DIR)/emacs
  endif
endif

ELPA_DIR ?= $(USER_EMACS_DIR)/elpa

SYSTYPE := $(shell $(EMACSBIN) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)lisp

# When making changes here, then don't forget to adjust "Makefile",
# ".github/workflows/test.yml", ".github/ISSUE_TEMPLATE/bug_report.md",
# `magit-emacs-Q-command' and the "Installing from the Git Repository"
# info node accordingly.  Also don't forget to "rgrep \b<pkg>\b".

endif # ifndef LOAD_PATH

## Publish ###########################################################

PUBLISH_TARGETS ?= html html-dir pdf epub

DOCBOOK_XSL ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl

EPUBTRASH = epub.xml META-INF OEBPS
