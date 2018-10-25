EMACS ?= emacs
EMACSFLAGS += --directory .
CASK ?= cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)
DESTDIR ?= dist
EMACSBATCH := $(EMACS) --no-site-file --no-site-lisp --quick --batch $(EMACSFLAGS)

# Export Emacs to goals, mainly for CASK
CASK_EMACS := $(EMACS)
export EMACS
export CASK_EMACS

SRCS := xcode-parser.el xcode-project.el
OBJS := $(SRCS:.el=.elc)
TEST_DIR := test
TEST_SRCS := xcode-parser-test.el xcode-project-test.el

.PHONY : all
all : $(OBJS)

.PHONY : clean
clean :
	rm -rf $(OBJS)

.PHONY : distclean
distclean : clean
	rm -rf $(DESTDIR)
	rm -rf .cask

.PHONY : dist
dist :
	$(CASK) package $(DESTDIR)

.PHONY : test
test : all $(SWIFT_MODE_SRCS)
	$(CASK) exec $(EMACSBATCH) \
		$(TEST_SRCS:%=--load $(TEST_DIR)/%) \
		--funcall ert-run-tests-batch-and-exit

$(PKGDIR) : Cask
	$(CASK) install
	touch $(@)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) \
	-f batch-byte-compile $<
