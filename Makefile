SETUP = ocaml setup.ml
ROOTDIR = $(realpath .)
DOCDIR = $(ROOTDIR)/doc
BUILDDIR = $(ROOTDIR)/_build
DOCSRCDIR = $(ROOTDIR)/src/compiler

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	mkdir -p $(DOCDIR)
	ocamldoc -html -d $(DOCDIR) -keep-code -charset utf-8 -colorize-code $(DOCSRCDIR)/*.ml{,i} -I $(BUILDDIR)/src/compiler/

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	rm -fR $(DOCDIR)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure
