# Generic Makefile
PKGNAME = mindstorm
INTERFACES = mindstorm.mli
DOC_DIR = _build/API.docdir
WEB_DIR = web
WEB 	= forge.ocamlcore.org:/home/groups/ocaml-mindstorm/htdocs
INSTALL_FILES = mindstorm.mli mindstorm.cmi mindstorm.cma \
  mindstorm.cmx mindstorm.cmxa mindstorm.a
DISTFILES = $(INTERFACES) $(wildcard *.ml *.h *.c Makefile*) tests/ examples/

VERSION=$(shell grep "Version" _oasis | sed "s/[^0-9]*//")
ROOT_TARBALL=$(PKGNAME)-$(VERSION)
PKG_TARBALL=$(PKGNAME)-$(VERSION).tar.gz

.PHONY: all byte native tests examples
all: byte native
byte native:
	$(MAKE) -C src $@

tests: byte
	$(MAKE) -B -C tests byte

examples: byte
	$(MAKE) -C examples -B byte

configure: setup.ml
	ocaml $< -configure

setup.ml src/META: _oasis
	oasis setup -setup-update dynamic

# Install

install: src/META all
	$(OCAMLFIND) install $(PKGNAME) META $(INSTALL_FILES)

uninstall:
	$(OCAMLFIND) remove $(PKGNAME)

# Generate HTML documentation
.PHONY: doc
doc: configure
	ocaml setup.ml -doc

# Publish the doc to OCamlCore
.PHONY: web web-doc website website-img
web: web-doc website website-img
web-doc: doc
	@ if [ -d $(DOC_DIR) ] ; then \
	  scp $(wildcard $(DOC_DIR)/*.html $(DOC_DIR)/*.css) $(WEB)/doc/ \
	  && echo "--- Published documentation on OCamlForge." ; \
	fi

website:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(wildcard $(WEB_DIR)/*.html $(WEB_DIR)/*.css) LICENSE $(WEB) \
	  && echo "--- Published web site (in $(WEB_DIR)/) on OCamlForge." ; \
	fi

website-img:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(wildcard $(WEB_DIR)/*.png $(WEB_DIR)/*.jpg) $(WEB) \
	  && echo "--- Published images (in $(WEB_DIR)/) on OCamlForge." ; \
	fi

.PHONY: dist tar
dist: tar
# "Force" a tag to be defined for each released tarball
tar:
	@ TMP=`mktemp -d` && \
	bzr export "$$TMP/$(ROOT_TARBALL)" -r "tag:$(VERSION)" && \
	cd $$TMP && \
	$(RM) -rf $(ROOT_TARBALL)/doc/Lego/ $(ROOT_TARBALL)/bin \
	  $(ROOT_TARBALL)/web && \
	tar -zcf /tmp/$(PKG_TARBALL) $(ROOT_TARBALL) && \
	$(RM) -rf $$TMP
	@echo "Created tarball '/tmp/$(PKG_TARBALL)'."


.PHONY: clean
clean::
	-ocaml setup.ml -clean
	$(MAKE) -C src $@
	$(MAKE) -C tests $@
	$(MAKE) -C examples $@
	$(RM) $(PKG_TARBALL) setup.data setup.log
