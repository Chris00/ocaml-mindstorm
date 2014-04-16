# Generic Makefile
PKGNAME = $(shell oasis query name)
VERSION = $(shell oasis query version)
DOC_DIR = _build/API.docdir
WEB_DIR = web
WEB 	= forge.ocamlcore.org:/home/groups/ocaml-mindstorm/htdocs
INSTALL_FILES = mindstorm.mli mindstorm.cmi mindstorm.cma \
  mindstorm.cmx mindstorm.cmxa mindstorm.a
DISTFILES = Makefile myocamlbuild.ml _oasis setup.ml _tags \
  $(wildcard $(addprefix src/, *.ml *.mli *.h *.c)) \
  tests/ examples/

ROOT_TARBALL=$(PKGNAME)-$(VERSION)
PKG_TARBALL=$(PKGNAME)-$(VERSION).tar.gz

.PHONY: all byte native configure doc test install uninstall reinstall \
  upload-doc

all byte native: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic

test doc install uninstall reinstall: all
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

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
dist tar:
	mkdir -p $(ROOT_TARBALL)
	for f in $(DISTFILES); do \
	  cp -r --parents $$f $(ROOT_TARBALL); \
	done
# Make a setup.ml independent of oasis:
	cd $(ROOT_TARBALL) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(ROOT_TARBALL)
	$(RM) -r $(ROOT_TARBALL)
	@echo "Created tarball '$(PKG_TARBALL)'."


.PHONY: clean
clean::
	-ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL) setup.data setup.log
