# Generic Makefile
PKGNAME = ocaml-mindstorm
INTERFACES = mindstorm.mli
DOC_DIR = doc
WEB_DIR = web
WEB 	= forge.ocamlcore.org:/home/groups/ocaml-mindstorm/htdocs
PKG = mindstorm
INSTALL_FILES = mindstorm.mli mindstorm.cmi mindstorm.cma \
  mindstorm.cmx mindstorm.cmxa mindstorm.a
DISTFILES = $(INTERFACES) $(wildcard *.ml *.h *.c Makefile*) tests/ examples/

CFLAGS= -Wall -fPIC $(D_HAS_USB) $(USB_INCLUDE)
OCAMLC_FLAGS = -g -dtypes

PP = camlp4o pa_macro.cmo $(D_OS) $(D_ARCH64) $(D_HAS_USB)

VERSION=$(shell grep "@version" mindstorm.mli | sed "s/[^0-9]*//")
ROOT_TARBALL=$(PKGNAME)-$(VERSION)
PKG_TARBALL=$(PKGNAME)-$(VERSION).tar.gz

.PHONY: all byte native
all: byte native
byte: mindstorm.cma
native: mindstorm.cmxa


.PHONY: tests
tests: mindstorm.cma
	$(CD) tests && $(MAKE) -B PP="$(PP)" byte

# FIXME: Early testing (obsolete?)
test.exe: $(STUBS:.c=$(EXT_O)) test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I "$(OCAMLLIBDIR)" $(addprefix -cclib ,$(CC_LIB)) $^

.PHONY: examples ex
ex: examples
examples: mindstorm.cma
	$(CD) examples && $(MAKE) -B byte

# Install

install: META all
	$(OCAMLFIND) install $(PKG) META $(INSTALL_FILES)

uninstall:
	$(OCAMLFIND) remove $(PKG)

META: META.in
	cat $^ > $@
	echo "version = \"$(VERSION)\"" >> $@

# Generate HTML documentation
.PHONY: doc
doc:
#	cd src/; $(MAKE) $(INTERFACES)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt

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


include Makefile.ocaml
# Define the OS type for the Camlp4 preprocessor and load system
# specific Makefiles for rules to create the .cm[x]a
make_os_type.exe: LIBS_CMA+=unix.cma
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

.PHONY: clean
clean::
	$(MAKE) -C tests RM="$(RM)" $@
	$(MAKE) -C examples RM="$(RM)" $@
	-$(RM) META .os_type $(wildcard *.exe) $(PKG_TARBALL)
	-cd $(DOC_DIR) && $(RM) $(wildcard *~ *.html *.css)
