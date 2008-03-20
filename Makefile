# Generic Makefile
PKGNAME = ocaml-mindstorm
INTERFACES = mindstorm.mli
DOC_DIR = doc
WEB_DIR = web
SF_WEB 	= shell.sf.net:/home/groups/o/oc/ocaml-mindstorm/htdocs
DISTFILES = $(INTERFACES) $(wildcard *.ml *.h *.c Makefile*) examples/

CFLAGS= -Wall -fPIC
OCAMLC_FLAGS = -g -dtypes

PP = camlp4o pa_macro.cmo $(D_OS) $(D_ARCH64)

VERSION=$(shell grep "@version" mindstorm.mli | sed "s/[^0-9]*//")
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

# Generate HTML documentation
.PHONY: doc
doc:
#	cd src/; $(MAKE) $(INTERFACES)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt

# Publish the doc to SF
.PHONY: web web-doc website website-img
web: web-doc website website-img
web-doc: doc
	@ if [ -d $(DOC_DIR) ] ; then \
	  $(DOC_DIR)/add_sf_logo && \
	  scp $(DOC_DIR)/*.html $(DOC_DIR)/*.css $(SF_WEB)/doc/ \
	  && echo "--- Published documentation on SF." ; \
	fi

website:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(WEB_DIR)/*.html $(WEB_DIR)/*.css LICENSE $(SF_WEB) \
	  && echo "--- Published web site (in $(WEB_DIR)/) on SF." ; \
	fi

website-img:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(WEB_DIR)/*.png $(WEB_DIR)/*.jpg  $(SF_WEB) \
	  && echo "--- Published web site images (in $(WEB_DIR)/) on SF." ; \
	fi

META: META.in
	cat $^ > $@
	echo "version = \"$(VERSION)\"" >> $@

# Upload tarball
.PHONY: dist upload
dist:
	[ -d $(PKGNAME)-$(VERSION) ] || mkdir $(PKGNAME)-$(VERSION)
	cp --preserve -r --dereference $(DISTFILES) $(PKGNAME)-$(VERSION)/
	tar --exclude "CVS" --exclude ".cvsignore" --exclude-from=.cvsignore \
	  -zcvf $(PKG_TARBALL) $(PKGNAME)-$(VERSION)/
	$(RM) -rf $(PKGNAME)-$(VERSION)

upload: dist
	@ if [ -z "$(PKG_TARBALL)" ]; then \
		echo "PKG_TARBALL not defined"; exit 1; fi
	echo -e "bin\ncd incoming\nput $(PKG_TARBALL)" \
	  | ncftp -p chris_77@users.sf.net upload.sourceforge.net \
	  && echo "*** Uploaded $(PKG_TARBALL) to SF"


include Makefile.ocaml
# Define the OS type for the Camlp4 preprocessor and load system
# specific Makefiles for rules to create the .cm[x]a
make_os_type.exe: LIBS_CMA+=unix.cma
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

.PHONY: clean
clean::
	-$(RM) META .os_type $(wildcard *.exe) $(PKG_TARBALL)
	-cd $(DOC_DIR) && $(RM) $(wildcard *~ *.html *.css)
	-cd tests/ && $(MAKE) RM="$(RM)" clean
	-cd examples/ && $(MAKE) RM="$(RM)" clean
