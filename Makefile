# Generic Makefile
INTERFACES = mindstorm.mli
DOC_DIR = doc
WEB_DIR = web
SF_WEB 	= shell.sf.net:/home/groups/o/oc/ocaml-mindstorm/htdocs

CFLAGS= -Wall -fPIC
OCAMLC_FLAGS = -g -dtypes

PP = camlp4o pa_macro.cmo $(D_OS) $(D_ARCH64)

VERSION=$(shell grep "@version" mindstorm.mli | sed "s/[^0-9]*//")

.PHONY: all byte native
all: byte native
byte: mindstorm.cma
native: mindstorm.cmxa

# See system specific Makefiles for the rules to create the .cm[x]a

make_os_type.exe: LIBS_CMA+=unix.cma

.PHONY: tests
tests: mindstorm.cma
	$(CD) tests; $(MAKE) -B PP="$(PP)" byte

# FIXME: Early testing (obsolete?)
test.exe: $(STUBS:.c=$(EXT_O)) test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I "$(OCAMLLIBDIR)" $(addprefix -cclib ,$(CC_LIB)) $^

.PHONY: examples ex
ex: examples
examples: mindstorm.cma
	$(CD) examples; $(MAKE) -B byte

# Generate HTML documentation
.PHONY: doc
doc:
#	cd src/; $(MAKE) $(INTERFACES)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt

# Publish the doc to SF
.PHONY: web web-doc website
web: web-doc website
web-doc: doc
	@ if [ -d $(DOC_DIR) ] ; then \
	  $(DOC_DIR)/add_sf_logo && \
	  scp $(DOC_DIR)/*.html $(DOC_DIR)/*.css $(SF_WEB)/doc/ \
	  && echo "--- Published documentation on SF." ; \
	fi

website:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(WEB_DIR)/*.html $(WEB_DIR)/*.css LICENSE \
	    $(WEB_DIR)/*.png  $(SF_WEB) \
	  && echo "--- Published web site ($(WEB_DIR)/) on SF." ; \
	fi

META: META.in
	cat $^ > $@
	echo "version = \"$(VERSION)\"" >> $@


include Makefile.ocaml
# Define the OS type for the Camlp4 preprocessor
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

.PHONY: clean
clean::
	-$(RM) META .os_type
	-cd $(DOC_DIR); $(RM) $(wildcard*~ *.html *.css)
	-cd tests/; $(MAKE) RM="$(RM)" clean
	-cd examples/; $(MAKE) RM="$(RM)" clean
