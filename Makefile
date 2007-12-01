# Makefile for Unix
INTERFACES = mindstorm.mli
DOC_DIR = doc
WEB_DIR = web
SF_WEB 	= shell.sf.net:/home/groups/o/oc/ocaml-mindstorm/htdocs

CFLAGS= -Wall -fPIC
OCAMLC_FLAGS = -g -dtypes

STUBS=mindstorm_unix.c

PP = camlp4o pa_macro.cmo
ifeq ($(shell ocaml arch.ml),64)
	PP += -DARCH64
endif

VERSION=$(shell grep "@version" mindstorm.mli | sed "s/[^0-9]*//")

mindstorm.cma: $(STUBS:.c=.o) mindstorm.cmo
	$(OCAMLMKLIB) -o mindstorm  $^ -lbluetooth
# 	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) -custom unix.cma \
# 	  -I $(OCAMLLIBDIR) -cclib -lbluetooth unix.cma $^

mindstorm.cmxa: $(STUBS:.c=.o) mindstorm.cmx
	$(OCAMLMKLIB) -o mindstorm  $^ -lbluetooth

.PHONY: tests
tests: mindstorm.cma
	$(CD) tests; $(MAKE) -B PP="$(PP)" byte

test.exe: mindstorm_unix.c test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I $(OCAMLLIBDIR) -cclib -lbluetooth $^

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
	  && echo "*** Published documentation on SF." ; \
	fi

website:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(WEB_DIR)/*.html $(WEB_DIR)/*.css $(WEB_DIR)/*.jpg \
	  $(WEB_DIR)/*.png  LICENSE $(SF_WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/) on SF." ; \
	fi

META: META.in
	cat $^ > $@
	echo "version = \"$(VERSION)\"" >> $@


include Makefile.ocaml

clean::
	$(RM) *.so META
	-cd doc/; $(RM) *~ *.html *.css
	-cd tests/; $(MAKE) clean
	-cd examples/; $(MAKE) clean
