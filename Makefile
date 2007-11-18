# Makefile for Unix
INTERFACES = mindstorm.mli
DOC_DIR = doc
WEB_DIR = web/
SF_WEB 	= /home/groups/o/oc/ocaml-mindstorm/htdocs

PP = -pp "camlp4o pa_macro.cmo"
CAML_H = $(shell ocamlc -where)
CFLAGS= -Wall -fPIC
OCAMLC_FLAGS = -g

STUBS=mindstorm_unix.c

mindstorm.cma: $(STUBS:.c=.o) mindstorm.cmo
	$(OCAMLMKLIB) -o mindstorm  $^ -lbluetooth
# 	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) -custom unix.cma \
# 	  -I $(CAML_H) -cclib -lbluetooth unix.cma $^

mindstorm.cmxa: $(STUBS:.c=.o) mindstorm.cmx
	$(OCAMLMKLIB) -o mindstorm  $^ -lbluetooth

.PHONY: tests
tests: mindstorm.cma
	$(CD) tests; $(MAKE) -B byte

test.exe: mindstorm_unix.c test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I $(CAML_H) -cclib -lbluetooth $^

.PHONY: ex
ex: mindstorm.cma
	$(CD) examples; $(MAKE) -B byte

# Generate HTML documentation
.PHONY: doc
doc:
#	cd src/; $(MAKE) $(INTERFACES)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt

# Publish the doc to SF
web: doc
	@ if [ -d $(DOC_DIR) ] ; then \
	  scp $(DOC_DIR)/*.html $(DOC_DIR)/*.css \
		shell.sf.net:$(SF_WEB)/doc/ \
	  && echo "*** Published documentation on SF" ; \
	fi
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(WEB_DIR)/*.html $(WEB_DIR)/*.jpg LICENSE \
	    shell.sf.net:$(SF_WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/) on SF" ; \
	fi



include Makefile.ocaml

clean::
	$(RM) *.so
	cd doc/; $(RM) *~ *.html *.css
	cd tests/; $(MAKE) clean