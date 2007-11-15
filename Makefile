# Makefile for Unix
INTERFACES = mindstorm.mli
DOC_DIR = doc

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
	$(OCAMLDOC) -d $(DOC_DIR) $(PP) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt


include Makefile.ocaml

clean::
	$(RM) *.so
	cd doc/; $(RM) *~ *.html *.css
	cd tests/; $(MAKE) clean