
INTERFACES = mindstorm.mli
DOC_DIR = doc

CAML_H = $(shell ocamlc -where)

mindstorm.cma: mindstorm_unix.c mindstorm.ml
	$(OCAMLC) -o $@ -custom unix.cma -I $(CAML_H) -cclib -lbluetooth \
	  unix.cma $^

test.exe: mindstorm_unix.c test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I $(CAML_H) -cclib -lbluetooth $^

# Generate HTML documentation
.PHONY: doc
doc:
#	cd src/; $(MAKE) $(INTERFACES)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -intro $(DOC_DIR)/intro.txt



## Generic OCaml dependencies
PP	   ?=
OCAMLC     ?= ocamlc
OCAMLOPT   ?= ocamlopt
OCAMLDEP   ?= ocamldep
OCAMLDOC   ?= ocamldoc
OCAMLFIND  ?= ocamlfind
OCAMLTAGS  ?= ocamltags

TAR	?= tar
CD	?= cd

# The location of OCaml's library files.
OCAMLLIBDIR ?= $(shell $(OCAMLC) -where)

# Caml general dependencies
.SUFFIXES: .ml .mli .cmi .cmo .cma .cmx .cmxa

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma:
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $^

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa:
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $^

%.exe: %.cmo
	$(OCAMLC) -o $@ $(PP) $(OCAMLC_FLAGS) $(LIBS_CMA) $<

%.com: %.cmx
	$(OCAMLOPT) -o $@ $(PP) $(OCAMLOPT_FLAGS) $(LIBS_CMXA) $<

.depend.ocaml: $(wildcard *.ml) $(wildcard *.mli)
	-$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend.ocaml


.PHONY: clean
clean::
	$(RM) *~ *.cm{i,o,x,a,xa} *.annot *.o *.a
	cd doc/; $(RM) *~ *.html *.css