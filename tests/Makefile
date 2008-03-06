# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
OCAMLC_FLAGS = -g -I .. -dllpath .. -custom

TESTS=$(wildcard *.ml)
LIBS_CMA=unix.cma mindstorm.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa)

.PHONY: tests byte native
tests: byte
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)


include ../Makefile.ocaml

.PHONY: clean
clean::
	$(RM) *.exe *.com