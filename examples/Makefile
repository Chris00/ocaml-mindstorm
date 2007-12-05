# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
OCAMLC_FLAGS = -thread -g -I .. -dllpath .. -custom

EXAMPLES=$(wildcard *.ml)
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa)

.PHONY: byte
byte: $(EXAMPLES:.ml=.exe)
native: $(EXAMPLES:.ml=.com)

include ../Makefile.ocaml

clean::
	$(RM) *.exe *.com