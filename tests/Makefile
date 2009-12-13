# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
OCAMLC_FLAGS = -g -dtypes -I .. -dllpath .. -custom
PP = camlp4o pa_macro.cmo $(D_OS) $(D_ARCH64)

TESTS=$(filter-out connect.ml, $(wildcard *.ml))
LIBS_CMA=unix.cma mindstorm.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa)

.PHONY: tests byte native
tests: byte
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

$(TESTS:.ml=.exe): connect.cmo
$(TESTS:.ml=.exe): LIBS_CMA += connect.cmo
$(TESTS:.ml=.com): connect.cmx
$(TESTS:.ml=.com): LIBS_CMXA += connect.cmx

include ../Makefile.ocaml
# Define the OS type for the Camlp4 preprocessor
../.os_type: ../make_os_type.exe
	"$<" > $@
include ../.os_type

.PHONY: clean
clean::
	$(RM) *.exe *.com