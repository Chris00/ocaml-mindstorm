
CFLAGS= -Wall -fPIC $(D_HAS_USB) $(USB_INCLUDE)
OCAMLC_FLAGS = -g -dtypes

PP = camlp4o pa_macro.cmo $(D_OS) $(D_ARCH64) $(D_HAS_USB)

.PHONY: all byte native
all: byte native
byte: mindstorm.cma
native: mindstorm.cmxa


# FIXME: Early testing (obsolete?)
test.exe: $(STUBS:.c=$(EXT_O)) test.ml
	$(OCAMLC) -o $@ -custom unix.cma -I "$(OCAMLLIBDIR)" $(addprefix -cclib ,$(CC_LIB)) $^


include ../Makefile.ocaml
# Define the OS type for the Camlp4 preprocessor and load system
# specific Makefiles for rules to create the .cm[x]a
make_os_type.exe: LIBS_CMA+=unix.cma
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

clean::
	-$(RM) META .os_type $(wildcard *.exe)
