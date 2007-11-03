# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
OCAMLC_FLAGS = -g -I .. -dllpath .. -custom

LIBS=unix.cma mindstorm.cma

.PHONY: byte
byte: sound.exe
native: sound.com

sound.exe: sound.cmo
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS) $^

sound.com: sound.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS:.cma=.cmxa) $^


include ../Makefile.ocaml

clean::
	$(RM) *.exe *.com