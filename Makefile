# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
PACKAGES = -package mindstorm,threads
DOC_DIR=doc

OCAMLC_FLAGS = -thread -g -dtypes -custom $(PACKAGES)
OCAMLOPT_FLAGS = -thread -dtypes $(PACKAGES)

TESTS=$(wildcard test*.ml)
SCAN=$(wildcard scan*.ml)
SOURCES=robot.ml game.ml
LIBS_CMA = -linkpkg
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: all byte native test scan scan-native scan-byte test-byte test-native clean-scan clean-test
all: clean byte native test scan
byte: robot.cma game.cma
native: robot.cmxa game.cmxa
test: clean-test native test-native test-byte
scan: clean-scan native scan-native scan-byte
test-byte: $(TESTS:.ml=.exe)
test-native: $(TESTS:.ml=.com)
scan-byte: $(SCAN:.ml=.exe)
scan-native: $(SCAN:.ml=.com)

# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx

scan.exe: robot.cmo
testLum.exe: robot.cmo
test_pincer.exe:robot.cmo
testClavier.exe:robot.cmo


# Generate HTML documentation
MAKE_DOC = $(OCAMLDOC) -colorize-code -stars -html $(PACKAGES)
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	-$(MKDIR) $(DOC_DIR)
	$(MAKE_DOC) -d $(DOC_DIR) $(wildcard *.mli)
	-$(MKDIR) $(DOC_DIR)/labyrinth
	$(MAKE_DOC) -d $(DOC_DIR)/labyrinth -I labyrinth \
	  $(wildcard labyrinth/*.mli)
	-$(MKDIR) $(DOC_DIR)/rubik
	$(MAKE_DOC) -d $(DOC_DIR)/rubik -I rubik $(wildcard rubik/*.mli)

# Add subdirectories (necessary to compile the doc of all modules)
.depend.ocaml: $(wildcard labyrinth/*.ml) $(wildcard labyrinth/*.mli)
.depend.ocaml: $(wildcard rubik/*.ml) $(wildcard rubik/*.mli)

labyrinth/%:
	cd labyrinth/ && $(MAKE) $(@F)
rubik/%:
	cd rubik/ && $(MAKE) $(@F)

# Define the OS type for the Camlp4 preprocessor, RM,...
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

include Makefile.ocaml

clean::
		-$(RM) $(wildcard *.exe *.com *.obj *.lib)

clean-scan::
		-$(RM) $(wildcard scan*.exe scan*.com scan*.com scan*.lib)

clean-test::
		-$(RM) $(wildcard test*.exe test*.com test*.com test*.lib)
