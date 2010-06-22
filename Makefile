# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so

PACKAGES = -package mindstorm,bigarray,graphics
DOC_DIR=doc

OCAMLC_FLAGS = -g -dtypes -custom $(PACKAGES)
OCAMLOPT_FLAGS = -g -dtypes $(PACKAGES)

TESTS=$(wildcard *-*.ml)
LIBS_CMA = -linkpkg
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa)

.PHONY: all byte native tests tests-byte test-native

all: byte native

SOURCES = utils.ml board.ml rules.ml adjacency.ml solver.ml \
    evaluate.ml heuristic.ml opening_book.ml ia.ml display.ml \
    scanPiece.ml pincer.ml
byte: run_connect4mem.exe
native: run_connect4mem.com test.com

run_connect4.exe : robot.cma $(SOURCES:.ml=.cmo)
run_connect4mem.exe : robot.cma $(SOURCES:.ml=.cmo)
run_connect4.com : robot.cmxa $(SOURCES:.ml=.cmx)
run_connect4mem.com : robot.cmxa $(SOURCES:.ml=.cmx)
test.com : robot.cmxa $(SOURCES:.ml=.cmx)


# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx

# Various tests
TESTS=$(wildcard test*.ml)
tests: $(TESTS:.ml=.com)
$(TESTS:.ml=.com): robot.cmxa

test_alphabeta.com: game.cmx alphabeta.cmx
test_pincer.com: pincer.cmx
test_scanPiece.com: scanPiece.cmx
#test_scanPiece2.com: scanPiece2.cmx


# Generate HTML documentation
MAKE_DOC = $(OCAMLDOC) -colorize-code -stars -html $(PACKAGES)
.PHONY: doc
#doc: $(INTERFACES:.mli=.cmi)
#	-$(MKDIR) $(DOC_DIR)#	$(MAKE_DOC) -d $(DOC_DIR) $(wildcard *.mli)
#	-$(MKDIR) $(DOC_DIR)/labyrinth
#	$(MAKE_DOC) -d $(DOC_DIR)/labyrinth -I labyrinth \
#	  $(wildcard labyrinth/*.mli)
#	-$(MKDIR) $(DOC_DIR)/rubik
#	$(MAKE_DOC) -d $(DOC_DIR)/rubik -I rubik $(wildcard rubik/*.mli)

# Add subdirectories (necessary to compile the doc of all modules)
#.depend.ocaml: $(wildcard labyrinth/*.ml) $(wildcard labyrinth/*.mli)
#.depend.ocaml: $(wildcard rubik/*.ml) $(wildcard rubik/*.mli)

#labyrinth/%:
#	cd labyrinth/ && $(MAKE) $(@F)
#rubik/%:
#	cd rubik/ && $(MAKE) $(@F)

#Define the OS type for the Camlp4 preprocessor, RM,...
.os_type: make_os_type.exe
	"./$<" > $@
include .os_type

include Makefile.ocaml

clean::
	-$(RM) $(wildcard *.exe *.com *.obj *.lib)
	-$(CD) $(DOC_DIR) && $(RM) *~ *.html *.css
#	-$(CD) labyrinth/ &&  $(MAKE) clean
#	-$(CD) rubik/ &&  $(MAKE) clean
