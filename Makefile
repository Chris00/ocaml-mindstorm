# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so

PACKAGES = -package mindstorm,bigarray,graphics
DOC_DIR=doc

OCAMLC_FLAGS = -g -dtypes -custom $(PACKAGES)
OCAMLOPT_FLAGS = -dtypes $(PACKAGES)

TESTS=$(wildcard *-*.ml)
LIBS_CMA = -linkpkg
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa)

.PHONY: all byte native tests tests-byte test-native

all: byte native

#changer sources et faire une autre variable, une qui prendra ceux a exectuer
#une autre qui prendra ceux a ne pas utiliser en .exe
SOURCES = game.ml alphabeta.ml plateauJeu.ml DeuxUnitesPush.ml scanPiece.ml pincer.ml run_connect4.ml scanPiece2.ml
byte: $(SOURCES:.ml=.exe)
native: $(SOURCES:.ml=.com)

DeuxUnitesPush.exe : robot.cma
alphabeta.exe : game.cmo 
scanPiece.exe : robot.cma
scanPiece2.exe : robot.cma
pincer.exe : robot.cma
run_connect4.exe : robot.cma game.cmo alphabeta.cmo scanPiece.cmo pincer.cmo
plateauJeu.exe : game.cmo

DeuxUnitesPush.com : robot.cmxa
alphabeta.com : game.cmx
scanPiece.com : robot.cmxa
scanPiece2.com : robot.cmxa
pincer.com : robot.cmxa
run_connect4.com : robot.cmxa game.cmx alphabeta.cmx pincer.cmx scanPiece.cmx
plateauJeu.com : game.cmx

# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx

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

# Define the OS type for the Camlp4 preprocessor, RM,...
#.os_type: make_os_type.exe
#	"./$<" > $@
#include .os_type

include Makefile.ocaml

clean::
	-$(RM) $(wildcard *.exe *.com *.obj *.lib)
	-$(CD) $(DOC_DIR) && $(RM) *~ *.html *.css
#	-$(CD) labyrinth/ &&  $(MAKE) clean
#	-$(CD) rubik/ &&  $(MAKE) clean
