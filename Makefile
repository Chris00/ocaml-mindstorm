# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
PACKAGES = -package mindstorm,graphics
DOC_DIR=doc

OCAMLC_FLAGS = -thread -g -dtypes -custom $(PACKAGES)
OCAMLOPT_FLAGS = -thread -dtypes $(PACKAGES)

TESTS=$(wildcard *-*.ml)
LIBS_CMA = -linkpkg
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx



.PHONY: all byte native tests tests-byte test-native

all: byte native

SOURCES = scan.ml game.ml plateauJeu.ml DeuxUnitesPush.ml alphabeta.ml 
byte: $(SOURCES:.ml=.exe)
native: $(SOURCES: .ml=.com)

DeuxUnitesPush.exe : robot.cma
scan.exe : robot.cma
plateauJeu.exe : game.cmo

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
