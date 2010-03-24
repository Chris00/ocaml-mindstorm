# Makefile for VELENA Engine (Linux gcc)

CC = gcc
JC = javac
LD = ld
AR = ar
NM = nm
MAKE = make


MODULES = adjmtrx.o   connect4.o  error.o     ia_main.o   pnsearch.o \
	  bintree.o   database.o  evaluate.o  pbsolver.o  wc.o       \
	  buildob.o   dummy.o     heurist.o   playgame.o  cmdline.o

CFLAGS =  -O3 -fPIC

all:	 veleng
	 @echo "VELENA Engine built."

veleng:	$(MODULES)
	$(CC) $(CFLAGS) $(MODULES) -o veleng

%.o: %.c
	 $(CC) -c $(CFLAGS) -o $@ $<

clean:
	 rm -f veleng
	 rm -f *.o
	 rm -f *~






