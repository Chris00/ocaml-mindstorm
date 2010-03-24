// ****************************************************************************
// *                                                                          *
// *                      Velena Source Code V1.0                             *
// *                   Written by Giuliano Bertoletti                         *
// *       Based on the knowledged approach of Louis Victor Allis             *
// *   Copyright (C) 1996-97 by Giuliano Bertoletti & GBE 32241 Software PR   *
// *                                                                          *
// ****************************************************************************

// Portable engine version.
// read the README file for further informations.

// ==========================================================================


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#include "connect4.h"
#include "pnsearch.h"
#include "proto.h"

extern struct board *brd;

void fatal_error(char *erstring)
	{
        FILE *h1;
        short x;

	h1=fopen(LOGFILE,"a+");
	fprintf(h1,"\n*** Fatal Error *** : %s\n",erstring);

        if(brd && brd->filled>0)
		{
		for(x=0;x<brd->filled;x++)
			putc(brd->moves[x]+0x31,h1);

		fprintf(h1,"\n\n");
		}

	printf("\n");
	printf("Fatal Error: %s\n",erstring);
	exit(0);
	}

