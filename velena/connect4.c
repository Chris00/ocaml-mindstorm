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

// This module contains the main function. Just see file cmdline.c to see how
// to send positions to the engine. 

// Main module

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <unistd.h>

#include "connect4.h"
#include "pnsearch.h"
#include "proto.h"

#define BLKSIZE 16384           // Buffer size for I/O

#define PLAYER1 0
#define PLAYER2 1

struct board *brd;

short check_solution_groups(struct board *board)
	{
	short x,y,z,q,c,answer=YES;

	for(y=0;y<BOARDY && answer;y++)
		for(x=0;x<BOARDX && answer;x++)
			for(z=0;z<board->solvable_groups->sqpnt[ELM(x,y)] && answer;z++)
				{
				answer=NO;
				c=board->solvable_groups->square[ELM(x,y)][z];
				for(q=0;q<TILES;q++)
					if(*board->groups[c][q]==ELM(x,y)) answer=YES;
				}

	return answer;
	}


// Initialize the program data structures, it reads and builds (if needed)
// the opening book.

void init_prg(struct board *board)
	{
    long size,len;
    FILE *h1;
    short x;

    brd=board;

    board->wins[PLAYER1]=0;
    board->wins[PLAYER2]=0;
    board->draws        =0;
    board->lastguess    =0;
    board->bestguess   =MAXMEN;
    board->lastwin       =EMPTY;

    board->white_lev=0;   // Human
    board->black_lev=3;   // Computer-Strong

    board->videotype = CHARS;
    board->enablegr  = NO;

    board->autotest = NO;

    for(x=0;x<3;x++)
	board->rule[x]=0L;

    board->oracle_guesses=0;

    h1=fopen(WHITE_BOOK,"rb");
    if(!h1) fatal_error("Opening book not present");

    size=fileln(h1);
    if(size%14!=0) fatal_error("White opening book is corrupted");

    board->wbposit=size/14;

    board->white_book=(unsigned char *)malloc(size);
    if(!board->white_book) fatal_error("Not enough memory to allocate opening book");

    len=0;               // We read all the position from disk
    while(size>0)     // each position takes 14 bytes of storage
        {
        fread(&board->white_book[len],1,14,h1);
        len+=14;
        size-=14;
        }

    fclose(h1);

    board->bbposit=0;
    }

void initboard(struct board *board)
	{
	short x,y,i,j,p;

	randomize();

	for(i=0;i<10;i++)
		board->instances[i]=0L;

	/* Groups initializations */

    // Step one. Horizontal lines.

    i=0;
    for(y=0;y<BOARDY;y++)
		for(x=0;x<BOARDX-3;x++)
			{
			board->groups[i][0]=&board->square[ELM(x+0,y)];
			board->groups[i][1]=&board->square[ELM(x+1,y)];
			board->groups[i][2]=&board->square[ELM(x+2,y)];
			board->groups[i][3]=&board->square[ELM(x+3,y)];

			board->xplace[i][0]=x;
			board->xplace[i][1]=x+1;
			board->xplace[i][2]=x+2;
			board->xplace[i][3]=x+3;

			board->yplace[i][0]=y;
			board->yplace[i][1]=y;
			board->yplace[i][2]=y;
			board->yplace[i][3]=y;

			i++;
			}

    // Step two. Vertical lines

	for(y=0;y<BOARDY-3;y++)
		for(x=0;x<BOARDX;x++)
			{
			board->groups[i][0]=&board->square[ELM(x,y+0)];
			board->groups[i][1]=&board->square[ELM(x,y+1)];
			board->groups[i][2]=&board->square[ELM(x,y+2)];
			board->groups[i][3]=&board->square[ELM(x,y+3)];

			board->xplace[i][0]=x;
			board->xplace[i][1]=x;
			board->xplace[i][2]=x;
			board->xplace[i][3]=x;

			board->yplace[i][0]=y+0;
			board->yplace[i][1]=y+1;
			board->yplace[i][2]=y+2;
			board->yplace[i][3]=y+3;

			i++;
			}

    // Step three. Diagonal (north east) lines

	for(y=0;y<BOARDY-3;y++)
		for(x=0;x<BOARDX-3;x++)
			{
			board->groups[i][0]=&board->square[ELM(x+0,y+0)];
			board->groups[i][1]=&board->square[ELM(x+1,y+1)];
			board->groups[i][2]=&board->square[ELM(x+2,y+2)];
			board->groups[i][3]=&board->square[ELM(x+3,y+3)];

			board->xplace[i][0]=x+0;
			board->xplace[i][1]=x+1;
			board->xplace[i][2]=x+2;
			board->xplace[i][3]=x+3;

			board->yplace[i][0]=y+0;
			board->yplace[i][1]=y+1;
			board->yplace[i][2]=y+2;
			board->yplace[i][3]=y+3;

			i++;
			}

    // Step four. Diagonal (south east) lines

    for(y=3;y<BOARDY;y++)
		for(x=0;x<BOARDX-3;x++)
			{
			board->groups[i][0]=&board->square[ELM(x+0,y-0)];
			board->groups[i][1]=&board->square[ELM(x+1,y-1)];
			board->groups[i][2]=&board->square[ELM(x+2,y-2)];
			board->groups[i][3]=&board->square[ELM(x+3,y-3)];

			board->xplace[i][0]=x+0;
			board->xplace[i][1]=x+1;
			board->xplace[i][2]=x+2;
			board->xplace[i][3]=x+3;

			board->yplace[i][0]=y-0;
			board->yplace[i][1]=y-1;
			board->yplace[i][2]=y-2;
			board->yplace[i][3]=y-3;

			i++;
			}

	if(i!=GROUPS) fatal_error("Implementation error!");

	for(x=0;x<64;x++)
		{
		board->solvable_groups->sqpnt[x]=0;
		for(y=0;y<16;y++)
			board->solvable_groups->square[x][y]=-1;
		}

	for(x=0;x<BOARDX;x++)
		for(y=0;y<BOARDY;y++)
			board->square[ELM(x,y)]=ELM(x,y);


	for(i=0;i<GROUPS;i++)
		for(j=0;j<TILES;j++)
			{
			p=*board->groups[i][j];
			board->solvable_groups->square[p][board->solvable_groups->sqpnt[p]++]=i;
			}

	if(!check_solution_groups(board))
		fatal_error("Implementation error!");

    // Here we set all out squares to a default value to detect problems

	for(i=0;i<8;i++)
		{
		board->square[ELM(7,i)]=FULL;
		board->square[ELM(i,6)]=board->square[ELM(i,7)]=FULL;
		}

	board->stack[7]=FULL;

	for(y=0;y<BOARDY;y++)
		for(x=0;x<BOARDX;x++)
			board->square[ELM(x,y)]=EMPTY;

	for(x=0;x<BOARDX;x++)
		board->stack[x]=0;

	board->turn=WHITE;
	board->filled=0;
        board->cpu=0x01;
        return;
	}

void initTitle()
    {
    printf("\n");
    printf("Velena Engine %s; revision %s\n",SEARCH_ENGINE_VERSION,__DATE__);
    
    printf("\n");
    printf("Connect four AI engine written by Giuliano Bertoletti\n");
    printf("Based on the knowledged approach of Victor Allis\n");
    printf("Copyright (C) 1996-97 Giuliano Bertoletti ");
    printf("and GBE 32241 Software PR.\n");
    printf("All rights reserved.\n\n");
    }

void main(short ac,char **av)
	{
	short x;
	struct board *board;

        // Here we initialize our environment and the call
        // command_line_input in file cmdline.c to process data from the
        // outside world.

	initTitle();
	
        fight(NO);

	brd=NULL;
	board=(struct board *)malloc(sizeof(struct board));
	board->solvable_groups=(struct solvable_groups *)
	                        malloc(sizeof(struct solvable_groups));

	if(!board || !board->solvable_groups)
		fatal_error("Cannot allocate memory!");

        board->debug=0;

        init_prg(board);       // Initialize data structures

	for(x=0;x<ALLOC_SOLUTIONS;x++)
		{
		board->solution[x]=(struct solution *)
		                   malloc(sizeof(struct solution));
		if(!board->solution[x]) 
		     fatal_error("Not enough memory for solutions");
		}

        board->usegraphics = NO;

        // ******************************************************************

        // Build your program here...
 
        command_line_input(board);

        // ******************************************************************

        for(x=0;x<ALLOC_SOLUTIONS;x++)
	    if(board->solution[x]) free(board->solution[x]);

        free(board->solvable_groups);
        free(board);
        exit(0);
        }



