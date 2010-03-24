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
#include "con4vals.h"

#include "rules.h"
#include "pnsearch.h"
#include "proto.h"

extern char *rulename[];

short rulecombo[9][9]={ 1, 1, 1, 1, 3, 3, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 4, 3, 3, 1, 4,12,
                        3, 1, 1, 3, 8, 8, 3, 6, 6,
                        3, 1, 1, 3, 8, 8, 3, 3, 3,
                        1, 1, 1, 1, 3, 3, 1, 1, 1,
                        1, 1, 1, 4, 6, 3, 1, 4, 4,
                        1, 1, 1,12, 6, 3, 1, 4, 4 };

char **allocate_matrix(struct board *board)
	{
	unsigned char **matrix;
	short x;

	matrix=(unsigned char **)malloc(board->sp * sizeof(unsigned char *));
	if(!matrix) fatal_error("Memory overflow!");

	for(x=0;x<board->sp;x++)
		{
		matrix[x]=(unsigned char *)malloc(x+1);
		if(!matrix[x]) fatal_error("Cannot allocate adjacency matrix");
		}

	return (char **)matrix;
	}

void free_matrix(char **matrix,struct board *board)
	{
	short x;

	for(x=0;x<board->sp;x++)
		free(matrix[x]);

	free(matrix);
	}

short overlap(struct board *board,short p1,short p2)
	{
	short x,y;
	short temp[(BOARDX+1)*(BOARDY+2)];

	for(x=0;x<board->solution[p1]->sqinvnumb;x++)
		temp[board->solution[p1]->sqinv[x]]=NO;

	for(x=0;x<board->solution[p2]->sqinvnumb;x++)
		temp[board->solution[p2]->sqinv[x]]=YES;

	for(x=0;x<board->solution[p1]->sqinvnumb;x++)
		if(temp[board->solution[p1]->sqinv[x]]) return YES;

	return NO;
	}

short claimeven_below(struct board *board,short p1,short p2)
	{
	short x,y,name,q1,q2;
	short q1x,q2x,q1y,q2y,solcheck;

	name=board->solution[p1]->solname;
	if(name!=HIGHINVERSE && name!=LOWINVERSE)
		{
		q1=p2;
		q2=p1;
		}
	else
		{
		q1=p1;
		q2=p2;
		}

	name=board->solution[q1]->solname;
	if(name!=HIGHINVERSE && name!=LOWINVERSE)
		fatal_error("Condition not coming from an Inverse");

    if(board->solution[q2]->solname==AFTEREVEN)
        {
        solcheck=board->solution[q2]->sqinvnumb/2;

        for(x=0;x<2;x++)
            {
            /* That's tricky to get the lowest square in the column */
            q1x=ELX(board->solution[q1]->sqinv[x+2]);
            q1y=ELY(board->solution[q1]->sqinv[x+2]);

            for(y=0;y<solcheck;y++)
                {
                q2x=ELX(board->solution[q2]->sqinv[solcheck+y]);
                q2y=ELY(board->solution[q2]->sqinv[solcheck+y]);

                if(q1x==q2x && q1y>q2y && (q2y&1)==1) return YES;
                }
            }
        }
    else if (board->solution[q2]->solname==BEFORE ||
		board->solution[q2]->solname==SPECIALBEFORE)
			{
			solcheck=board->solution[q2]->sqinvnumb/2;

			for(x=0;x<2;x++)
				{
				/* That's tricky to get the lowest square in the column */
				q1x=ELX(board->solution[q1]->sqinv[x+2]);
				q1y=ELY(board->solution[q1]->sqinv[x+2]);

				for(y=0;y<solcheck;y++)
					{
                    q2x=ELX(board->solution[q2]->sqinv[1+(y<<1)]);
                    q2y=ELY(board->solution[q2]->sqinv[1+(y<<1)]);

					if(q1x==q2x && q1y>q2y && (q2y&1)==1) return YES;
					}
				}
			}

    else if(board->solution[q2]->solname==CLAIMEVEN)
		{
		for(x=0;x<2;x++)
			{
			/* That's tricky to get the lowest square in the column */
			q1x=ELX(board->solution[q1]->sqinv[x+2]);
			q1y=ELY(board->solution[q1]->sqinv[x+2]);
			q2x=ELX(board->solution[q2]->sqinv[0]);
			q2y=ELY(board->solution[q2]->sqinv[0]);

			if(q1x==q2x && q1y>q2y) return YES;
			}
		}
	else if(board->solution[q2]->solname==BASECLAIM)
		{
		for(x=0;x<2;x++)
			{
			/* That's tricky to get the lowest square in the column */
			q1x=ELX(board->solution[q1]->sqinv[x+2]);
			q1y=ELY(board->solution[q1]->sqinv[x+2]);
			q2x=ELX(board->solution[q2]->sqinv[3]);
			q2y=ELY(board->solution[q2]->sqinv[3]);

			if(q1x==q2x && q1y>q2y) return YES;
			}
		}

	else fatal_error("Could not figure out what combination I am dealing about");

	return NO;
	}

short column_wdoe(struct board *board,short p1,short p2)
	{
	char debug[80];
	short joinmtrx[(BOARDX+1)*(BOARDY+2)];
	short x,y,cnt,answer=YES,w1,w2;

	w1=board->solution[p1]->solname;
	w2=board->solution[p2]->solname;

	if(w1!=SPECIALBEFORE && w1!=BEFORE && w1!=AFTEREVEN && w1!=LOWINVERSE)
		{
		sprintf(debug,"Inconsistent wdow p1: rule = %d",w1);
		fatal_error(debug);
		}

	if(w2!=SPECIALBEFORE && w2!=BEFORE && w2!=AFTEREVEN && w2!=LOWINVERSE)
		{
		sprintf(debug,"Inconsistent wdow p2: rule = %d",w2);
		fatal_error(debug);
		}

	memset(joinmtrx,0,(BOARDX+1)*(BOARDY+2)*sizeof(short));

	for(x=0;x<board->solution[p1]->sqinvnumb;x++)
		joinmtrx[board->solution[p1]->sqinv[x]]=YES;

	for(y=0;y<board->solution[p2]->sqinvnumb;y++)
		joinmtrx[board->solution[p2]->sqinv[y]]=YES;

	for(x=0;x<BOARDX && answer;x++)
		{
		cnt=0;
		for(y=0;y<BOARDY;y++)
			if(joinmtrx[ELM(x,y)]) cnt++;

		if((cnt&1)==1) answer=0;
		}

	return answer;
	}

short comp_rules(struct board *board,short p1,short p2)
	{
	short way,c1,c2;

	c1=board->solution[p1]->solname-1;
	c2=board->solution[p2]->solname-1;

	way=rulecombo[c1][c2];

	if(way&9)
		{
		board->rule[0]++;
		if(overlap(board,p1,p2)) return NO;
		}

	if(way&2)
		{
		board->rule[1]++;
		if(claimeven_below(board,p1,p2)) return NO;
		}

	if(way&4)
		{
		board->rule[2]++;
		if(!column_wdoe(board,p1,p2)) return NO;
		}

	return YES;
	}

void build_adjacency_matrix(char **matrix,struct board *board)
	{
	short x,y;

	for(x=0;x<board->sp;x++)
		for(y=x;y<board->sp;y++)
			{
			if(x==y) matrix[x][x]=NO;
			else if(comp_rules(board,x,y)) matrix[y][x]=YES;
			else matrix[y][x]=NO;
			}
	}

void dump_no_adjacencies(char **matrix,struct board *board)
	{
	long comp=0,uncomp=0;
	short x,y,px1,px2,py1,py2,i;

	printf("Adjacency matrix faults\n\r");

	for(x=0;x<board->sp-1;x++)
		for(y=x+1;y<board->sp;y++)
			{
			if(matrix[y][x]==NO)
				{
				uncomp++;
				px1=ELX(board->solution[x]->solpoint[0]);
				py1=ELY(board->solution[x]->solpoint[0]);
				px2=ELX(board->solution[x]->solpoint[1]);
				py2=ELY(board->solution[x]->solpoint[1]);

				printf("%13s ",rulename[board->solution[x]->solname-1]);
				printf("%c%d-%c%d  ",px1+97,py1+1,px2+97,py2+1);

				for(i=0;i<6;i++)
					{
					if(i<board->solution[x]->sqinvnumb)
						printf("%2d,",board->solution[x]->sqinv[i]);
					else printf("   ");
					}

				px1=ELX(board->solution[y]->solpoint[0]);
				py1=ELY(board->solution[y]->solpoint[0]);
				px2=ELX(board->solution[y]->solpoint[1]);
				py2=ELY(board->solution[y]->solpoint[1]);

				printf("%13s ",rulename[board->solution[y]->solname-1]);
				printf("%c%d-%c%d  ",px1+97,py1+1,px2+97,py2+1);

				for(i=0;i<6;i++)
					{
					if(i<board->solution[y]->sqinvnumb)
						printf("%2d,",board->solution[y]->sqinv[i]);
					else printf("   ");
					}

				printf("\n\r");
				}
			else comp++;
			}

	printf("\n\r");
	printf("Total rules %d, matrix size %ld bytes\n\r",board->sp,(long)board->sp*board->sp);
	printf("Comb. rules uncomp=%ld, comp=%ld, total=%ld\n\r",uncomp,comp,uncomp+comp);
	}
