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

short tempsolused;
char *wside[]={"none","yellow","red"};


struct problem {
					short group;
					short solved;
					short solutions[ALLOC_SOLUTIONS];
					short solnumb;
					};


struct problem_list {
						  short number;
						  struct problem *problem[GROUPS];
						  short pointer[GROUPS];
						  short final[GROUPS];
						  };

struct up_solution {
						 short howmany;
						 short *which;
						 short hmprobs;
						 short *wprobs;
						 };

extern char *rulename[];


void dump_ddebug(struct board *board,struct problem_list *pblist,short j)
	{
	short x,y,name,px,py,qx,qy,i,k;
	FILE *h1;

	h1=fopen("DEBUG.LOG","a+");
	if(!h1) fatal_error("Cannot debug!");

	fprintf(h1,"\n");

	for(x=0;x<tempsolused;x++)
		putc(0x09,h1);

	fprintf(h1,"Problem [%2d] %c%d-%c%d\n",j,
		board->xplace[pblist->problem[j]->group][0]+97,board->yplace[pblist->problem[j]->group][0]+1,
		board->xplace[pblist->problem[j]->group][3]+97,board->yplace[pblist->problem[j]->group][3]+1);

	fclose(h1);
	}


void dump_debug(struct board *board,struct problem_list *pblist,short j)
	{
	short x,y,name,px,py,qx,qy,i,k;
	FILE *h1;

	h1=fopen("DEBUG.LOG","a+");
	if(!h1) fatal_error("Cannot debug!");

	for(x=0;x<tempsolused;x++)
		putc(0x09,h1);

	fprintf(h1,"PG[%2d] %c%d-%c%d  --> sol. ",j,
		board->xplace[pblist->problem[j]->group][0]+97,board->yplace[pblist->problem[j]->group][0]+1,
		board->xplace[pblist->problem[j]->group][3]+97,board->yplace[pblist->problem[j]->group][3]+1);

	i=pblist->final[j];
	if(i==-1) fprintf(h1,"    NOT FOUND\n");
	else
		{
		name=board->solution[i]->solname;
		px=ELX(board->solution[i]->solpoint[0]);
		py=ELY(board->solution[i]->solpoint[0]);

		qx=ELX(board->solution[i]->solpoint[1]);
		qy=ELY(board->solution[i]->solpoint[1]);

		fprintf(h1,"%13s  ",rulename[name-1]);
		fprintf(h1,"%c%d-%c%d   ",px+97,py+1,qx+97,qy+1);
		fprintf(h1,"[%03d] {",i);

		for(y=0;y<board->solution[i]->sqinvnumb;y++)
			{
			k=board->solution[i]->sqinv[y];
			fprintf(h1," %c%d",ELX(k)+97,ELY(k+1)+1);
			}

		fprintf(h1," }\n");
		}

	fclose(h1);
	}



void find_most_difficult_problem(struct problem_list *pblist,short *minsol,short *solpnt,struct board *board)
	{
	short i,j,k,m;

	*minsol=32767;
	*solpnt=-1;

	for(i=0;i<pblist->number;i++)
		{
		if(!pblist->problem[i]->solved)
			{
			k=0;
			for(j=0;j<pblist->problem[i]->solnumb;j++)
				{
				m=pblist->problem[i]->solutions[j];
				if(board->solution[m]->valid) k++;
				}

			if(k<(*minsol))
				{
				*minsol=k;
				*solpnt=i;
				}
			}
		}
	}


void build_problem_list(struct problem_list *pblist,struct board *board)
	{
	short i,j=0,x,y;

	for(i=0;i<GROUPS;i++)
		{
		pblist->final[i]=-1;

		if(board->intgp.tgroups[i]==YES)
			{
			pblist->problem[j]=(struct problem *)malloc(sizeof(struct problem));
			if(!pblist->problem[j]) fatal_error("Not enough memory to build problem list");

			pblist->pointer[i]=j;
			pblist->problem[j]->group=i;
			pblist->problem[j]->solnumb=0;
			pblist->problem[j]->solved=NO;

			pblist->number=++j;
			}

		else pblist->pointer[i]=-1;
		}

	for(x=0;x<board->sp;x++)
		{
		board->solution[x]->valid=YES;
		for(y=0;y<board->solution[x]->solgroupsnumb;y++)
			{
			i=board->solution[x]->solgroups[y];
			j=pblist->pointer[i];

			pblist->problem[j]->solutions[pblist->problem[j]->solnumb++]=x;
			}
		}
	}

void remove_problem_list(struct problem_list *pblist)
	{
	short x;

	for(x=0;x<pblist->number;x++)
		free(pblist->problem[x]);
	}

void remove_solutions(struct up_solution *update,struct problem_list *pblist,
							 struct board *board,char **matrix,short psol)
	{
	short x,y,z,j,ps;
	short tsol=0,*temp,*tprobs;
	short probs=0;

	temp=(short *)malloc(MAXSOLS*sizeof(short));
	tprobs=(short *)malloc(GROUPS*sizeof(short));
	if(!temp || !tprobs) fatal_error("Not enough memory!");

	for(y=0;y<board->solution[psol]->solgroupsnumb;y++)
		{
		z=board->solution[psol]->solgroups[y];
		j=pblist->pointer[z];
		if(j==-1) fatal_error("No real problem found");

		if(!pblist->problem[j]->solved)
			{
			pblist->problem[j]->solved=YES;
			tprobs[probs++]=j;
			pblist->final[j]=psol;
			}
		}


	for(x=0;x<board->sp;x++)
		{
		if(x>psol) ps=matrix[x][psol];
		else ps=matrix[psol][x];

		if(ps==NO && board->solution[x]->valid)
			{
			board->solution[x]->valid=NO;
			temp[tsol++]=x;
			}
		}

	if(tsol>=MAXSOLS) fatal_error("Wrote beyond buffer allocation");

	update->howmany=tsol;

	if(tsol>0)
		{
		update->which=(short *)malloc(tsol*sizeof(short));
		if(!update->which) fatal_error("No memory left");
		memcpy(update->which,temp,tsol*sizeof(short));
		}

	update->hmprobs=probs;

	if(probs>0)
		{
		update->wprobs=(short *)malloc(probs*sizeof(short));
		if(!update->wprobs) fatal_error("No memory left");
		memcpy(update->wprobs,tprobs,probs*sizeof(short));
		}

	free(tprobs);
	free(temp);
	}

void restore_solutions(struct up_solution *update,
							  struct problem_list *pblist,struct board *board)
	{
	short x;

	if(update->hmprobs>0)
		{
		for(x=0;x<update->hmprobs;x++)
			{
			if(pblist->problem[update->wprobs[x]]->solved==NO) fatal_error("Something is wrong");
			pblist->problem[update->wprobs[x]]->solved=NO;
			}
		free(update->wprobs);
		}

	if(update->howmany>0)
		{
		for(x=0;x<update->howmany;x++)
			{
			if(board->solution[update->which[x]]->valid==YES) fatal_error("Something is wrong!");
			board->solution[update->which[x]]->valid=YES;
			}
		free(update->which);
		}
	}

short solve_problem_list(struct problem_list *pblist,struct board *board,char **matrix)
	{
	struct up_solution update;
	short x,mdp,answer,sols,j;

	find_most_difficult_problem(pblist,&sols,&mdp,board);

	if(mdp==-1) return YES;
	if(sols==0) return NO;

	tempsolused++;
	if(board->solused<tempsolused) board->solused=tempsolused;

	#ifdef DEBUG
	dump_ddebug(board,pblist,mdp);
	#endif

	answer=NO;
	for(x=0;x<pblist->problem[mdp]->solnumb && answer==NO;x++)
		{
		j=pblist->problem[mdp]->solutions[x];
		if(!board->solution[j]->valid) continue;

		pblist->problem[mdp]->solved=YES;
		pblist->final[mdp]=j;

		#ifdef DEBUG
		dump_debug(board,pblist,mdp);
		#endif

		remove_solutions(&update,pblist,board,matrix,j);
		answer=solve_problem_list(pblist,board,matrix);
		restore_solutions(&update,pblist,board);

		if(answer==NO) pblist->final[mdp]=-1;
		pblist->problem[mdp]->solved=NO;
		}

	tempsolused--;

	return answer;
	}

short problem_solver(struct board *board,char **matrix,short debug,FILE *h1)
	{
	short x,y,i,px,py,qx,qy,name,answer,k;
	struct problem_list pblist;

	board->problem_solved=0;
	board->solused=0;
	tempsolused=0;

	build_problem_list(&pblist,board);
	answer=solve_problem_list(&pblist,board,matrix);

	if(debug)
		{
        x=board->turn^SWITCHSIDE;

        if(!answer)
            {
            fprintf(h1,"It has not been possible to find a subset of rules which both satisfies\n");
            fprintf(h1,"compatibility and full coverage of the problem list. Therefore nothing\n");
            fprintf(h1,"can be said about %s possibilities\n\n",wside[x]);
            goto PB_ENDING;
            }

        fprintf(h1,"Sol. matching problem list for %s ",wside[x]);
        fprintf(h1,"(according to rules compatibility):\n\n");

        for(x=0;x<pblist.number;x++)
			{
            fprintf(h1,"PG[%2d] %c%d-%c%d  ",x,
				board->xplace[pblist.problem[x]->group][0]+97,board->yplace[pblist.problem[x]->group][0]+1,
				board->xplace[pblist.problem[x]->group][3]+97,board->yplace[pblist.problem[x]->group][3]+1);

			i=pblist.final[x];
			if(i==-1)
				{
				printf("    NOT FOUND\n\r");
				fprintf(h1,"    NOT FOUND\n");
				}
			else
				{
				board->problem_solved++;
				name=board->solution[i]->solname;
				px=ELX(board->solution[i]->solpoint[0]);
				py=ELY(board->solution[i]->solpoint[0]);

				qx=ELX(board->solution[i]->solpoint[1]);
				qy=ELY(board->solution[i]->solpoint[1]);

				fprintf(h1,"%13s  ",rulename[name-1]);
				fprintf(h1,"%c%d-%c%d   ",px+97,py+1,qx+97,qy+1);
				fprintf(h1,"[%03d] {",i);

				for(y=0;y<board->solution[i]->sqinvnumb;y++)
					{
					k=board->solution[i]->sqinv[y];
					fprintf(h1," %c%d",ELX(k)+97,ELY(k+1)+1);
					}

				fprintf(h1," }\n");
				}
			}
        fprintf(h1,"\nThe aforementioned statements prove that ");

        x=board->turn^SWITCHSIDE;

        if(x==WHITE)
            fprintf(h1,"white can win from this position.\n");
        else
            fprintf(h1,"black can at least draw the game\nfrom this position.\n");
        }

    PB_ENDING:
    remove_problem_list(&pblist);
	return answer;
	}
