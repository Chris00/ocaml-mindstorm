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
#include <time.h>
#include <math.h>

#include "connect4.h"
#include "con4vals.h"

#include "rules.h"
#include "pnsearch.h"
#include "proto.h"


/* Prototypes */

/* That should not be removed in order to get the correct pointer size */

struct threat_combo {
                    short cross,even,odd;
                    short gp1,gp2;
                    };


char **allocate_matrix(struct board *board);

char *who[]={"None","White (O)","Black (X)"};
char *eng[]={"Draw","won by White","won by Black"};

/* Program */

char *rulename[]={"CLAIMEVEN",
						"BASEINVERSE",
						"VERTICAL",
						"AFTEREVEN",
						"LOWINVERSE",
						"HIGHINVERSE",
						"BASECLAIM",
						"BEFORE",
						"SPECIALBEFORE"};
/* --------------------------------------------------------------- */

void show_square_used(struct board *board)
    {
    short x,y;

    for(y=0;y<BOARDY;y++)
        {
        for(x=0;x<BOARDX;x++)
            {
            if(board->sqused[ELM(x,BOARDY-y-1)]) printf("*");
            else printf(".");
            }
        printf("\n");
        }
    }

void test_conditions(char **matrix,struct board *board)
	{
	short x,y,z,n1,n2,x1,y1;

	printf("Enter rule 1 number : ");
	scanf("%d %d",&x,&y);

	printf("\n\r\n\r");

	if(x>=0 && x<board->sp && y>=0 && y<board->sp)
		{
		n1=board->solution[x]->solname;
		n2=board->solution[y]->solname;

		printf("Rule %d: ",x);

		printf("%13s ",rulename[n1-1]);

		printf("--> %c%d-%c%d  ",ELX(board->solution[x]->solpoint[0])+65,
										  ELY(board->solution[x]->solpoint[0])+1,
										  ELX(board->solution[x]->solpoint[1])+65,
										  ELY(board->solution[x]->solpoint[1])+1);

		for(z=0;z<board->solution[x]->sqinvnumb;z++)
			printf("%c%d ",ELX(board->solution[x]->sqinv[z])+97,
								 ELY(board->solution[x]->sqinv[z])+1);

		printf("\n\r");

		printf("Rule %d: ",y);

		printf("%13s ",rulename[n2-1]);

		printf("--> %c%d-%c%d  ",ELX(board->solution[y]->solpoint[0])+65,
										  ELY(board->solution[y]->solpoint[0])+1,
										  ELX(board->solution[y]->solpoint[1])+65,
										  ELY(board->solution[y]->solpoint[1])+1);

		for(z=0;z<board->solution[y]->sqinvnumb;z++)
			printf("%c%d ",ELX(board->solution[y]->sqinv[z])+97,
								 ELY(board->solution[y]->sqinv[z])+1);

		printf("\n\r");
		printf("\n\r");

		x1=x;
		y1=y;

		if(y1>x1)
			{
			x1=y;
			y1=x;
			}

		if(matrix[x1][y1])
			{
			printf("They seem to be compatible :-)))\n\r\n\r");
			}
		else
			{
			printf("They don't seem to be compatible :-(((\n\r\n\r");
			}

		}
	else
		{
		printf("Rules %d,%d don't exist!\n\r\n\r");
		}

	}

void update_log(struct board *board)
	{
	time_t t;
	short x;
	FILE *h1;

	h1=fopen(LOGFILE,"a+");
	if(!h1) fatal_error("Cannot open logfile");

	time(&t);
	fprintf(h1,"%s",ctime(&t));
	fprintf(h1,"Game played: ");

	for(x=0;x<board->filled;x++)
		putc(board->moves[x]+0x31,h1);

	fprintf(h1," (%d moves)",board->filled);
	fprintf(h1," %s\n",eng[board->lastwin]);

    fprintf(h1,"\n");
	fclose(h1);
	}

void dump_file_threat(struct board *board,short anythreat)
	{
	short px1,px2,py1,py2;
	FILE *h1;

	h1=fopen(LOGFILE,"a+");
	if(!h1) fatal_error("Cannot open logfile");

	px1=board->xplace[anythreat][0];
	py1=board->yplace[anythreat][0];

	px2=board->xplace[anythreat][3];
	py2=board->yplace[anythreat][3];

	fprintf(h1,"Group taken as odd threat for white is %c%d-%c%d\n\n",px1+97,py1+1,px2+97,py2+1);
	fclose(h1);
	}

void dump_file_board(struct board *board)
	{
	short x,y,z;
	FILE *h1;

	h1=fopen(LOGFILE,"a+");
	if(!h1) fatal_error("Cannot write to logfile");

	fprintf(h1,"------------------------------------------------------------------------\n\n");
	fprintf(h1,"Current board status: ");

	for(x=0;x<board->filled;x++)
		fputc(board->moves[x]+0x31,h1);

	fprintf(h1,"\n\n");

	for(y=BOARDY-1;y>=0;y--)
		{
		for(x=0;x<BOARDX;x++)
			{
			z=board->square[ELM(x,y)];
			if(z==EMPTY) fprintf(h1," .");
			else if(z==WHITE) fprintf(h1," O");
			else fprintf(h1," X");
			}
		fprintf(h1,"\n\n");
		}

	fprintf(h1,"%s is to move\n",who[board->turn]);

	fclose(h1);
	}


/***************************************************************************/

short odd_threat(struct board *board,short x)
	{
	short y,empty=0,fill=0,emp[TILES],px,py;

	for(y=0;y<TILES;y++)
		{
		if(*board->groups[x][y]==EMPTY)
			{
			px=board->xplace[x][y];
			py=board->yplace[x][y];
			emp[empty++]=ELM(px,py);
			}
		else if (*board->groups[x][y]==WHITE) fill++;
		}

	if(empty==1 && fill==3 && (py&1)==0 && board->stack[px]<py) return emp[0];
	return -1;
	}

void wipe_above(struct board *board,short sq)
	{
	short x,y,z;

	x=ELX(sq);
	y=ELY(sq);

	for(z=y;z<BOARDY;z++)
		board->wipesq[ELM(x,z)]=1;
	}

void wipe_odd(struct board *board,short sq)
	{
	short x,y,z,ye;

	x=ELX(sq);
	y=(board->stack[x]&0x0e)+2;
	ye=ELY(sq);

	for(z=y;z<=ye;z+=2)
		board->wipesq[ELM(x,z)]=1;
	}

short wiped_group(struct board *board,short group)
	{
	short x,px,py;

	for(x=0;x<TILES;x++)
		{
		px=board->xplace[group][x];
		py=board->yplace[group][x];

		if(board->wipesq[ELM(px,py)]) return YES;
		}

	return NO;
	}

short check_threat(struct board *board,short px,short i,short side)
	{
	short x,y,p1,p2,fx,fy,j;

	for(y=0;y<board->solvable_groups->sqpnt[ELM(px,i)];y++)
		{
		j=board->solvable_groups->square[ELM(px,i)][y];

		if(board->xplace[j][0]==board->xplace[j][3]) continue;

		p1=0;
		p2=0;

		for(x=0;x<TILES;x++)
			{
			fx=board->xplace[j][x];
			fy=board->yplace[j][x];

			if(board->square[ELM(fx,fy)]==side) p1++;
			else if (board->square[ELM(fx,fy)]==EMPTY) p2++;
			}

		if(p1+p2==TILES) return YES;
		}

	return NO;
	}

short check_men(struct board *board,short group,short side)
	{
	short x,y,p1=0,p2=0,fx,fy;

	for(x=0;x<TILES;x++)
		{
		fx=board->xplace[group][x];
		fy=board->yplace[group][x];

		if(board->square[ELM(fx,fy)]==side) p1++;
		else if (board->square[ELM(fx,fy)]==EMPTY) p2++;
		}

	return ((p1+p2==4)?p1:-1);
	}


short check_even_below(struct board *board,short square,short side)
	{
	short px,py,i;

	px=ELX(square);
	py=ELY(square);

	for(i=1;i<py;i+=2)
		if(board->square[ELM(px,i)]==EMPTY && check_threat(board,px,i,side))
			return YES;

	return NO;
	}

short count_odd_threats(struct board *board,short *threats)
	{
	short x,y,oddpnt=0;

	for(x=0;x<GROUPS;x++)
		{
		y=odd_threat(board,x);
		if(y!=-1) threats[oddpnt++]=y;
		}

	return oddpnt;
	}

/* --------------------------------------------------------------- */

void both_groups(struct board *board,short q1,short q2)
	{
	short x,y,p1,p2,g1,g2;

	p1=board->solvable_groups->sqpnt[q1];
	p2=board->solvable_groups->sqpnt[q2];

	for(x=0;x<p1;x++)
		for(y=0;y<p2;y++)
			{
			g1=board->solvable_groups->square[q1][x];
			g2=board->solvable_groups->square[q2][y];
			if(g1==g2 && board->intgp.tgroups[g1])
				board->solution[board->sp]->solgroups[board->solution[board->sp]->solgroupsnumb++]=g1;
			}
	}

short recurse_groups(struct board *board,short cols,short *cl,short gp)
	{
	short p,i,g1,g2;

	p=board->solvable_groups->sqpnt[cl[0]];

	for(i=0;i<p;i++)
		{
		g1=board->solvable_groups->square[cl[0]][i];
		if(cols==1 && g1==gp) return YES;
		else if(g1==gp && recurse_groups(board,cols-1,&cl[1],g1)) return YES;
		}

	return NO;
	}

void both_many_groups(struct board *board,short cols,short *cl)
	{
	short p,i,g1;

	if(cols==0) return;

	p=board->solvable_groups->sqpnt[cl[0]];

	for(i=0;i<p;i++)
		{
		g1=board->solvable_groups->square[cl[0]][i];
		if(!board->intgp.tgroups[g1]) continue;

		if(cols==1 || recurse_groups(board,cols-1,&cl[1],g1))
			board->solution[board->sp]->solgroups[board->solution[board->sp]->solgroupsnumb++]=g1;
		}
	}

void solve_columns(struct board *board,short cl,short *cols)
	{
	short i,j,k,t,answer;
	short px,py,tx,ty;

	for(i=0;i<GROUPS;i++)
		{
		if(board->intgp.tgroups[i]!=YES) continue;

		answer=YES;

		for(j=0;j<cl && answer;j++)
			{
			answer=NO;

			px=ELX(cols[j]);
			py=ELY(cols[j]);

			for(k=0;k<TILES && !answer;k++)
				{
				if(*board->groups[i][k]==EMPTY &&
				  px==board->xplace[i][k] &&
				  py<=board->yplace[i][k]) answer=YES;
				}
			}

		if(j==cl && answer)
			{
			if(board->solution[board->sp]->solgroupsnumb==0)
				{
				board->solution[board->sp]->sqinvnumb=2*cl;
				for(t=0;t<cl;t++)
					{
					tx=ELX(cols[t]);
					ty=ELY(cols[t]);

					board->solution[board->sp]->sqinv[t   ]=ELM(tx,ty-1);
					board->solution[board->sp]->sqinv[t+cl]=ELM(tx,ty);
					}
				}
			board->solution[board->sp]->solgroups[board->solution[board->sp]->solgroupsnumb++]=i;
			}
		}
	}

void check_claim(struct board *board,short *cl)
	{
	short x,y,z;
	short px,py;

	px=ELX(cl[1]);
	py=ELY(cl[1])+1;

	if(py<BOARDY && (py&1)==1)
		{
		board->solution[board->sp]->solgroupsnumb=0;
		board->solution[board->sp]->solname=BASECLAIM;
		board->solution[board->sp]->sqinv[0]=cl[0];
		board->solution[board->sp]->sqinv[1]=cl[1];
		board->solution[board->sp]->sqinv[2]=cl[2];
		board->solution[board->sp]->sqinv[3]=ELM(px,py);
		board->solution[board->sp]->sqinvnumb=4;

		board->solution[board->sp]->solpoint[0]=cl[0];
		board->solution[board->sp]->solpoint[1]=ELM(px,py);
		both_groups(board,cl[0],ELM(px,py));
		board->instances[BASECLAIM]++;
		if(board->solution[board->sp]->solgroupsnumb>0)
			{
			both_groups(board,cl[1],cl[2]);
			board->sp++;
			}
		board->solution[board->sp]->solgroupsnumb=0;
		board->solution[board->sp]->solname=BASECLAIM;
		board->solution[board->sp]->sqinv[0]=cl[0];
		board->solution[board->sp]->sqinv[1]=cl[1];
		board->solution[board->sp]->sqinv[2]=cl[2];
		board->solution[board->sp]->sqinv[3]=ELM(px,py);
		board->solution[board->sp]->sqinvnumb=4;
		board->solution[board->sp]->solpoint[0]=ELM(px,py);
		board->solution[board->sp]->solpoint[1]=cl[2];
		both_groups(board,ELM(px,py),cl[2]);
		board->instances[BASECLAIM]++;
		if(board->solution[board->sp]->solgroupsnumb>0)
			{
			both_groups(board,cl[0],cl[1]);
			board->sp++;
			}
		}
	}

void generate_all_other_before_instances(struct board *board,short cols,short *cl,short j)
	{
	short cnt=0,step,pn[4];
	short gc[4][3],sl[4][2];
	short x,y,px,py,flag,px1,px2,py1,py2;

	step=(128>>cols);

	for(x=0;x<cols;x++)
		{
		px=ELX(cl[x]);
		py=ELY(cl[x]);

		gc[x][2]=cl[x];
		gc[x][1]=ELM(px,py-1);
		if(board->stack[px]<=py-2) gc[x][0]=ELM(px,py-2);
		else gc[x][0]=-1;
		}

	while(cnt<128)
		{
		pn[0]=(cnt>>6)&1;
		pn[1]=(cnt>>5)&1;
		pn[2]=(cnt>>4)&1;
		pn[3]=(cnt>>3)&1;

		for(x=0;x<cols;x++)
			{
			sl[x][1]=gc[x][1+pn[x]];
			sl[x][0]=gc[x][0+pn[x]];
			}

		flag=YES;
		for(x=0;x<cols && flag;x++)
			if(sl[x][0]==-1) flag=FALSE;

		for(y=0;y<2 && flag;y++)
			for(x=0;x<cols && flag;x++)
				if(board->sqused[sl[x][y]]==NO) flag=FALSE;

		if(flag)
			{
			board->solution[board->sp]->solgroupsnumb=0;
			board->solution[board->sp]->solname=BEFORE;
			board->solution[board->sp]->solpoint[0]=ELM(board->xplace[j][0],board->yplace[j][0]);
			board->solution[board->sp]->solpoint[1]=ELM(board->xplace[j][3],board->yplace[j][3]);

			board->solution[board->sp]->sqinvnumb=2*cols;
			for(x=0;x<2*cols;x++)
				board->solution[board->sp]->sqinv[x]=sl[x>>1][x&1];

			for(x=0;x<cols;x++)
				{
				py2=ELY(sl[x][1]);

				if((py2&1)==1) both_many_groups(board,1,&sl[x][1]);
				else both_groups(board,sl[x][0],sl[x][1]);
				}

			both_many_groups(board,cols,cl);
			board->instances[BEFORE]++;

			if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
			}

		cnt+=step;
		}
	}

/* ******************************************************************** */


/* ******************************************************************** */

void check_double_threat(struct board *board,short x,short y,
								 struct threat_combo *tch,short *pnt)
	{
	short j,k,wx,pq,w1,w2,px,py,g1,g2,jg,kg;

	pq=ELM(x,y);

	for(j=0;j<board->solvable_groups->sqpnt[pq]-1;j++)
		for(k=j+1;k<board->solvable_groups->sqpnt[pq];k++)
			{
         jg=board->solvable_groups->square[pq][j];
			kg=board->solvable_groups->square[pq][k];

			w1=check_men(board,jg,WHITE);
			w2=check_men(board,kg,WHITE);

			if(w1!=2 || w2!=2) continue;

			for(wx=0;wx<TILES;wx++)
				{
				px=board->xplace[jg][wx];
				py=board->yplace[jg][wx];

				if(*board->groups[jg][wx]==EMPTY && (px!=x || py!=y))
					g1=ELM(px,py);
				}

			for(wx=0;wx<TILES;wx++)
				{
				px=board->xplace[kg][wx];
				py=board->yplace[kg][wx];

				if(*board->groups[kg][wx]==EMPTY && (px!=x || py!=y))
					g2=ELM(px,py);
				}

			if(ELX(g1)==ELX(g2) && abs(ELY(g1)-ELY(g2))==1)
				{
				tch[*pnt].cross=pq;

				if((ELY(g1)&1)==1)
					{
					tch[*pnt].even=g1;
					tch[*pnt].odd =g2;
					}
				else
					{
					tch[*pnt].odd =g1;
					tch[*pnt].even=g2;
					}

				tch[*pnt].gp1=jg;
				tch[*pnt].gp2=kg;

				(*pnt)++;
				}
			}
	}

short threat_combo(struct board *board,struct threat_combo *tc)
	{
	short x,y,z=0;

	/* The common square must be odd */
	for(y=2;y<BOARDY;y+=2)
		for(x=0;x<BOARDX;x++)
			if(board->stack[x]<y) check_double_threat(board,x,y,tc,&z);

	return z;
	}

/* --------------------------------------------------------------- */

void wipe_many_groups(struct board *board,short cols,short *cl)
	{
	short p,i,g1;

	if(cols==0) return;

	p=board->solvable_groups->sqpnt[cl[0]];

	for(i=0;i<p;i++)
		{
		g1=board->solvable_groups->square[cl[0]][i];
		if(!board->usablegroup[g1]) continue;

		if(cols==1 || recurse_groups(board,cols-1,&cl[1],g1))
			board->usablegroup[g1]=NO;
		}
	}



void handle_even_above_odd(struct board *board,struct threat_combo *tc)
	{
	short x,y,y1,y2,px,py,qx,qy,sx,sy;
	short cl[2];

	/* Rule one */
	wipe_odd(board,tc->cross);

	/* Rule two */
	px=ELX(tc->cross);
	py=ELY(tc->cross)+1;

	qx=ELX(tc->odd);
	qy=ELY(tc->odd)+1;

	for(y1=qy;y1<BOARDY;y1++)
		for(y2=py;y2<BOARDY;y2++)
			{
			cl[0]=ELM(qx,y1);
			cl[1]=ELM(px,y2);

			wipe_many_groups(board,2,cl);
			}

	/* Rule three */

	cl[0]=tc->odd;
	cl[1]=ELM(ELX(tc->cross),ELY(tc->cross)+1);
	wipe_many_groups(board,2,cl);


	/* Rule four */

	if(board->stack[qx]==ELY(tc->odd))
		{
		sx=px;
		sy=py+1;

		wipe_above(board,ELM(sx,sy));
		}

	/* Rule five */

	if((board->stack[px]&1)==0 && board->stack[qx]<(qy-1))
		{
		cl[0]=ELM(px,board->stack[px]);
		cl[1]=ELM(qx,board->stack[qx]);

		wipe_many_groups(board,2,cl);
		}

	/* Rule six */

	qx=ELX(tc->odd);
	qy=board->stack[qx];

	for(y=qy;y<BOARDY-1;y++)
		{
		cl[0]=ELM(qx,y);
		cl[1]=ELM(qx,y+1);

		wipe_many_groups(board,2,cl);
		}
	}


void handle_odd_above_even(struct board *board,struct threat_combo *tc)
	{
	short x,y,y1,y2,px,py,qx,qy;
	short cl[2];

	/* Rule one */
	wipe_odd(board,tc->cross);

	/* Rule two */
	px=ELX(tc->cross);
	py=ELY(tc->cross)+1;

	qx=ELX(tc->even);
	qy=ELY(tc->even)+1;

	for(y1=qy;y1<BOARDY;y1++)
		for(y2=py;y2<BOARDY;y2++)
			{
			cl[0]=ELM(qx,y1);
			cl[1]=ELM(px,y2);

			wipe_many_groups(board,2,cl);
			}

	/* Rule three */

	if((board->stack[px]&1)==0 && board->stack[qx]<(qy-1))
		{
		cl[0]=ELM(px,board->stack[px]);
		cl[1]=ELM(qx,board->stack[qx]);

		wipe_many_groups(board,2,cl);
		}

	/* Rule four */

	qx=ELX(tc->odd);
	qy=board->stack[qx];

	for(y=qy;y<BOARDY-1;y++)
		{
		cl[0]=ELM(qx,y);
		cl[1]=ELM(qx,y+1);

		wipe_many_groups(board,2,cl);
		}
	}


/* --------------------------------------------------------------- */



/* --------------------------------------------------------------- */

void claimeven(struct board *board)
	{
	short x1,y1,q1,q2;
	short sln,*grp,j,k;

	for(y1=1;y1<BOARDY;y1+=2)
		for(x1=0;x1<BOARDX;x1++)
			{
			if(!board->sqused[ELM(x1,y1)]) continue;

			board->solution[board->sp]->solgroupsnumb=0;

			q1=ELM(x1,y1);
			q2=ELM(x1,y1-1);

			if(board->square[q1]==EMPTY &&
				board->square[q2]==EMPTY && board->sqused[q2])
					{
					sln=board->solvable_groups->sqpnt[q1];
					grp=board->solvable_groups->square[q1];

					for(j=0;j<sln;j++)
						if(board->intgp.tgroups[grp[j]]==YES)
							{
							if(board->solution[board->sp]->solgroupsnumb==0)
								{
								board->solution[board->sp]->solname=CLAIMEVEN;
								board->solution[board->sp]->solpoint[0]=q1;
								board->solution[board->sp]->solpoint[1]=q2;
								board->solution[board->sp]->sqinv[0]=q1;
								board->solution[board->sp]->sqinv[1]=q2;
								board->solution[board->sp]->sqinvnumb=2;
                        board->instances[CLAIMEVEN]++;
								}
							board->solution[board->sp]->solgroups[board->solution[board->sp]->solgroupsnumb++]=grp[j];
							}
					}

			if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
			}
	}


void baseinverse(struct board *board)
	{
	short x1,y1,q1,q2,px,py,wx,wy;
	short sln,*grp,j,k,x,y;
	char  set[64];

	for(y1=0;y1<BOARDY;y1++)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			if(board->stack[x1]==y1)
				{
				memset(set,YES,64);

				sln=board->solvable_groups->sqpnt[q1];
				grp=board->solvable_groups->square[q1];

				for(j=0;j<sln;j++)
					{
					if(board->intgp.tgroups[grp[j]]==YES)
						{
						for(x=0;x<TILES;x++)
							{
							wx=board->xplace[grp[j]][x];
							wy=board->yplace[grp[j]][x];

							if(x1<wx && board->stack[wx]==wy &&
								set[ELM(wx,wy)] && board->sqused[ELM(wx,wy)])
								{
								set[ELM(wx,wy)]=NO;
								board->solution[board->sp]->solgroupsnumb=0;
								board->solution[board->sp]->solname=BASEINVERSE;
								board->solution[board->sp]->solpoint[0]=q1;
								board->solution[board->sp]->solpoint[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinv[0]=q1;
								board->solution[board->sp]->sqinv[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinvnumb=2;
								both_groups(board,q1,ELM(wx,wy));
								board->instances[BASEINVERSE]++;
								if(board->solution[board->sp]->solgroupsnumb>0)
									board->sp++;
								}
							}
						}
					}
				}
			}
	}


void vertical(struct board *board)
	{
	short x1,y1,q1,wy;
	short sln,*grp,j,x;
	char set[64];

	for(y1=0;y1<BOARDY;y1++)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			if(board->square[q1]==EMPTY)
				{
				memset(set,YES,64);

				sln=board->solvable_groups->sqpnt[q1];
				grp=board->solvable_groups->square[q1];

				for(j=0;j<sln;j++)
					{
					if(board->intgp.tgroups[grp[j]]==YES &&
						board->xplace[grp[j]][0]==board->xplace[grp[j]][3])
							{
							for(x=0;x<TILES;x++)
								{
								wy=board->yplace[grp[j]][x];

								if(wy>0 && (wy&1)==0 && y1==wy-1 &&
									set[ELM(x1,wy)] && board->sqused[ELM(x1,wy)])
									{
									set[ELM(x1,wy)]=NO;
									board->solution[board->sp]->solgroupsnumb=0;
									board->solution[board->sp]->solname=VERTICAL;
									board->solution[board->sp]->solpoint[0]=q1;
									board->solution[board->sp]->solpoint[1]=ELM(x1,wy);
									board->solution[board->sp]->sqinv[0]=q1;
									board->solution[board->sp]->sqinv[1]=ELM(x1,wy);
									board->solution[board->sp]->sqinvnumb=2;
									both_groups(board,q1,ELM(x1,wy));
									board->instances[VERTICAL]++;
									if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
									}
								}

							}
					}
				}

			}
	}


void aftereven(struct board *board)
	{
	short x1,y1,q1,px,py,aftereven;
	short sln,*grp,j,x,cols,cl[4],pj;

	for(y1=1;y1<BOARDY;y1+=2)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			sln=board->solvable_groups->sqpnt[q1];
			grp=board->solvable_groups->square[q1];

			for(j=0;j<sln;j++)
				{
				if(board->intgp.mygroups[grp[j]]==YES &&
					x1==board->xplace[grp[j]][0] &&
					x1<board->xplace[grp[j]][3])
						{
						aftereven=YES;
						cols=0;
						memset(cl,0xff,8);

						for(x=0;x<TILES && aftereven;x++)
							{
							px=board->xplace[grp[j]][x];
							py=board->yplace[grp[j]][x];

							if(board->sqused[ELM(px,py)]==NO) aftereven=NO;

							if(board->square[ELM(px,py)]==EMPTY)
								{
								if((py&1)==1 && board->stack[px]<=py-1)
									cl[cols++]=ELM(px,py);
								else aftereven=NO;
								}
							}

						if(aftereven && cols>0)
							{
							board->solution[board->sp]->solgroupsnumb=0;
							board->solution[board->sp]->solname=AFTEREVEN;
							board->solution[board->sp]->solpoint[0]=q1;
							board->solution[board->sp]->solpoint[1]=ELM(board->xplace[grp[j]][3],board->yplace[grp[j]][3]);
							board->instances[AFTEREVEN]++;

							board->solution[board->sp]->sqinvnumb=cols;
							for(pj=0;pj<cols;pj++)
								board->solution[board->sp]->sqinv[pj]=cl[pj];

							solve_columns(board,cols,cl);
							if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
							}
						}
				}

			}
	}


void lowinverse(struct board *board)
	{
	short x1,y1,q1,q2,px,py,wx,wy;
	short sln,*grp,j,k,x,y;
	char set[64];

	for(y1=2;y1<BOARDY;y1+=2)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			if(board->stack[x1]<y1)
				{
				memset(set,YES,64);

				sln=board->solvable_groups->sqpnt[q1];
				grp=board->solvable_groups->square[q1];

				for(j=0;j<sln;j++)
					{
					if(board->intgp.tgroups[grp[j]]==YES &&
						board->xplace[grp[j]][0]!=board->xplace[grp[j]][3])
						{
						for(x=0;x<TILES;x++)
							{
							wx=board->xplace[grp[j]][x];
							wy=board->yplace[grp[j]][x];

							if(x1<wx && board->stack[wx]<wy && wy>0 &&
								(wy&1)==0 && set[ELM(wx,wy)] &&
								board->sqused[ELM(wx,wy)])
								{
								set[ELM(wx,wy)]=NO;
								board->solution[board->sp]->solgroupsnumb=0;
								board->solution[board->sp]->solname=LOWINVERSE;
								board->solution[board->sp]->solpoint[0]=q1;
								board->solution[board->sp]->solpoint[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinv[0]=q1;
								board->solution[board->sp]->sqinv[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinv[2]=ELM(x1,y1-1);
								board->solution[board->sp]->sqinv[3]=ELM(wx,wy-1);
								board->solution[board->sp]->sqinvnumb=4;
								both_groups(board,q1,ELM(wx,wy));
								both_groups(board,q1,ELM(x1,y1-1));
								both_groups(board,ELM(wx,wy),ELM(wx,wy-1));
								board->instances[LOWINVERSE]++;
								if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
								}
							}
						}
					}
				}
			}
	}

void highinverse(struct board *board)
	{
	short x1,y1,q1,q2,px,py,wx,wy;
	short sln,*grp,j,k,x,y;
	char set[64];

	for(y1=2;y1<BOARDY;y1+=2)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			if(board->stack[x1]<y1)
				{
				memset(set,YES,64);

				sln=board->solvable_groups->sqpnt[q1];
				grp=board->solvable_groups->square[q1];

				for(j=0;j<sln;j++)
					{
					if(board->intgp.tgroups[grp[j]]==YES &&
						board->xplace[grp[j]][0]!=board->xplace[grp[j]][3])
						{
						for(x=0;x<TILES;x++)
							{
							wx=board->xplace[grp[j]][x];
							wy=board->yplace[grp[j]][x];

							if(x1<wx && board->stack[wx]<wy && wy>0 &&
								(wy&1)==0 && set[ELM(wx,wy)] &&
								board->sqused[ELM(wx,wy)])
								{
								set[ELM(wx,wy)]=NO;
								board->solution[board->sp]->solgroupsnumb=0;
								board->solution[board->sp]->solname=HIGHINVERSE;
								board->solution[board->sp]->solpoint[0]=q1;
								board->solution[board->sp]->solpoint[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinv[0]=q1;
								board->solution[board->sp]->sqinv[1]=ELM(wx,wy);
								board->solution[board->sp]->sqinv[2]=ELM(x1,y1-1);
								board->solution[board->sp]->sqinv[3]=ELM(wx,wy-1);
								board->solution[board->sp]->sqinv[4]=ELM(x1,y1+1);
								board->solution[board->sp]->sqinv[5]=ELM(wx,wy+1);
								board->solution[board->sp]->sqinvnumb=6;
								board->instances[HIGHINVERSE]++;

								/* Upper and middle squares */
								both_groups(board,ELM(x1,y1+1),ELM(wx,wy+1));
								both_groups(board,q1,ELM(wx,wy));

								/* Vertical groups */
								both_groups(board,ELM(x1,y1),ELM(x1,y1+1));
								both_groups(board,ELM(wx,wy),ELM(wx,wy+1));

								/* Lower 1st and Upper 2nd if lower is playable */
								if(board->stack[x1]==y1-1)
									both_groups(board,ELM(x1,y1-1),ELM(wx,wy+1));

								/* Upper 1st and Lower 2nd if lower is playable */
								if(board->stack[wx]==wy-1)
									both_groups(board,ELM(x1,y1+1),ELM(wx,wy-1));

								if(board->solution[board->sp]->solgroupsnumb>0) board->sp++;
								}
							}
						}
					}
				}
			}
	}


void baseclaim(struct board *board)
	{
	short x1,y1,q1,q2,px,py,wx,wy;
	short sln,*grp,j,k,x,y,cl[6],cols;
	char set[64];

	memset(set,YES,64);
	for(y1=0;y1<BOARDY;y1++)
		for(x1=0;x1<BOARDX;x1++)
			{
			if(!board->sqused[ELM(x1,y1)]) continue;
			if(board->stack[x1]==y1)
				{
				q1=ELM(x1,y1);

				sln=board->solvable_groups->sqpnt[q1];
				grp=board->solvable_groups->square[q1];

				for(j=0;j<sln;j++)
					{
					if(board->intgp.tgroups[grp[j]]==YES &&
						board->xplace[grp[j]][0]!=board->xplace[grp[j]][3])
						{
						cols=0;

						for(x=0;x<TILES;x++)
							{
							wx=board->xplace[grp[j]][x];
							wy=board->yplace[grp[j]][x];

							if(board->sqused[ELM(wx,wy)]==0) continue;
							if(board->stack[wx]==wy) cl[cols++]=ELM(wx,wy);
							}

						if(cols!=3) continue;
						else
							{
							cl[3]=cl[1];
							cl[4]=cl[0];

							if(set[cl[0]]) check_claim(board,&cl[0]);

							set[cl[0]]=NO;
							}              						}
					}
				}
			}
	}


void before(struct board *board)
	{
	short j,px,py,before;
	short x,cols,cl[TILES];

	for(j=0;j<GROUPS;j++)
		{
		if(!board->intgp.mygroups[j] ||
			board->xplace[j][0]==board->xplace[j][3] ||
			board->yplace[j][0]==BOARDY-1 ||
			board->yplace[j][3]==BOARDY-1) continue;

		cols=0;
		before=YES;

		for(x=0;x<TILES && before;x++)
			{
			px=board->xplace[j][x];
			py=board->yplace[j][x];

			if(!board->sqused[ELM(px,py)]) before=NO;
			else if(board->square[ELM(px,py)]==EMPTY)
				cl[cols++]=ELM(px,py+1);

			}

		if(before && cols>0)
			{
			for(x=0;x<cols;x++)
				board->solution[board->sp]->sqinv[x]=cl[x];

			generate_all_other_before_instances(board,cols,cl,j);
			}
		}
	}
/*

void before(struct board *board)
	{
	short x1,y1,q1,px,py,before,pj,fx,fy;
	short sln,*grp,j,x,cols,cl[TILES];

	for(y1=0;y1<BOARDY;y1++)
		for(x1=0;x1<BOARDX;x1++)
			{
			q1=ELM(x1,y1);
			if(!board->sqused[q1]) continue;

			sln=board->solvable_groups->sqpnt[q1];
			grp=board->solvable_groups->square[q1];

			for(j=0;j<sln;j++)
				{
				if(board->intgp.mygroups[grp[j]]==YES &&
					x1==board->xplace[grp[j]][0] &&
					x1<board->xplace[grp[j]][3])
						{
						before=YES;
						cols=0;
						memset(cl,0xff,8);

						for(x=0;x<TILES && before;x++)
							{
							px=board->xplace[grp[j]][x];
							py=board->yplace[grp[j]][x];

							if(board->square[ELM(px,py)]==EMPTY)
								{
								if(py<BOARDY-1) cl[cols++]=ELM(px,py+1);
								else before=NO;
								}
							}


						if(before && cols>0)
							{
							for(pj=0;pj<cols;pj++)
								board->solution[board->sp]->sqinv[pj]=cl[pj];

							generate_all_other_before_instances(board,cols,cl,grp[j]);
							}
						}
				}

			}
	}


/* --------------------------------------------------------------- */

short threat_group(struct board *board,short group,short who)
	{
	short p;

	p=((*board->groups[group][0])|(*board->groups[group][1])|
		(*board->groups[group][2])|(*board->groups[group][3]));

	if(p&who) return NO;
	return YES;
	}

/* -------------------------------------------------------------------- */

short check_early_win(struct board *board)
	{
	if(board->square[SQ_d1]==WHITE &&
		board->square[SQ_c3]==WHITE &&
		board->square[SQ_e3]==WHITE &&
		(board->square[SQ_c2]==WHITE || board->square[SQ_e2]==WHITE) &&
		board->square[SQ_b2]==EMPTY &&
		board->square[SQ_d2]==EMPTY &&
		board->square[SQ_f2]==EMPTY) return YES;

	return NO;
	}

short anypentas(struct board *board)
	{
	short an=0;

	if(board->square[SQ_c1]!=WHITE && board->square[SQ_e1]!=WHITE) return 0;

	if(board->square[SQ_b3]==WHITE &&
		board->square[SQ_d3]==WHITE &&
		(board->square[SQ_b2]==WHITE || board->square[SQ_d2]==WHITE) &&
		board->square[SQ_a2]==EMPTY &&
		board->square[SQ_c2]==EMPTY &&
		board->square[SQ_e2]==EMPTY) an|=2;

	if(board->square[SQ_d3]==WHITE &&
		board->square[SQ_f3]==WHITE &&
		(board->square[SQ_d2]==WHITE || board->square[SQ_f2]==WHITE) &&
		board->square[SQ_c2]==EMPTY &&
		board->square[SQ_e2]==EMPTY &&
		board->square[SQ_g2]==EMPTY) an|=1;

	return an;
	}

void dump_instances(struct board *board)
	{
	short x,y,i,px1,py1,px2,py2;

	for(x=1;x<=9;x++)
		{
		printf("%13s ",rulename[x-1]);

		for(y=0;y<board->sp;y++)
			{
			if(board->solution[y]->solname==x)
				{
				px1=ELX(board->solution[y]->solpoint[0]);
				py1=ELY(board->solution[y]->solpoint[0]);
				px2=ELX(board->solution[y]->solpoint[1]);
				py2=ELY(board->solution[y]->solpoint[1]);

				printf("(%c%d,%c%d) ",px1+65,py1+1,px2+65,py2+1);
				}
			}
		printf("\n\r");
		}
	}

void ss_dump_instances(struct board *board,FILE *h1)
	{
	short x,y,i,px1,py1,px2,py2;

	for(x=1;x<=9;x++)
		{
        fprintf(h1,"%13s ",rulename[x-1]);

		for(y=0;y<board->sp;y++)
			{
			if(board->solution[y]->solname==x)
				{
				px1=ELX(board->solution[y]->solpoint[0]);
				py1=ELY(board->solution[y]->solpoint[0]);
				px2=ELX(board->solution[y]->solpoint[1]);
				py2=ELY(board->solution[y]->solpoint[1]);

                fprintf(h1,"(%c%d,%c%d) ",px1+65,py1+1,px2+65,py2+1);
				}
			}
        fprintf(h1,"\n\r");
		}
	}

void dump_solutions(struct board *board,short sol)
	{
	short x,y,px1,px2,py1,py2,gp,k,found=0;
	FILE *h1;

	h1=fopen(LOGFILE,"a+");
	if(!h1) fatal_error("Cannot open logfile");

	printf("\n\r");
	printf("%s\n\r",rulename[sol-1]);

	fprintf(h1,"\n");
	fprintf(h1,"%s\n",rulename[sol-1]);

	for(x=0;x<board->sp;x++)
		{
		if(board->solution[x]->solname==sol)
			{
			found++;

			px1=ELX(board->solution[x]->solpoint[0]);
			py1=ELY(board->solution[x]->solpoint[0]);
			px2=ELX(board->solution[x]->solpoint[1]);
			py2=ELY(board->solution[x]->solpoint[1]);

			printf("(%c%d,%c%d) [%3d]--> ",px1+65,py1+1,px2+65,py2+1,x);
			fprintf(h1,"(%c%d,%c%d) [%3d]--> ",px1+65,py1+1,px2+65,py2+1,x);

			for(y=0;y<board->solution[x]->solgroupsnumb;y++)
				{
				gp=board->solution[x]->solgroups[y];

				px1=board->xplace[gp][0];
				py1=board->yplace[gp][0];
				px2=board->xplace[gp][3];
				py2=board->yplace[gp][3];

				printf("%c%d%c%d  ",px1+97,py1+1,px2+97,py2+1);
				fprintf(h1,"%c%d%c%d  ",px1+97,py1+1,px2+97,py2+1);
				}

			printf("\n\r");
			fprintf(h1,"\n");
			for(k=0;k<board->solution[x]->sqinvnumb;k++)
				{
				gp=board->solution[x]->sqinv[k];
				printf("%c%d ",ELX(gp)+97,ELY(gp)+1);
				fprintf(h1,"%c%d ",ELX(gp)+97,ELY(gp)+1);
				}

			printf("\n\r");
			fprintf(h1,"\n");
			}
		}

	printf("\n\r");
	fprintf(h1,"\n");

	printf("Total solution found of specified kind: %d\n\r",found);
	fprintf(h1,"Total solution found of specified kind: %d\n",found);

	printf("Output end.\n\r");

	fclose(h1);
	}

void debug_black(struct board *board)
	{
	short key,oracle;
	register x;
	char **matrix;
	short i,*bpos;


	board->sp=0;

	board->intgp.j=0;
	board->intgp.k=0;

	memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));

	for(i=0;i<GROUPS;i++)
		{
		if(threat_group(board,i,BLACK))
			{
			board->intgp.tgroups[i]=YES;
			board->intgp.j++;
			}
		else board->intgp.tgroups[i]=NO;

		if(threat_group(board,i,WHITE))
			{
			board->intgp.mygroups[i]=YES;
			board->intgp.k++;
			}
		else board->intgp.mygroups[i]=NO;
		}

	claimeven(board);
	baseinverse(board);
	vertical(board);
	aftereven(board);
	lowinverse(board);
	highinverse(board);
	baseclaim(board);
	before(board);

	dump_instances(board);
	dump_file_board(board);

	matrix=(char **)allocate_matrix(board);
	build_adjacency_matrix(matrix,board);

    oracle=problem_solver(board,matrix,NO,NULL);

	printf("\n\r");
	printf("Total sol. found : %d, total dangerous groups %d, groups solved %d\n\r",
				board->sp,board->intgp.j,board->problem_solved);
	printf("Oracle spits out %d\n\r",oracle);

	printf("Key 1 - 9 to see rules instances, ESC to return to game play\n\r");
	printf("j to dump the non adjacencies, t to test rule compatibility\n\r");

	while((key=readkey())!=0x1b)
		{
		switch(key)
			{
			case '1':
				dump_solutions(board,CLAIMEVEN);
				break;

			case '2':
				dump_solutions(board,BASEINVERSE);
				break;

			case '3':
				dump_solutions(board,VERTICAL);
				break;

			case '4':
				dump_solutions(board,AFTEREVEN);
				break;

			case '5':
				dump_solutions(board,LOWINVERSE);
				break;

			case '6':
				dump_solutions(board,HIGHINVERSE);
				break;

			case '7':
				dump_solutions(board,BASECLAIM);
				break;

			case '8':
				dump_solutions(board,BEFORE);
				break;

			case '9':
				dump_solutions(board,SPECIALBEFORE);
				break;

			case 'j':
				dump_no_adjacencies(matrix,board);
				break;

			case 't':
				test_conditions(matrix,board);
				break;
			}
		}

	free_matrix(matrix,board);
	}


void debug_white(struct board *board)
	{
	short x,y,x1,y1,f,g,j;
	struct threat_combo tc[GROUPS];
	short threats[GROUPS],thnumb,anythreat=0,ccmb;
	short key,oracle,combo,pentas;
	char **matrix;
	short i,*bpos,px,py;

	if(check_early_win(board))
		{
		printf("An early win is detected! (ESC to return)\n\r");
		while(readkey()!=0x1b);
		return;
		}

    pentas=0;
	combo=threat_combo(board,tc);

	thnumb=count_odd_threats(board,threats);
	if(thnumb+combo==0 && !pentas)
		{
		printf("No decisive threats, (ESC to return)\n\r");
		while(readkey()!=0x1b);
		return;
		}

	do
		{
		memset(board->usablegroup,0xff,GROUPS*sizeof(short));
		memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));
		memset(board->wipesq,0x00,(BOARDX+1)*(BOARDY+2)*sizeof(short));

		if(anythreat<thnumb)
			{
			wipe_above(board,threats[anythreat]);
			wipe_odd(board,threats[anythreat]);

			px=ELX(threats[anythreat]);
			for(py=0;py<BOARDY;py++)
                if(board->square[ELM(px,py)]==EMPTY)
                    board->sqused[ELM(px,py)]=NO;
			}
		else if(anythreat<thnumb+combo)
			{
			ccmb=anythreat-thnumb;

			px=ELX(tc[ccmb].cross);
			x =ELX(tc[ccmb].odd);

			for(y=0;y<BOARDY;y++)
				{
				if(board->square[ELM(px,y)]==EMPTY)
					board->sqused[ELM(px,y)]=NO;

				if(board->square[ELM( x,y)]==EMPTY)
					board->sqused[ELM( x,y)]=NO;
				}

			if(ELY(tc[ccmb].even)>ELY(tc[ccmb].odd))
				handle_even_above_odd(board,&tc[ccmb]);
            else handle_odd_above_even(board,&tc[ccmb]);
            }
		else fatal_error("What am I doing?");

		board->sp=0;
		board->intgp.j=0;
		board->intgp.k=0;

		for(i=0;i<GROUPS;i++)
			{
			if(threat_group(board,i,WHITE) &&
				!wiped_group(board,i) && board->usablegroup[i])
				{
				board->intgp.tgroups[i]=YES;
				board->intgp.j++;
				}
			else board->intgp.tgroups[i]=NO;

			if(threat_group(board,i,BLACK))
				{
				board->intgp.mygroups[i]=YES;
				board->intgp.k++;
				}
			else board->intgp.mygroups[i]=NO;
			}

		claimeven(board);
		baseinverse(board);
		vertical(board);
		aftereven(board);
		lowinverse(board);
		highinverse(board);
		baseclaim(board);
		before(board);

		dump_instances(board);
		dump_file_board(board);

		if(anythreat<thnumb) dump_file_threat(board,anythreat);

		matrix=(char **)allocate_matrix(board);
		build_adjacency_matrix(matrix,board);

        oracle=problem_solver(board,matrix,NO,NULL);

		printf("\n\r");
		printf("Total sol. found : %d, total dangerous groups %d, groups solved %d\n\r",
				board->sp,board->intgp.j,board->problem_solved);


		if(anythreat<thnumb)
			{
			printf("Odd threat at %c%d, oracle spits out %d\n\r",
						ELX(threats[anythreat])+97,ELY(threats[anythreat])+1,oracle);
			}
		else if(anythreat<thnumb+combo)
			{
			printf("Double odd threat at %c%d, oracle spits out %d\n\r",
						ELX(tc[anythreat-thnumb].cross)+97,ELY(tc[anythreat-thnumb].cross)+1,oracle);
			}
		else
			{
			printf("Pentas found.\n\r",pentas);
			}


        printf("1 - 9 to see rules instances, ESC to return to game play\n");
        printf("j to dump the non adjacencies, t to test rule compatibility\n");
        printf("s to show squares available\n");

		while((key=readkey())!=0x1b)
			{
			switch(key)
				{
				case '1':
					dump_solutions(board,CLAIMEVEN);
					break;

				case '2':
					dump_solutions(board,BASEINVERSE);
					break;

				case '3':
					dump_solutions(board,VERTICAL);
					break;

				case '4':
					dump_solutions(board,AFTEREVEN);
					break;

				case '5':
					dump_solutions(board,LOWINVERSE);
					break;

				case '6':
					dump_solutions(board,HIGHINVERSE);
					break;

				case '7':
					dump_solutions(board,BASECLAIM);
					break;

				case '8':
					dump_solutions(board,BEFORE);
					break;

				case '9':
					dump_solutions(board,SPECIALBEFORE);
					break;

				case 'j':
					dump_no_adjacencies(matrix,board);
					break;

				case 't':
					test_conditions(matrix,board);
					break;

                case 's':
                    show_square_used(board);
                    break;

				}
			}

		free_matrix(matrix,board);
		anythreat++;
		} while(anythreat<thnumb+combo || pentas);

	}

void debug_function(struct board *board)
	{
    clrscr();
    switch(board->turn)
		{
		case WHITE:
			debug_black(board);
			break;

		case BLACK:
			debug_white(board);
			break;

		}
	}


/* -------------------------------------------------------------------- */

// Main evaluation function for the oracle

short evaluate_black(struct board *board)
	{
	short key,oracle;
	register x;
	char **matrix;
	short i,*bpos;

	board->sp=0;

	board->intgp.j=0;
	board->intgp.k=0;

	memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));

	for(i=0;i<GROUPS;i++)
		{
		if(threat_group(board,i,BLACK))
			{
			board->intgp.tgroups[i]=YES;
			board->intgp.j++;
			}
		else board->intgp.tgroups[i]=NO;

		if(threat_group(board,i,WHITE))
			{
			board->intgp.mygroups[i]=YES;
			board->intgp.k++;
			}
		else board->intgp.mygroups[i]=NO;
		}

	claimeven(board);
	baseinverse(board);
	vertical(board);
	aftereven(board);
	lowinverse(board);
	highinverse(board);
	baseclaim(board);
	before(board);

	if(board->intgp.j==0) return YES;
	if(board->sp==0) return NO;

	matrix=(char **)allocate_matrix(board);
	build_adjacency_matrix(matrix,board);

    oracle=problem_solver(board,matrix,NO,NULL);
	free_matrix(matrix,board);

	return oracle;
	}


short evaluate_white(struct board *board)
	{
	short x,y;
	struct threat_combo tc[GROUPS];
	short threats[MAXGROUPS],thnum=0,combo,ccmb;
	short key,oracle,anythreat=0;
	char **matrix;
	short i,*bpos,px,py;

	combo=threat_combo(board,tc);
	thnum=count_odd_threats(board,threats);
	if(thnum+combo==0) return NO;

	do
		{
		memset(board->usablegroup,0xff,GROUPS*sizeof(short));
		memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));
		memset(board->wipesq,0x00,(BOARDX+1)*(BOARDY+2)*sizeof(short));

		if(anythreat<thnum)
			{
			wipe_above(board,threats[anythreat]);
			wipe_odd(board,threats[anythreat]);

			px=ELX(threats[anythreat]);
			for(py=0;py<BOARDY;py++)
                if(board->square[ELM(px,py)]==EMPTY)
                    board->sqused[ELM(px,py)]=NO;
			}
		else
			{
			ccmb=anythreat-thnum;

			px=ELX(tc[ccmb].cross);
			x =ELX(tc[ccmb].odd);

			for(y=0;y<BOARDY;y++)
				{
				if(board->square[ELM(px,y)]==EMPTY)
					board->sqused[ELM(px,y)]=NO;

				if(board->square[ELM( x,y)]==EMPTY)
					board->sqused[ELM( x,y)]=NO;
				}

			if(ELY(tc[ccmb].even)>ELY(tc[ccmb].odd))
				handle_even_above_odd(board,&tc[ccmb]);
			else
				handle_odd_above_even(board,&tc[ccmb]);
			}

		board->sp=0;
		board->intgp.j=0;
		board->intgp.k=0;

		for(i=0;i<GROUPS;i++)
			{
			if(threat_group(board,i,WHITE) &&
				!wiped_group(board,i) && board->usablegroup[i])
				{
				board->intgp.tgroups[i]=YES;
				board->intgp.j++;
				}
			else board->intgp.tgroups[i]=NO;

			if(threat_group(board,i,BLACK))
				{
				board->intgp.mygroups[i]=YES;
				board->intgp.k++;
			}
			else board->intgp.mygroups[i]=NO;
			}

        for(y=0;y<BOARDY;y++)
            for(x=0;x<BOARDX;x++)
                if(board->square[ELM(x,y)]!=EMPTY &&
                   board->sqused[ELM(x,y)]==NO)
                      fatal_error("Marked as used an useless square...");

		claimeven(board);
		baseinverse(board);
		vertical(board);
		aftereven(board);
		lowinverse(board);
		highinverse(board);
		baseclaim(board);
		before(board);

		if(board->intgp.j==0) oracle=YES;
		else if(board->sp==0) oracle=NO;
		else
			{
			matrix=(char **)allocate_matrix(board);
			build_adjacency_matrix(matrix,board);

            oracle=problem_solver(board,matrix,NO,NULL);
			free_matrix(matrix,board);
			}

		anythreat++;
		} while(anythreat<thnum+combo && oracle==NO);

	return oracle;
	}


short evaluation_function(struct board *board)
	{
	switch(board->turn)
		{
		case WHITE:
			return evaluate_black(board);

		case BLACK:
			return evaluate_white(board);
		}

	return NOCOMMENT;
	}

// *************************************************************************

// Single step evaluation to file

short ss_evaluate_black(struct board *board)
	{
    FILE *h1;
    short key,oracle;
	register x;
	char **matrix;
	short i,*bpos;

    h1=fopen(TMPFILENAME,"w");
    if(!h1) fatal_error("Cannot open temporary file");

    fprintf(h1,"Oracle analysis for red:\n\n");

	board->sp=0;

	board->intgp.j=0;
	board->intgp.k=0;

	memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));

	for(i=0;i<GROUPS;i++)
		{
		if(threat_group(board,i,BLACK))
			{
			board->intgp.tgroups[i]=YES;
			board->intgp.j++;
			}
		else board->intgp.tgroups[i]=NO;

		if(threat_group(board,i,WHITE))
			{
			board->intgp.mygroups[i]=YES;
			board->intgp.k++;
			}
		else board->intgp.mygroups[i]=NO;
		}

    fprintf(h1,"There are %d groups in which red can connect four men and ",board->intgp.j);
    fprintf(h1,"%d dangerous\ngroups in which yellow can connect four men.\n",board->intgp.k);
    fprintf(h1,"\n");

	claimeven(board);
	baseinverse(board);
	vertical(board);
	aftereven(board);
	lowinverse(board);
	highinverse(board);
	baseclaim(board);
	before(board);

    if(board->intgp.j==0)
        {
        fprintf(h1,"This implies that at least a draw is guaranteed.\n");

        oracle=YES;
        goto BSS_ENDING;
        }

    if(board->sp==0)
        {
        fprintf(h1,"No rules can be applied to solve the aforementioned groups in which\n");
        fprintf(h1,"yellow can connect four men. Therefore nothing can be said about red.\n");

        oracle=NO;
        goto BSS_ENDING;
        }

    fprintf(h1,"Total number of rule instances is: %d\n\n",board->sp);

	matrix=(char **)allocate_matrix(board);
	build_adjacency_matrix(matrix,board);

    oracle=problem_solver(board,matrix,YES,h1);
	free_matrix(matrix,board);

    BSS_ENDING:

    fprintf(h1,"\n");
    fclose(h1);
    return oracle;
	}


short ss_evaluate_white(struct board *board)
	{
    FILE *h1;
    short x,y;
	struct threat_combo tc[GROUPS];
	short threats[MAXGROUPS],thnum=0,combo,ccmb;
	short key,oracle,anythreat=0;
	char **matrix;
	short i,*bpos,px,py;

    h1=fopen(TMPFILENAME,"w");
    if(!h1) fatal_error("Cannot open temporary file");

    fprintf(h1,"Oracle analysis for yellow:\n\n");

    combo=threat_combo(board,tc);
	thnum=count_odd_threats(board,threats);
    if(thnum+combo==0)
        {
        fprintf(h1,"There are no single or double threats for yellow\n");
        fprintf(h1,"therefore nothing can be said about yellow possibilities\n");

        oracle=NO;
        goto WSS_ENDING;
        }

    fprintf(h1,"There are %d odd threats and %d double threats for yellow\n\n",
            thnum,combo);

    do
		{
        for(x=0;x<75;x++)
            fputc('*',h1);

        fprintf(h1,"\n\nCase #%d: ",anythreat+1);

        memset(board->usablegroup,0xff,GROUPS*sizeof(short));
		memset(board->sqused,0xff,(BOARDX+1)*(BOARDY+2)*sizeof(short));
		memset(board->wipesq,0x00,(BOARDX+1)*(BOARDY+2)*sizeof(short));

		if(anythreat<thnum)
			{
            fprintf(h1,"odd threat at %c%d.\n\n",
                    ELX(threats[anythreat])+'a',ELY(threats[anythreat])+1);

			wipe_above(board,threats[anythreat]);
			wipe_odd(board,threats[anythreat]);

			px=ELX(threats[anythreat]);
			for(py=0;py<BOARDY;py++)
                if(board->square[ELM(px,py)]==EMPTY)
                    board->sqused[ELM(px,py)]=NO;
			}
		else
			{
            ccmb=anythreat-thnum;

			px=ELX(tc[ccmb].cross);
			x =ELX(tc[ccmb].odd);

            fprintf(h1,"double threat at %c%d.\n\n",
                    ELX(tc[ccmb].cross)+'a',ELY(tc[ccmb].cross)+1);

			for(y=0;y<BOARDY;y++)
				{
				if(board->square[ELM(px,y)]==EMPTY)
					board->sqused[ELM(px,y)]=NO;

				if(board->square[ELM( x,y)]==EMPTY)
					board->sqused[ELM( x,y)]=NO;
				}

			if(ELY(tc[ccmb].even)>ELY(tc[ccmb].odd))
				handle_even_above_odd(board,&tc[ccmb]);
			else
				handle_odd_above_even(board,&tc[ccmb]);
			}

		board->sp=0;
		board->intgp.j=0;
		board->intgp.k=0;

        fprintf(h1,"Eliminating squares: ");

        for(x=0;x<BOARDX;x++)
            for(y=0;y<BOARDY;y++)
                if(!board->sqused[ELM(x,y)])
                    fprintf(h1,"%c%d ",x+'a',y+1);

        fprintf(h1,"\n\n");

		for(i=0;i<GROUPS;i++)
			{
			if(threat_group(board,i,WHITE) &&
				!wiped_group(board,i) && board->usablegroup[i])
				{
				board->intgp.tgroups[i]=YES;
				board->intgp.j++;
				}
			else board->intgp.tgroups[i]=NO;

			if(threat_group(board,i,BLACK))
				{
				board->intgp.mygroups[i]=YES;
				board->intgp.k++;
			}
			else board->intgp.mygroups[i]=NO;
			}

        for(y=0;y<BOARDY;y++)
            for(x=0;x<BOARDX;x++)
                if(board->square[ELM(x,y)]!=EMPTY &&
                   board->sqused[ELM(x,y)]==NO)
                      fatal_error("Marked as used an useless square...");

        fprintf(h1,"There are %d group(s) in which yellow can connect four men and ",board->intgp.j);
        fprintf(h1,"%d dangerous\ngroup(s) in which red can connect four men.\n",board->intgp.k);
        fprintf(h1,"\n");

        claimeven(board);
		baseinverse(board);
		vertical(board);
		aftereven(board);
		lowinverse(board);
		highinverse(board);
		baseclaim(board);
		before(board);

        if(board->intgp.j==0)
            {
            fprintf(h1,"This implies that a win is guaranteed.\n");
            oracle=YES;
            }

        else if(board->sp==0)
            {
            fprintf(h1,"No rules can be applied to solve the aforementioned groups in which\n");
            fprintf(h1,"red can connect four men. Therefore nothing can be said about yellow.\n");
            oracle=NO;
            }

        else
			{
            fprintf(h1,"Total number of rule instances is: %d\n\n",board->sp);

            matrix=(char **)allocate_matrix(board);
			build_adjacency_matrix(matrix,board);

            oracle=problem_solver(board,matrix,YES,h1);
			free_matrix(matrix,board);
			}

        fprintf(h1,"\n");

        anythreat++;
		} while(anythreat<thnum+combo && oracle==NO);

    if(oracle==YES && anythreat<thnum+combo)
        {
        fprintf(h1,"No further cases are taken into consideration since\n");
        fprintf(h1,"this last case is sufficient to prove yellow can win\n");
        fprintf(h1,"from this position.\n");
        }

    WSS_ENDING:

    fprintf(h1,"\n");
    fclose(h1);
    return oracle;
	}

short ss_evaluation_function(struct board *board)
	{
	switch(board->turn)
		{
		case WHITE:
            return ss_evaluate_black(board);

		case BLACK:
            return ss_evaluate_white(board);
		}

	return NOCOMMENT;
	}
