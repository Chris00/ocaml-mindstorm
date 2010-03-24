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

#include "connect4.h"
#include "con4vals.h"
#include "rules.h"
#include "pnsearch.h"
#include "proto.h"

#define AUTO_BOOK "autobook.cn4"

#define SCREENSAVER 10

#define DATABASEFILE    "openbook.cn4"
#define DATABASELOGFILE "openbook.log"

#define BADDATABASEFILE "openbook.bad"
#define NEWDATABASEFILE "openbook.new"

short nodeseq[]={ 3,2,4,5,1,0,6 };  // This is the sequence we follow to
                                    // scan the seven moves available

unsigned short *myscreen,saveflag;

char *tr[]={"None","White (O)","Black (X)"};

long max_nodes_numb,node_expanded,node_not_expanded;
long nodes_added;
short timepass,noscreensaver=NO;
short best_keep;
short manual_steer;

long my_t_sequence;

struct board *auxboard,*tempboard;
struct node *root;
struct bintree *treeroot;
struct dbtree *dbroot;

struct parameters parameters;

short rootsym,currsym;

void steering(short steer)
    {
    manual_steer=steer;
    }

short compatible_sym(unsigned char *p1,unsigned char *p2)
    {
    short x;

    for(x=0;x<BOARDX && p1[x]!=EMPTY && p2[x]!=EMPTY;x++)
        if(p1[x]!=p2[x]) return YES;

    return NO;
    }

void show_pinfo()
    {
    short a;
    char *adv[]={ "Adv White","Adv Black","Deuce" };
    unsigned long tm;
    short x;
    short hh,mm,ss;
    char st[80],seqs[10];

    if(manual_steer) strcpy(seqs,"None");
    else sprintf(seqs,"%-4ld",my_t_sequence);

    if(root->proof==root->disproof) a=2;
    else
        {
        if(root->turn==WHITE)
            {
            if(root->proof<root->disproof) a=0;
            else a=1;
            }
        else
            {
            if(root->proof<root->disproof) a=1;
            else a=0;
            }
        }

    memset(st,0x20,80);

    parameters.maxnodes=max_nodes_numb;
    tm=parameters.end-parameters.start;

    hh=(short)(tm/3600);
    mm=(short)((tm/60)%60);
    ss=(short)(tm%60);

    if(parameters.pr>0 && parameters.ds>0)
        {
        sprintf(st," Nodes: %5ld/%-5ld   P=%-3d   D=%-3d   Time: %2d:%02d:%02d   SEQ=%s   %s",
                parameters.nodes,parameters.maxnodes,parameters.pr,parameters.ds,hh,mm,ss,
                seqs,adv[a]);
        }
    else if(parameters.pr==0)
        {
        sprintf(st," Nodes: %5ld/%-5ld   P=%-3c   D=%-3c   Time: %2d:%02d:%02d   SEQ=%s   %s",
                parameters.nodes,parameters.maxnodes,'í','ì',hh,mm,ss,
                seqs,adv[a]);
        }
    else if(parameters.ds==0)
        {
        sprintf(st," Nodes: %5ld/%-5ld   P=%-3c   D=%-3c   Time: %2d:%02d:%02d   SEQ=%s   %s",
                parameters.nodes,parameters.maxnodes,'ì','í',hh,mm,ss,
                seqs,adv[a]);
        }

    for(x=0;x<78;x++)
        myscreen[x]=0x1e00+st[x];

    if(noscreensaver) myscreen[78]=0x1e00+'S';
    else myscreen[78]=0x1e00+' ';
    }

/* ------------------------------------------------------------------- */

void change_max_nodes_numb(void)
    {
    long nodes;

    printf("\n");
    printf("Enter new number of nodes (old is %ld) : ",max_nodes_numb);
    scanf("%ld",&nodes);

    if(nodes>0L && nodes<1000000L)
        {
        max_nodes_numb=nodes;
        printf("%ld nodes accepted (%ld bytes)\n\n",
            max_nodes_numb,max_nodes_numb*sizeof(struct node));
        }
    else printf("Number refused...           \n\n");
    }

void change_sequence()
	{
	short seq[BOARDX],done[BOARDX],x,valid=YES;

	for(x=0;x<BOARDX;x++)
		done[x]=NO;

	printf("\n");
	printf("Enter new searching sequence\n");

	scanf("%d%d%d%d%d%d%d",&seq[0],&seq[1],&seq[2],&seq[3],
								  &seq[4],&seq[5],&seq[6]);

	for(x=0;x<BOARDX && valid;x++)
		{
		if(seq[x]<1 || seq[x]>BOARDX) valid=NO;
		else done[seq[x]-1]=YES;
		}

	for(x=0;x<BOARDX && valid;x++)
		if(!done[x]) valid=NO;

	if(valid)
		{
		for(x=0;x<BOARDX;x++)
			nodeseq[x]=seq[x]-1;

		printf("Sequence accepted!\n\n");
		}
	else printf("Sequence refused!\n\n");
	}

void dump_pnboard(struct node *mynode)
	{
	short x,y,cn,x1;

	printf("\n");
	printf("Root status position:\n\n");

	for(y=0;y<BOARDY;y++)
		{
		for(x=0;x<BOARDX;x++)
			{
			if(!rootsym) x1=x;
			else x1=BOARDX-1-x;

			cn=mynode->square[ELM(x1,BOARDY-1-y)];
			if(cn==EMPTY) printf(".");
			else if(cn==WHITE) printf("O");
			else if(cn==BLACK) printf("X");
			else fatal_error("#@!=?");
			}
		printf("\n");
		}

	printf("\n");
	printf("%s is to move\n\n",tr[mynode->turn]);
	}

void generate_all_children(struct node *node)
	{
	struct node *dummy,*symm;
	short x,y,x1,y1,backtrace;

	for(x=0;x<BOARDX;x++)
		{
		if(node->child[x]) fatal_error("Tried to allocate a node twice!");
		else if(node->stack[x]<BOARDY)
			{
			node->child[x]=(struct node *)malloc(sizeof(struct node));
			symm=(struct node *)malloc(sizeof(struct node));

			if(!node->child[x] || !symm)
				fatal_error("Not enough memory to build the tree");

			memcpy(node->child[x]->square,node->square,(BOARDX+1)*(BOARDY+2));
			memcpy(node->child[x]->stack,node->stack,BOARDX+1);
			backtrace=x;

			node->child[x]->square[ELM(x,node->child[x]->stack[x]++)]=node->turn;
			node->child[x]->turn=node->turn^SWITCHSIDE;
			symm->turn=node->turn^SWITCHSIDE;

			for(y1=0;y1<BOARDY;y1++)
				{
				symm->square[ELM(BOARDX,y1)]=node->child[x]->square[ELM(BOARDX,y1)];
				for(x1=0;x1<BOARDX;x1++)
					symm->square[ELM(x1,y1)]=node->child[x]->square[ELM(BOARDX-x1-1,y1)];
				}

			symm->stack[BOARDX]=node->child[x]->stack[BOARDX];
			for(x1=0;x1<BOARDX;x1++)
				symm->stack[x1]=node->child[x]->stack[BOARDX-x1-1];

			if(bin_compare(symm->square,node->child[x]->square)>0)
				{
				dummy=symm;
				symm=node->child[x];
				node->child[x]=dummy;
				backtrace=BOARDX-x-1;
				}

			dummy=check_node(treeroot,node->child[x],0);
			if(dummy)
				{
				free(node->child[x]);
				node->child[x]=dummy;
				node->child[x]->parent[backtrace]=node;
				node_not_expanded++;
				}
			else
				{
				node->child[x]->expanded=NO;
				node->child[x]->evaluated=NO;
                node->child[x]->direct=YES;

				for(y=0;y<BOARDX;y++)
					node->child[x]->parent[y]=NULL;

				node->child[x]->parent[backtrace]=node;

				for(y=0;y<BOARDX;y++)
					node->child[x]->child[y]=NULL;

				node_expanded++;

				if(node->type==AND_TYPE) node->child[x]->type=OR_TYPE;
				else if(node->type==OR_TYPE) node->child[x]->type=AND_TYPE;
				else fatal_error("Could not recognize node type!");

				/*add_node(treeroot,node->child[x]);*/
				}

			node->symmetric[x]=backtrace;
			free(symm);
			}

		else node->child[x]=NULL;
		}
	}

short think_function(struct board *board)
	{
	return -evaluation_function(board);
	}

void evaluate(struct node *node)
	{
	short x,oracle,value,flag=YES;

	if(node->evaluated)
		{
		printf("!");
		return;
		}

	value=test_dbtree_position(dbroot,node->square);

	if(value!=0)
		{
		flag=NO;
		if(value==PROVED)
			{
			if(root->turn==WHITE) node->value=PROVED;
			else node->value=DISPROVED;
			}
		else
			{
			if(root->turn==WHITE) node->value=DISPROVED;
			else node->value=PROVED;
			}

		node->evaluated=YES;
		goto BYE_BYE;
		}

    myscreen[79]=0x2b20;
	if(heuristic_search(node,2800L)) goto BYE_BYE;

    myscreen[79]=0x4b20;

    for(x=0;x<64;x++)
        auxboard->square[x]=(short)node->square[x];

	auxboard->turn=node->turn;
	auxboard->filled=0;

	for(x=0;x<BOARDX+1;x++)
		{
		auxboard->stack[x]=(short)node->stack[x];
		if(x<BOARDX) auxboard->filled+=auxboard->stack[x];
		}

    if(auxboard->filled<MAXMEN) oracle=think_function(auxboard);
    else fatal_error("I thought I never would have come here...");

	if(oracle==NO) node->value=UNKNOWN;
	else if(oracle==-1)
		{
        if(node->type==OR_TYPE) node->value=DISPROVED;
		else node->value=PROVED;
		}
	else if(oracle==YES)
		{
		if(node->type==OR_TYPE) node->value=PROVED;
		else node->value=DISPROVED;
		}
	else fatal_error("Oracle got crazy!");

	BYE_BYE:
	if(node->value==UNKNOWN) printf("?");
	else if(node->value==PROVED)
		{
		if(flag) printf("+");
		else printf("*");
		}
	else
		{
		if(flag) printf("-");
		else printf("=");
		}

    myscreen[79]=0x1b20;
    }

void free_whole_tree(struct bintree *tree)
	{
	short x;

	if(!tree) return;
	if(tree->lson) free_whole_tree(tree->lson);
	if(tree->rson) free_whole_tree(tree->rson);

	free(tree->node);
	}

/* ------------------------------------------------------------------- */

short resources_available()
	{
	short key;

	key=readkey();

    if(key==0 && timepass<SCREENSAVER)
        {
        timepass++;
        if(timepass==SCREENSAVER && !noscreensaver) screen_off();
        }
    else if(key!=0)
        {
        timepass=0;
        screen_on();
        }

    switch(key)
		{
        case 'h':
            printf("\n");
            printf("Keys available:\n");
            printf("ESC - abort search\n");
            printf(" p  - pause search\n");
            printf(" d  - dump start board status\n");
            printf(" c  - change search sequence\n");
            printf(" n  - change max. nodes number\n");
            printf(" o  - turn on screen saver immediately\n");
            printf(" h  - show this help screen\n");
            printf(" s  - toggle screen saver on/off\n");
            printf("\n");
            break;

        case 0x1b:
			printf("\n");
			printf("Interrupted by user!\n");
			return NO;

		case 'p':
			while(readkey()==0);
			break;

		case 'd':
			dump_pnboard(root);
			break;

		case 'c':
			change_sequence();
			break;

        case 'n':
            change_max_nodes_numb();
            break;

        case 'o':
            screen_off();
            break;

        case 's':
            noscreensaver^=1;
            printf("\n");
            if(noscreensaver) printf("Screen saver is turned off\n");
            else printf("Screen saver is turned on\n");
            printf("\n");
            break;
        }

    printf(" Nod. %6ld %4ld %4ld> ",node_expanded,root->proof,root->disproof);
	if(node_expanded>max_nodes_numb) return NO;
	return YES;
	}

struct node *select_most_proving_node(struct node *node)
	{
	short i,flag,depth=0;

    currsym=rootsym;

	while(node->expanded)
		{
		i=0;
		flag=FALSE;

		switch(node->type)
			{
			case OR_TYPE:
				do
					{
					if(node->child[nodeseq[i]] &&
						node->child[nodeseq[i]]->proof==node->proof)
							flag=TRUE;
					else i++;
					} while(i<BOARDX && !flag);

				if(!flag)
					fatal_error("Could not find a good OR child!");
				break;

			case AND_TYPE:
				do
					{
					if(node->child[nodeseq[i]] &&
                         node->child[nodeseq[i]]->disproof==node->disproof)
							flag=TRUE;
					else i++;
					} while(i<BOARDX && !flag);

				if(!flag)
					fatal_error("Could not find a good AND child!");
				break;

			default:
				fatal_error("Unknown node type!");
				break;

			}
		if(flag)
			{
            short jk;

            depth++;
            jk=nodeseq[i]+1;
            if(currsym) jk=(8-jk);
            printf("%d",jk);

            if(depth==1) best_keep=jk;

            if(node->child[nodeseq[i]]->parent[nodeseq[i]]!=node)
                currsym^=1;

            node=node->child[nodeseq[i]];
			}
		else fatal_error("Null node found!");
		}

	while(depth++<32)
		printf(" ");

	printf("  ");

	if(node->type==OR_TYPE) printf("Or : ");
	else if(node->type==AND_TYPE) printf("And: ");
	else fatal_error("Yes, there's a bug...");

	return node;
	}

void set_pdv_according_to_children(struct node *node)
	{
	short x,children=0;

	for(x=0;x<BOARDX;x++)
		if(node->stack[x]<BOARDY) children++;

	if(children==0)
		fatal_error("Father without children but of value unknown");

	switch(node->type)
		{
		case AND_TYPE:
			node->proof=children;
			node->disproof=1;
			break;

		case OR_TYPE:
			node->proof=1;
			node->disproof=children;
			break;
		}
	}

void set_proof_and_disproof_numbers(struct node *node)
	{
	short x;

	if(!node) fatal_error("Invalid node choosen");
	else if(node->expanded)
		{
		switch(node->type)
			{
			case AND_TYPE:
				node->proof=0;
				node->disproof=MAXVALUE;
				for(x=0;x<BOARDX;x++)
					if(node->child[x])
						{
						node->proof+=node->child[x]->proof;
						node->disproof=min(node->disproof,node->child[x]->disproof);
						}
                if(node->disproof==0)
                    {
                    if(node->direct) nodes_added++;
                    node->direct=NO;
                    node->proof=MAXVALUE;
                    node->value=DISPROVED;
                    }
                if(node->proof==0)
                    {
                    if(node->direct) nodes_added++;
                    node->direct=NO;
                    node->value=PROVED;
                    }
                break;

			case OR_TYPE:
				node->proof=MAXVALUE;
				node->disproof=0;
				for(x=0;x<BOARDX;x++)
					if(node->child[x])
						{
						node->proof=min(node->proof,node->child[x]->proof);
						node->disproof+=node->child[x]->disproof;
						}
                if(node->proof==0)
                    {
                    if(node->direct) nodes_added++;
                    node->direct=NO;
                    node->disproof=MAXVALUE;
                    node->value=PROVED;
                    }
                if(node->disproof==0)
                    {
                    if(node->direct) nodes_added++;
                    node->direct=NO;
                    node->value=DISPROVED;
                    }

				break;

			default:
				fatal_error("I don't know what to prove or disprove!");
				break;
			}

		if(node->proof<0 || node->disproof<0)
			fatal_error("Proof and disproof numbers cannot be lower than zero");

		}
	else if(node->evaluated)
		{
		switch(node->value)
			{
			case PROVED:
				node->proof=0;
				node->disproof=MAXVALUE;
				break;

			case DISPROVED:
				node->proof=MAXVALUE;
				node->disproof=0;
				break;

			case UNKNOWN:
				set_pdv_according_to_children(node);
				break;
			}
		}
	else fatal_error("Asked to set a node never evaluated!");
	}

void develop_node(struct node *node)
	{
	short i;

	node->expanded=YES;

    generate_all_children(node);

    if(!currsym)
        {
        for(i=0;i<BOARDX;i++)
            {
            if(node->child[i])
                {
                evaluate(node->child[i]);
                set_proof_and_disproof_numbers(node->child[i]);
                }
            }
        }
    else
        {
        for(i=BOARDX-1;i>=0;i--)
            {
            if(node->child[i])
                {
                evaluate(node->child[i]);
                set_proof_and_disproof_numbers(node->child[i]);
                }
            }
        }
    }

void update_ancestors(struct node *node)
	{
	struct node *previous_node;
	short x;

	if(node)
		{
		set_proof_and_disproof_numbers(node);

		for(x=0;x<BOARDX;x++)
			if(node->parent[x]) update_ancestors(node->parent[x]);
		}
	}

/*
struct node *update_ancestors(struct node *node)
	{
	struct node *previous_node;
	short old_proof,old_disproof;
	short changed=YES;

	previous_node=NULL;
	while(node && changed)
		{
		old_proof=node->proof;
		old_disproof=node->disproof;
		set_proof_and_disproof_numbers(node);
		changed=((old_proof!=node->proof)||(old_disproof!=node->disproof));
		previous_node=node;
		node=node->parent[0];
		}

	return previous_node;
	}
*/

void pn_search(struct node *root)
	{
	struct node *most_proving_node;

    saveflag=NO;

    best_keep=0;
    printf("\n");
    printf(" Root evaluation: ");
	evaluate(root);
	set_proof_and_disproof_numbers(root);
    printf("\n");

    while(root->proof!=0 && root->disproof!=0 && resources_available())
		{
        saveflag=YES;
        parameters.nodes=node_expanded;
        parameters.end=time(NULL);
        parameters.pr=root->proof;
        parameters.ds=root->disproof;
        parameters.nodes_added=nodes_added;
        show_pinfo();

        most_proving_node=select_most_proving_node(root);
		develop_node(most_proving_node);
		update_ancestors(most_proving_node);
		printf("\n");
        }

    if(root->proof==0) root->value=PROVED;
	else if (root->disproof==0) root->value=DISPROVED;
	else root->value=UNKNOWN;

	dump_pnboard(root);
    }

/* ------------------------------------------------------------------- */

short start_pn_search(struct board *board)
	{
    char output[200],temps[200];
    short seq[42],seqlen,retval;
    unsigned long tm;
    short hh,mm,ss;
	unsigned char tb[(BOARDX+1)*(BOARDY+2)],ts[BOARDX+1];
    short x,x1,y1,key,cancel_sym;
    FILE *logfile;

    for(x=0;x<board->filled;x++)
        seq[x]=board->moves[x]+1;

    seqlen=board->filled;

    myscreen=(unsigned short *)0xb8000;
    parameters.start=time(NULL);
    timepass=0;

	auxboard=board;
	tempboard=(struct board *)malloc(sizeof(struct board));
	if(!tempboard) fatal_error("Cannot store temporary board");
	memcpy(tempboard,board,sizeof(struct board));

	clrscr();

	max_nodes_numb=MAXNODESNUMB;

    printf("\n\nConnect Four Search Engine %s. ",SEARCH_ENGINE_VERSION);
    printf("Last revision %s\n",__DATE__);
    printf("Now building nodes... (each node takes %d bytes of memory)\n",
			 sizeof(struct node));

    printf("Max nodes numb = %ld  (%ld bytes) ",
			  max_nodes_numb,max_nodes_numb*sizeof(struct node));

	node_expanded=0L;
	node_not_expanded=0L;

	root=(struct node *)malloc(sizeof(struct node));
	if(!root) fatal_error("Not enough memory to begin the search");

	for(x=0;x<(BOARDX+1)*(BOARDY+2);x++)
		root->square[x]=(char)(board->square[x]&0xff);

	for(x=0;x<BOARDX+1;x++)
		root->stack[x]=(char)(board->stack[x]&0xff);

	for(y1=0;y1<BOARDY;y1++)
		{
		tb[ELM(BOARDX,y1)]=root->square[ELM(BOARDX,y1)];
		for(x1=0;x1<BOARDX;x1++)
			tb[ELM(x1,y1)]=root->square[ELM(BOARDX-x1-1,y1)];
		}

	ts[BOARDX]=root->stack[BOARDX];
	for(x1=0;x1<BOARDX;x1++)
		ts[x1]=root->stack[BOARDX-x1-1];

    cancel_sym=compatible_sym(tb,root->square);
    if(cancel_sym) printf(":-)))\n");
    else printf(":-(((\n");

	if(bin_compare(tb,root->square)>0)
		{
		rootsym=YES;
		memcpy(root->square,tb,48);
		memcpy(root->stack,ts,8);
		}
	else rootsym=NO;

	root->turn=board->turn;
	root->type=OR_TYPE;

	root->evaluated=NO;
	root->expanded=NO;
	root->value=UNKNOWN;
    root->direct=YES;

	for(x=0;x<BOARDX;x++)
		root->parent[x]=NULL;

	for(x=0;x<BOARDX;x++)
		root->child[x]=NULL;

	treeroot=init_bin_tree(root);
    dbroot=init_dbase(root->square,cancel_sym);

    nodes_added=0L;
	pn_search(root);
    retval=root->value;

	screen_on();

	printf("\n\n");
	printf("Search terminated, value found : ");
    sprintf(output,"; vl: ");

	switch(root->value)
		{
		case UNKNOWN:
            strcat(output,"unknown; ");
            printf("unknown\n");

            sprintf(temps,"BK = %d; ",best_keep);
            strcat(output,temps);

            printf("Best Keep = %d\n",best_keep);
            break;

		case PROVED:
            strcat(output,"proved; ");
			printf("proved\n");
			break;

		case DISPROVED:
            strcat(output,"disproved; ");
			printf("disproved\n");
			break;
		}

    if(manual_steer)
        {
        printf("\007\n");
        printf("Do you want to update dbase file ? (Y/N)\n");
        }

    parameters.nodes=node_expanded;
    parameters.end=time(NULL);
    parameters.pr=root->proof;
    parameters.ds=root->disproof;
    parameters.nodes_added=nodes_added;
    show_pinfo();

    sprintf(temps,"ndu=%ld/%ld; nda=%ld; ",
        node_expanded,max_nodes_numb,nodes_added);

    strcat(output,temps);

    tm=parameters.end-parameters.start;

    hh=(short)(tm/3600);
    mm=(short)((tm/60)%60);
    ss=(short)(tm%60);

    sprintf(temps,"time %2d:%02d:%02d\n",hh,mm,ss);
    strcat(output,temps);

    if(manual_steer)
        do  {
            key=readkey();
            } while(key!='y' && key!='n');

    else
        {
        sleep(2);
        key='n';
        if(root->value==PROVED || root->value==DISPROVED) key='y';
        if(!saveflag) key='n';
        }

    if(key=='y')
        {
        if(manual_steer)
            {
            logfile=fopen(DATABASELOGFILE,"a+");
            fprintf(logfile,"Sequence searched : ");

            for(x=0;x<seqlen;x++)
                fprintf(logfile,"%d",seq[x]);

            fprintf(logfile,"\n");
            fprintf(logfile,"%s\n",output);
            fclose(logfile);
            }

        copy_dbtree();      /* Backup data base file before updating... */
		printf("Updating dbtree\n");
		update_dbtree(treeroot,dbroot,root->turn);
		flush_dbtree(dbroot);
		}
	else printf("Skipping dbtree update...\n\n");

	printf("Freeing memory...\n");
	free_dbtree(dbroot);

	free_whole_tree(treeroot);
	free_bin_tree(treeroot);

	memcpy(board,tempboard,sizeof(struct board));
	free(tempboard);

	if(key=='y') shuffle();

    erase_temp_file();

	printf("Waiting for a key...\n");
    show_pinfo();

    if(manual_steer) while(readkey()==0);
    return retval;
    }

void output_sequence(FILE *h2,char *seq,short seqlen,short flag)
    {
    char bd[64];
    short stack[7],x,y,z,size=WHITE;

    memset(bd,EMPTY,64);
    memset(stack,0,14);

    for(z=0;z<seqlen;z++)
        {
        x=seq[z];
        printf("%d",x+1);

        if(stack[x]<BOARDY)
            {
            bd[ELM(x,stack[x])]=size;
            stack[x]++;
            size^=SWITCHSIDE;
            }
        else fatal_error("Invalid sequence found");
        }

    printf("\n");

    bd[63]=(char)flag;
    fwrite(bd,1,64,h2);
    }

void build_associated_file()
    {
    FILE *h1,*h2;
    char filein[255],seq[50];
    short seqlen,state=0,byte;
    short seqnumb=1,seqthreshold=0;
    unsigned short key;

    printf("Input file to parse : ");
    gets(filein);

    printf("Start from sequence #1 ? (Y/N)\n");
    do
        {
        key=readkey();
        if(key=='n' || key=='y') break;
        } while(1);

    if(key=='n')
        {
        printf("\nEnter sequence number to start from : ");
        scanf("%d",&seqthreshold);
        }

    h1=fopen(filein,"rb");
    h2=fopen(AUTO_BOOK,"wb");

    if(!h1 || !h2) fatal_error("File not found");

    while((byte=getc(h1))!=EOF)
        {
        switch(state)
            {
            case 0:
                if(byte==';') state=1;
                else if(byte=='S') state=2;
                break;

            case 1:
                if(byte==0x0d) state=0;
                break;

            case 2:
                if(byte==0x0d)
                    {
                    if(seqlen>0)
                        output_sequence(h2,seq,seqlen,seqnumb<seqthreshold);
                    seqlen=0;
                    state=0;
                    seqnumb++;
                    }
                else if(byte>='1' && byte<='7' && seqlen<MAXMEN)
                    seq[seqlen++]=byte-'1';

                break;
            }
        }

    fclose(h2);
    fclose(h1);

    printf("Work done, press any key!\n");
    while(readkey()==0);
    }

/* ======================================================================= */

void start_auto_pn_search(struct board *board)
    {
    FILE *h1;
    unsigned char *autobook;
    long len,size,cnt;
    short byte,value,x,y,cn,flag=0;
    struct board *tempboard;

    clrscr();

    tempboard=(struct board *)malloc(sizeof(struct board));
    if(!tempboard) fatal_error("Memory error!");

    memcpy(tempboard,board,sizeof(struct board));

    h1=fopen(AUTO_BOOK,"rb");
    if(!h1) fatal_error("Cannot find autobook");

    size=fileln(h1);
    autobook=(unsigned char *)malloc(size);
    if(!autobook) fatal_error("Memory error");

    len=0;
    while((byte=getc(h1))!=EOF)
        autobook[len++]=byte;

    fclose(h1);

    len=0;

    while(flag==0 && len<size)
        {
        printf("Sequence #%ld\n",(len/64)+1);

        if(autobook[len+63]==1) goto SKIP_ENDING;

        board->filled=0;

        for(y=0;y<BOARDY;y++)
            for(x=0;x<BOARDX;x++)
                {
                cn=autobook[len+ELM(x,y)];
                board->square[ELM(x,y)]=cn;
                if(cn!=EMPTY) board->filled++;
                }

        if(board->filled&1) board->turn=BLACK;
        else                board->turn=WHITE;

        for(x=0;x<BOARDX;x++)
            {
            board->stack[x]=0;
            while(board->square[ELM(x,board->stack[x])]!=EMPTY &&
                board->stack[x]<BOARDY)
                        board->stack[x]++;
            }

        sleep(3);
        my_t_sequence=(len/64)+1;
        value=start_pn_search(board);
        printf("\n\n");

        if(value==UNKNOWN) flag=1;
        else if(value==PROVED && board->turn==BLACK) flag=2;
        else if(value==DISPROVED && board->turn==WHITE) flag=3;

        if(flag==0)
            {
            autobook[len+63]=1;
            h1=fopen(AUTO_BOOK,"wb");
            for(cnt=0;cnt<size;cnt++)
                putc(autobook[cnt],h1);
            fclose(h1);
            }

        SKIP_ENDING:
        len+=64;
        }

    if(len<size)
        printf("Work has been either interrupted or computer has found a wrong result\n");

    else printf("Job has been completed, pal. Hope evrything went well this time!\n");

    printf("Press RETURN key to continue\n");
    while(readkey()!=0x0d);

    free(autobook);
    memcpy(board,tempboard,sizeof(struct board));
    free(tempboard);
    }































