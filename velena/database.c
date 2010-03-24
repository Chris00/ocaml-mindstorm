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

#define DATABASEFILE    "openbook.cn4"
#define DATABASEBACKUP  "openbook.bak"
#define SHIPDISKFILE    "tempship.$$$"
#define BUFSIZE         0x4000

struct dbtree *mmnode;

extern unsigned short *myscreen;

unsigned char blk[12],position[64],tpos[64];
long blocks_wr,added_blocks,unused_blocks;

void expand_block(unsigned char *blk,unsigned char *pss)
	{
    pss[ELM(0,0)]=(blk[0+0*2]>>6)&0x03;
    pss[ELM(1,0)]=(blk[0+0*2]>>4)&0x03;
    pss[ELM(2,0)]=(blk[0+0*2]>>2)&0x03;
    pss[ELM(3,0)]=(blk[0+0*2]   )&0x03;
    pss[ELM(4,0)]=(blk[1+0*2]>>6)&0x03;
    pss[ELM(5,0)]=(blk[1+0*2]>>4)&0x03;
    pss[ELM(6,0)]=(blk[1+0*2]>>2)&0x03;
    pss[ELM(7,0)]=0xff;

    pss[ELM(0,1)]=(blk[0+1*2]>>6)&0x03;
    pss[ELM(1,1)]=(blk[0+1*2]>>4)&0x03;
    pss[ELM(2,1)]=(blk[0+1*2]>>2)&0x03;
    pss[ELM(3,1)]=(blk[0+1*2]   )&0x03;
    pss[ELM(4,1)]=(blk[1+1*2]>>6)&0x03;
    pss[ELM(5,1)]=(blk[1+1*2]>>4)&0x03;
    pss[ELM(6,1)]=(blk[1+1*2]>>2)&0x03;
    pss[ELM(7,1)]=0xff;

    pss[ELM(0,2)]=(blk[0+2*2]>>6)&0x03;
    pss[ELM(1,2)]=(blk[0+2*2]>>4)&0x03;
    pss[ELM(2,2)]=(blk[0+2*2]>>2)&0x03;
    pss[ELM(3,2)]=(blk[0+2*2]   )&0x03;
    pss[ELM(4,2)]=(blk[1+2*2]>>6)&0x03;
    pss[ELM(5,2)]=(blk[1+2*2]>>4)&0x03;
    pss[ELM(6,2)]=(blk[1+2*2]>>2)&0x03;
    pss[ELM(7,2)]=0xff;

    pss[ELM(0,3)]=(blk[0+3*2]>>6)&0x03;
    pss[ELM(1,3)]=(blk[0+3*2]>>4)&0x03;
    pss[ELM(2,3)]=(blk[0+3*2]>>2)&0x03;
    pss[ELM(3,3)]=(blk[0+3*2]   )&0x03;
    pss[ELM(4,3)]=(blk[1+3*2]>>6)&0x03;
    pss[ELM(5,3)]=(blk[1+3*2]>>4)&0x03;
    pss[ELM(6,3)]=(blk[1+3*2]>>2)&0x03;
    pss[ELM(7,3)]=0xff;

    pss[ELM(0,4)]=(blk[0+4*2]>>6)&0x03;
    pss[ELM(1,4)]=(blk[0+4*2]>>4)&0x03;
    pss[ELM(2,4)]=(blk[0+4*2]>>2)&0x03;
    pss[ELM(3,4)]=(blk[0+4*2]   )&0x03;
    pss[ELM(4,4)]=(blk[1+4*2]>>6)&0x03;
    pss[ELM(5,4)]=(blk[1+4*2]>>4)&0x03;
    pss[ELM(6,4)]=(blk[1+4*2]>>2)&0x03;
    pss[ELM(7,4)]=0xff;

    pss[ELM(0,5)]=(blk[0+5*2]>>6)&0x03;
    pss[ELM(1,5)]=(blk[0+5*2]>>4)&0x03;
    pss[ELM(2,5)]=(blk[0+5*2]>>2)&0x03;
    pss[ELM(3,5)]=(blk[0+5*2]   )&0x03;
    pss[ELM(4,5)]=(blk[1+5*2]>>6)&0x03;
    pss[ELM(5,5)]=(blk[1+5*2]>>4)&0x03;
    pss[ELM(6,5)]=(blk[1+5*2]>>2)&0x03;
    pss[ELM(7,5)]=0xff;

    memset(&pss[48],0xff,16);
	}

void collapse_position(unsigned char *mypos,unsigned char *blk)
	{
	short y;

	for(y=0;y<BOARDY;y++)
		{
        blk[0+y*2]=((mypos[ELM(0,y)]&0x03)<<6)|
                   ((mypos[ELM(1,y)]&0x03)<<4)|
                   ((mypos[ELM(2,y)]&0x03)<<2)|
                   ((mypos[ELM(3,y)]&0x03)   );

        blk[1+y*2]=((mypos[ELM(4,y)]&0x03)<<6)|
                   ((mypos[ELM(5,y)]&0x03)<<4)|
                   ((mypos[ELM(6,y)]&0x03)<<2)| 0x03;
		}
	}

short unused_position(unsigned char *ds,unsigned char *bd,short cancel_sym)
    {
    short x,y,flag=NO;

    /*return NO;*/

    switch(cancel_sym)
        {
        case NO:
            for(y=0;y<BOARDY && !flag;y++)
                for(x=0;x<BOARDX && !flag;x++)
                    if(bd[ELM(x,y)]!=EMPTY && bd[ELM(x,y)]!=ds[ELM(x,y)] &&
                       bd[ELM(BOARDX-x-1,y)]!=EMPTY && bd[ELM(BOARDX-x-1,y)]!=ds[ELM(x,y)]) flag=YES;
            break;

        case YES:
            for(y=0;y<BOARDY && !flag;y++)
                for(x=0;x<BOARDX && !flag;x++)
                    if(bd[ELM(x,y)]!=EMPTY && bd[ELM(x,y)]!=ds[ELM(x,y)]) flag=YES;
            break;
        }
    return flag;
    }

short test_dbtree_position(struct dbtree *root,unsigned char *mypos)
	{
    short cmp;

    if(!root) return 0;

    expand_block(root->cpos,tpos);

    cmp=bin_compare(tpos,mypos);
    if(cmp<0) return test_dbtree_position(root->lson,mypos);
    else if(cmp>0) return test_dbtree_position(root->rson,mypos);

    return root->value;
    }

void add_dbtree(struct dbtree *root,unsigned char *mypos,short value)
	{
	short cmp;

	if(!root) fatal_error("Tryied to build an unallocated tree!");

    expand_block(root->cpos,tpos);
    cmp=bin_compare(tpos,mypos);
	if(cmp<0)
		{
        if(root->lson) add_dbtree(root->lson,mypos,value);
		else
			{
			root->lson=(struct dbtree *)malloc(sizeof(struct dbtree));
			if(!root->lson) fatal_error("Not enough memory");

			root->lson->parent=root;
			root->lson->lson=NULL;
			root->lson->rson=NULL;
            root->lson->value=value;
            root->lson->stacked=NO;

            collapse_position(mypos,root->lson->cpos);
			}
		}
	else if(cmp>0)
		{
        if(root->rson) add_dbtree(root->rson,mypos,value);
		else
			{
			root->rson=(struct dbtree *)malloc(sizeof(struct dbtree));
			if(!root->rson) fatal_error("Not enough memory");

			root->rson->parent=root;
			root->rson->lson=NULL;
			root->rson->rson=NULL;
            root->rson->value=value;
            root->rson->stacked=NO;

            collapse_position(mypos,root->rson->cpos);
			}
		}

	return;
	}

void my_add_dbtree(struct dbtree *root,unsigned char *mypos,short value,long posit)
	{
	short cmp;

	if(!root) fatal_error("Tryied to build an unallocated tree!");

    expand_block(root->cpos,tpos);

    cmp=bin_compare(tpos,mypos);
    if(cmp<0)
		{
        if(root->lson) my_add_dbtree(root->lson,mypos,value,posit);
		else
			{
            root->lson=(struct dbtree *)&mmnode[posit];
			if(!root->lson) fatal_error("Not enough memory");

			root->lson->parent=root;
			root->lson->lson=NULL;
			root->lson->rson=NULL;
            root->lson->value=value;
            root->lson->stacked=YES;

            collapse_position(mypos,root->lson->cpos);
            }
		}
	else if(cmp>0)
		{
        if(root->rson) my_add_dbtree(root->rson,mypos,value,posit);
		else
			{
            root->rson=(struct dbtree *)&mmnode[posit];
			if(!root->rson) fatal_error("Not enough memory");

			root->rson->parent=root;
			root->rson->lson=NULL;
			root->rson->rson=NULL;
            root->rson->value=value;
            root->rson->stacked=YES;

            collapse_position(mypos,root->rson->cpos);
            }
		}

	return;
	}

struct dbtree *init_dbase(unsigned char *currposit,short cancel_sym)
	{
    FILE *h1,*h2;
	struct dbtree *dbroot;
    long posit,size,tsize,rsize;
    short block,value,x;

	printf("\n");
	printf("Initializing database...\n");

    mmnode=NULL;

	dbroot=(struct dbtree *)malloc(sizeof(struct dbtree));
	if(!dbroot) fatal_error("Not enough memory to handle data base!");

    memset(dbroot->cpos,0xff,12);

    unused_blocks=0L;

	dbroot->parent=NULL;
	dbroot->lson=NULL;
	dbroot->rson=NULL;
	dbroot->value=0x7fff;

	h1=fopen(DATABASEFILE,"rb");
	if(!h1) goto SKIP_FILE_LOADING;

    size=tsize=fileln(h1);
	if((size%14)!=0) fatal_error("Invalid data received!");

    printf("Total number of blocks in data base: %ld (file size: %ld)\n",size/14,size);

    for(x=0;x<79;x++)
        myscreen[x]=0x7e20;

    h2=fopen(SHIPDISKFILE,"wb");
    if(!h2) fatal_error("Cannot write to hard disk!");

    printf("Looking for unused blocks...\n");

    while(size>0)
		{
		fread(blk,1,12,h1);
		value=((short)getc(h1));
		value+=((short)getc(h1)<<8);
		expand_block(blk,position);

        if(unused_position(position,currposit,cancel_sym))
            {
            unused_blocks++;
            fwrite(blk,1,12,h2);
            putc(value&0xff,h2);
            putc(value>>8,h2);
            }

        for(x=0;x<(80L*(tsize-size))/tsize;x++)
            myscreen[x]=0x4e20;

        size-=14;
        }

    fclose(h2);
    printf("%ld unused blocks shipped to disk (file size: %ld)\n",unused_blocks,unused_blocks*14L);

    fseek(h1,0L,SEEK_SET);
    size=tsize;
    rsize=tsize-unused_blocks*14L;

    if(rsize==0)
        {
        fclose(h1);
        goto SKIP_FILE_LOADING;
        }

    mmnode=(struct dbtree *)malloc((rsize/14)*sizeof(struct dbtree));
    if(!mmnode) fatal_error("Cannot allocate the big memory block, darn!\n");

    printf("%ld bytes successfully allocated as big memory block\n",
            (rsize/14)*sizeof(struct dbtree));

    posit=0L;
    while(size>0)
		{
		fread(blk,1,12,h1);
		value=((short)getc(h1));
		value+=((short)getc(h1)<<8);
        expand_block(blk,position);

        if(!unused_position(position,currposit,cancel_sym))
            {
            my_add_dbtree(dbroot,position,value,posit);
            posit++;
            }

		if((posit&0x03ff)==0)
            printf("Loading positions data base from disk... : %ld  (unused: %ld)\r",posit,unused_blocks);

        size-=14;

        for(x=0;x<(80L*(tsize-size))/tsize;x++)
            myscreen[x]=0x1e20;
        }

	fclose(h1);
    printf("Loading positions data base from disk... : %ld  (unused: %ld)\n",posit,unused_blocks);
    printf("\n");

    if(posit+unused_blocks!=tsize/14) fatal_error("Something went wrong!");

    SKIP_FILE_LOADING:
    return dbroot;
    }

void my_free_dbtree(struct dbtree *root)
	{
    if(root->lson) my_free_dbtree(root->lson);
    if(root->rson) my_free_dbtree(root->rson);
    if(!root->stacked) free(root);
	}

void free_dbtree(struct dbtree *root)
    {
    my_free_dbtree(root);
    if(mmnode) free(mmnode);
    }

void flush_tree(struct dbtree *root,FILE *h1)
	{
	if(!root) return;

    if(root->cpos[0]!=0xff)
		{
        fwrite(root->cpos,1,12,h1);
		putc(root->value&0xff,h1);
		putc(root->value>>8,h1);
		blocks_wr++;

		if((blocks_wr&0x03ff)==0)
			printf("Blocks written %ld\r",blocks_wr);
		}

    flush_tree(root->lson,h1);
    flush_tree(root->rson,h1);
	}

void flush_dbtree(struct dbtree *root)
	{
    long bb=0L;
    FILE *h1,*h2;

	blocks_wr=0L;

	h1=fopen(DATABASEFILE,"wb");
	if(!h1) fatal_error("Cannot write file to disk!");

	printf("Writing positions to disk...\n");
	flush_tree(root,h1);
    printf("Blocks written %ld\r",blocks_wr);

    h2=fopen(SHIPDISKFILE,"rb");
    if(h2)
        {
        short byte;
        while((byte=getc(h2))!=EOF)
            {
            putc(byte,h1);
            bb++;
            if(bb%(1024*14)==0)
                printf("Blocks written %ld + %ld = %ld\r",blocks_wr,bb/14,blocks_wr+bb/14);
            }
        fclose(h2);
        printf("Blocks written %ld + %ld = %ld\r",blocks_wr,bb/14,blocks_wr+bb/14);
        }
    fclose(h1);
    printf("\n");
    }

void copy_dbtree()
	{
	long len;
	unsigned char *buffer;
	short block;
	FILE *h1,*h2;

	h1=fopen(DATABASEFILE,"rb");
	if(!h1)
		{
		printf("Creating data base file...\n");
		return;
		}

	h2=fopen(DATABASEBACKUP,"wb");
	if(!h2) fatal_error("Error handling data base files...");

	buffer=(unsigned char *)malloc(BUFSIZE);
	if(!buffer) fatal_error("Cannot allocate temporary buffer!");

	printf("Backing up data base file...\n");
	len=fileln(h1);
	while(len>0)
		{
		block=(short)min(len,BUFSIZE);
		fread(buffer,1,block,h1);
		fwrite(buffer,1,block,h2);
		len-=block;
		}

	free(buffer);
	fclose(h1);
	fclose(h2);
	}

void update_dbtree(struct bintree *treeroot,struct dbtree *dbroot,short side)
	{
	short vl;

	if(treeroot->lson) update_dbtree(treeroot->lson,dbroot,side);
	if(treeroot->rson) update_dbtree(treeroot->rson,dbroot,side);

	if(side==WHITE)
		{
		if(treeroot->node->value==PROVED) vl=PROVED;
		else vl=DISPROVED;
		}
	else
		{
		if(treeroot->node->value==PROVED) vl=DISPROVED;
		else vl=PROVED;
		}

    if(treeroot->node->value!=UNKNOWN && !treeroot->node->direct)
		add_dbtree(dbroot,treeroot->node->square,vl);
	}

/* --------------------------------------------------------------------- */

long myrandom(long maxval)
	{
	return(long)(((double)rand()*maxval)/32768.0);
	}

void shuffle(void)
	{
	long size,nodes,pv,aq;
        unsigned char *p,temp[14];
	FILE *h1;

	h1=fopen(DATABASEFILE,"rb");
	if(!h1) fatal_error("Could not find opening book");

	size=fileln(h1);
	nodes=size/14;

        printf("Memory required: %ld bytes\n",14L*nodes*sizeof(unsigned char));
        p=(unsigned char *)malloc(14L*nodes*sizeof(unsigned char));
	if(!p) fatal_error("Not enough memory !");

	printf("File size = %ld bytes\n",size);
	printf("%ld nodes detected...\n",nodes);
	printf("Loading...\n");

	for(pv=0L;pv<nodes;pv++)
                fread(&p[14L*pv],1,14,h1);

	fclose(h1);

	h1=fopen(DATABASEFILE,"wb");
	if(!h1) fatal_error("Cannot write shuffled opening book");

    printf("Now shuffling and rewriting nodes...\n");
	while(pv>0)
		{
		aq=myrandom(pv);

                memcpy(temp,&p[14L*aq],14);
                memcpy(&p[14L*aq],&p[14L*(pv-1)],14);
                memcpy(&p[14L*(pv-1)],temp,14);

                fwrite(&p[14L*(pv-1)],1,14,h1);
		pv--;
		}

    fclose(h1);
    free(p);

    printf("Done!\n\n");
    }

/* ----------------------------------------------------------------- */

void free_small_tree(struct small_tree *root)
	{
	if(root->lson) free_small_tree(root->lson);
	if(root->rson) free_small_tree(root->rson);
	free(root);
	}

void flush_smtree(struct small_tree *root,FILE *h1)
	{
	if(!root) return;

	if(root->buffer[0]!=0xff)
		{
		fwrite(root->buffer,1,14,h1);
		blocks_wr++;
		if((blocks_wr&0x03ff)==0)
			printf("Blocks written %ld\r",blocks_wr);
		}

	if(my_random(256)&1)
		{
		flush_smtree(root->lson,h1);
		flush_smtree(root->rson,h1);
		}
	else
		{
		flush_smtree(root->rson,h1);
		flush_smtree(root->lson,h1);
		}
	}

void flush_small_tree(struct small_tree *root)
	{
	FILE *h1;

	blocks_wr=0L;

	h1=fopen(DATABASEFILE,"wb");
	if(!h1) fatal_error("Cannot write file to disk!");

	printf("Writing positions to disk...\n");
	flush_smtree(root,h1);
	printf("Blocks written %ld\r",blocks_wr);
	fclose(h1);
	printf("\n\n");
	}

short compare_small_tree(unsigned char *b1,unsigned char *b2)
	{
	short x;

	for(x=0;x<14;x++)
		{
		if(b1[x]<b2[x]) return -1;
		if(b2[x]<b1[x]) return  1;
		}

	return 0;
	}

struct small_tree *check_and_add(struct small_tree *root,unsigned char *buffer)
	{
	if(!root)
		{
		added_blocks++;

		if((added_blocks&0x3ff)==0)
			printf("Total blocks number %6ld\r",added_blocks);

		root=(struct small_tree *)malloc(sizeof(struct small_tree));
		if(!root) fatal_error("Not enough memory to merge files");

		root->lson=NULL;
		root->rson=NULL;
		memcpy(root->buffer,buffer,14);
		}

	else
		{
		short cmp;

		cmp=compare_small_tree(root->buffer,buffer);
		if(cmp<0)
			{
			root->lson=check_and_add(root->lson,buffer);
			root->lson->parent=root;
			}

		else if(cmp>0)
			{
			root->rson=check_and_add(root->rson,buffer);
			root->rson->parent=root;
			}
		}

	return root;
	}

short merge_file(char *name)
	{
	unsigned char buffer[14];
	long size,diff;
	FILE *h1,*h2;
	struct small_tree *root;

	h1=fopen(DATABASEFILE,"rb");
	if(!h1) return NO;

	h2=fopen(name,"rb");
	if(!h2)
		{
		fclose(h1);
		return NO;
		}

	added_blocks=-1L;

	memset(buffer,0xff,14);
	root=check_and_add(NULL,buffer);

	size=fileln(h1);
	if((size%14)!=0) fatal_error("Invalid database file");

	while(size>0)
		{
		fread(buffer,1,14,h1);
		check_and_add(root,buffer);
		size-=14;
		}

	diff=-added_blocks;

	size=fileln(h2);
	if((size%14)!=0) fatal_error("Invalid file to merge");

	while(size>0)
		{
		fread(buffer,1,14,h2);
		check_and_add(root,buffer);
		size-=14;
		}

	fclose(h1);
	fclose(h2);

	diff+=added_blocks;

	printf("Total blocks number %6ld (really added %ld)\n",added_blocks,diff);

	copy_dbtree();
	flush_small_tree(root);
	free_small_tree(root);

	shuffle();

	return 1;
	}

void merge()
	{
    unsigned short key;
	char filename[255];

	printf("Enter name of file to merge: ");
	gets(filename);

	printf("\n");
	printf("Sure you want to merge '%s' ? (Y/N)\n\n",filename);

	do
		{
		key=readkey();
		} while(key!='n' && key!='y');

	if(key=='y')
		{
		if(merge_file(filename))
			printf("Everything went ok!\n");

		else printf("Something went wrong, perhaps file has not been found!\n");
		}
	else printf("Nothing has been done...\n");

	printf("\n");
	printf("Any key to continue!\n");
	while(readkey()==0);
	}

void erase_temp_file()
    {
    unlink(SHIPDISKFILE);
    }

