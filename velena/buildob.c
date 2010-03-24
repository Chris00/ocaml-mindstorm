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


#include "connect4.h"
#include "pnsearch.h"
#include "proto.h"

#define DATABASEFILE "openbook.cn4"
#define TEMPFILE     "tempship.$$1"

extern short nodeseq[7];
extern unsigned short *myscreen;

char tbp[64];

struct ob_header_type {
                      char string_id[8];
                      short version;
                      unsigned long date;
                      unsigned long dataoffset[42];
                      unsigned long sha_header[5],sha_data[5];
                      };

struct tree_type {
                 struct tree_type *parent,*lson,*rson;
                 char position[14];
                 };

short compare_ob_positions(char *p1,char *p2)
    {
    register short x;

    for(x=0;x<14;x++)
        {
        if     (p1[x]<p2[x]) return -1;
        else if(p1[x]>p2[x]) return +1;
        }

    return 0;
    }

void insert_obook_tree(char *position,struct tree_type *curr_tree)
    {
    short cmp;

    cmp=compare_ob_positions(position,curr_tree->position);
    if(cmp<0)
        {
        if(!curr_tree->lson)
            {
            curr_tree->lson=(struct tree_type *)malloc(sizeof(struct tree_type));
            curr_tree->lson->parent=curr_tree;
            curr_tree->lson->lson=NULL;
            curr_tree->lson->rson=NULL;

            memcpy(curr_tree->lson->position,position,14);
            }
        else insert_obook_tree(position,curr_tree->lson);
        }
    else if(cmp>0)
        {
        if(!curr_tree->rson)
            {
            curr_tree->rson=(struct tree_type *)malloc(sizeof(struct tree_type));
            curr_tree->rson->parent=curr_tree;
            curr_tree->rson->lson=NULL;
            curr_tree->rson->rson=NULL;

            memcpy(curr_tree->rson->position,position,14);
            }
        else insert_obook_tree(position,curr_tree->rson);
        }

    return;
    }

void flush_out_obtree(struct tree_type *node,FILE *h1)
    {
    if(!node) return;

    flush_out_obtree(node->lson,h1);
    if(fwrite(node->position,1,14,h1)!=14)
        fatal_error("Cannot write data to disk, probably insufficient free space");
    flush_out_obtree(node->rson,h1);
    }

struct tree_type *init_obook_tree()
    {
    struct tree_type *obroot;

    obroot=(struct tree_type *)malloc(sizeof(struct tree_type));
    if(!obroot) return NULL;

    obroot->lson=NULL;
    obroot->rson=NULL;
    obroot->parent=NULL;

    return obroot;
    }

void free_obook_tree(struct tree_type *current)
    {
    if(!current) return;

    free_obook_tree(current->lson);
    free_obook_tree(current->rson);
    free(current);
    }

void build_white_opening_book()
    {
    struct tree_type *root;
    short bb[64],x,value;
    unsigned char blk[14],tp[64],t[64];
    long size,posit=0,wps=0,bps=0;
    FILE *h1,*h2;

    root=init_obook_tree();
    if(!root) fatal_error("Not enough memory, darn!");

    h1=fopen(DATABASEFILE,"rb");
    if(!h1) fatal_error("Data Base error!");

    size=fileln(h1);
    if(size%14!=0) fatal_error("Opening book file is corrupted");

    printf("Loading and sorting opening book...\n");

    while(size>0)
        {
        fread(blk,1,12,h1);
        expand_block(blk,t);

        for(x=0;x<64;x++)
            bb[x]=t[x];

        get_lower(bb,tp);
        collapse_position(tp,blk);

        blk[12]=getc(h1);
        blk[13]=getc(h1);

        if(posit>0) insert_obook_tree(blk,root);
        else memcpy(root->position,blk,14);

        if(blk[13]==0) wps++;
        else bps++;

        posit++;
        size-=14;
        }

    fclose(h1);

    printf("Flushing out ordered opening book...\n");

    h2=fopen(WHITE_BOOK,"wb");
    if(!h2) fatal_error("Cannot write output file!");

    flush_out_obtree(root,h2);

    free_obook_tree(root);
    fclose(h2);

    printf("Positions for white %ld; positions for black %ld\n\n",wps,bps);
    }


