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

#define MAXNODECHECK 2810L

struct bintree *tree_pool;
short tpp,semaphore;

short bin_compare(unsigned char *c1,unsigned char *c2)
	{
	unsigned long *p1,*p2;
	short flag=0,x;

	p1=(unsigned long *)c1;
	p2=(unsigned long *)c2;

	for(x=0;x<12 && flag==0;x++)
		{
		if(p1[x]==p2[x]) continue;
		else if(p1[x]>p2[x]) flag=-1;
		else flag=1;
		}

	return flag;
	}

struct bintree *init_bin_tree(struct node *node)
	{
	struct bintree *root;

	root=(struct bintree *)malloc(sizeof(struct bintree));
	if(!root) fatal_error("Cannot allocate root bintree");

	root->parent=NULL;
	root->lson=NULL;
	root->rson=NULL;
	root->node=node;

	return root;
	}

void free_bin_tree(struct bintree *tree)
	{
	if(!tree) return;
	if(tree->lson) free_bin_tree(tree->lson);
	if(tree->rson) free_bin_tree(tree->rson);
	free(tree);
	}

struct bintree *set_node(struct bintree *root,struct node *node,short depth)
	{
	struct bintree *child;

	child=(struct bintree *)malloc(sizeof(struct bintree));
	if(!child) fatal_error("Out of memory, while building tree!");

	child->parent=root;
	child->lson=NULL;
	child->rson=NULL;
	child->node=node;

	return child;
	}

struct node *check_node(struct bintree *root,struct node *node,short depth)
	{
	short cmp;

	cmp=bin_compare(root->node->square,node->square);

	if(cmp<0)
		{
		if(!root->lson)
			{
			root->lson=set_node(root,node,depth);
			return NULL;
			}
		else return check_node(root->lson,node,depth+1);
		}
	else if(cmp>0)
		{
		if(!root->rson)
			{
			root->rson=set_node(root,node,depth);
			return NULL;
			}
		else return check_node(root->rson,node,depth+1);
		}

	return root->node;
	}

/* ========================= Faster Version ============================ */

struct bintree *fast_init_bin_tree(struct node *node)
	{
	struct bintree *root;

    if(semaphore==1) fatal_error("Recursive calls forbidden");
    semaphore=1;
    tpp=1;

    tree_pool=root=(struct bintree *)malloc(MAXNODECHECK*sizeof(struct bintree));
	if(!root) fatal_error("Cannot allocate root bintree");

	root->parent=NULL;
	root->lson=NULL;
	root->rson=NULL;
	root->node=node;

	return root;
	}

void fast_free_bin_tree(struct bintree *tree)
	{
    if(!semaphore || tree!=tree_pool) fatal_error("Free bintree error");
    semaphore=0;
    free(tree_pool);
    tree_pool=NULL;
	}

struct bintree *fast_set_node(struct bintree *root,struct node *node,short depth)
	{
	struct bintree *child;

    child=(struct bintree *)&tree_pool[tpp++];
    if(tpp>=MAXNODECHECK)
        {
        printf("TPP=%d\n",tpp);
        fatal_error("Out of memory, while building tree!");
        }
	child->parent=root;
	child->lson=NULL;
	child->rson=NULL;
	child->node=node;

	return child;
	}

struct node *fast_check_node(struct bintree *root,struct node *node,short depth)
	{
	short cmp;

	cmp=bin_compare(root->node->square,node->square);

	if(cmp<0)
		{
		if(!root->lson)
			{
            root->lson=fast_set_node(root,node,depth);
			return NULL;
			}
        else return fast_check_node(root->lson,node,depth+1);
		}
	else if(cmp>0)
		{
		if(!root->rson)
			{
            root->rson=fast_set_node(root,node,depth);
			return NULL;
			}
        else return fast_check_node(root->rson,node,depth+1);
		}

	return root->node;
	}
