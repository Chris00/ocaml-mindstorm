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

// Here's the interface for sending positions to velena engine
// positions are passed in a C string in the form of: x+y+'0'

// where x is a char which defines the level: 'a' = computer weak
// 'b' = computer normal, 'c' = computer strong.

// y is a sequence of moves which brings to the wanted position. Columns are
// numbered from 1 to 7.

// '0' is the ASCII char 48 which tells Velena to ignore any further character

// for example a string like "c4444420" tells velena to analyze the 
// position you reach by placing 5 men in the middle column (3 yellow and 2
// red) and one red man in the second column, and sets velena level to strong.

// It's clear from the sequence of moves which side is in turn to move. 

// You can pass a position only if you need an answer from the AI core.
// if you pass a position where the game is over you get an error in return.

void command_line_input(struct board *board)
   {
   int flag = 1,answer;
   char st[80];

   printf("Enter positions in the form x+y+'0', where:\n");
   printf("x is the level of play: 'a' = weak, 'b' = normal, 'c' = strong\n");
   printf("y is a sequence of moves that brings to the position you want\n");
   printf("  (columns are numbered from 1 to 7)\n");
   printf("'0' is the 48 ASCII char which tells Velena where the string ends\n"); 
   printf("\nEnter 'q' to quit\n\n");

     do {
        printf(">");   // Waiting for the user to enter the string
        gets(st);

        if(st[0]=='q') flag = 0;   // if the string begins with 'q' we quit.
        else {        
             // otherwise we send the string to Velena for parsing...
             answer = playgame(st,board);  

             if(answer>0 && answer<=7)   // We made a legal move... 
                 printf("Velena answers in %d\n",answer);        

             // We made a syntax error, for example we send a 'g' as a level
             // or we send a 8 as a column number.
             else if(answer==-1) printf("?Syntax error\n");

             // We made a positional error, that position is not reachable
             // through a series of legal moves. For example we keep playing
             // after four men have been connected.
             else if(answer==-2) printf("?Positional error\n");

             // Actually this is not of much use. We simply close the if-else
             // condition.
             else printf("?Unprintable error");
	     }
     } while(flag); // We loop until a quit is forced.

   printf("Program terminated\n");
   }

