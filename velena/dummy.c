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

// This code is used under Linux to define dummy functions which are
// referenced somewhere in the deadcode.

// Since this is a porting of the core engine I didn't bother to delete
// the functions which are not used... :-)))

int readkey()
        {
        return 0;
        }

void screen_on()
        {
        }

void screen_off()
        {
        }

void clrscr()
        {
        }

long fileln(FILE *f) {
  long int save_pos, file_size;
  save_pos = ftell(f);
  if(save_pos == -1)
    return -1L;
  fseek(f,0L,SEEK_END);
  file_size = ftell(f);
  fseek(f, save_pos,SEEK_SET);
  return file_size;
}



