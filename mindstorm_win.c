/* File: mindstorm_win.c

   Copyright (C) 2007

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>

#include "unixsupport_win.h" /* linked from OCaml CVS */


CAMLprim value ocaml_mindstorm_connect(value vdest)
{
  /* noalloc */
  HANDLE h;

  h = CreateFile(String_val(vdest), GENERIC_READ | GENERIC_WRITE,
                 NULL, NULL, OPEN_EXISTING,
                 FILE_FLAG_WRITE_THROUGH | FILE_FLAG_OVERLAPPED
                 | FILE_FLAG_NO_BUFFERING, NULL);

  /* Error functions available in windows unix.cm[x]a */
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("open", path);
  }
  return win_alloc_handle(h);
}
