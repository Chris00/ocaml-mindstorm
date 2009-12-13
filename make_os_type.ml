(* File: make_os_type.ml

   Copyright (C) 2008

   Christophe Troestler <chris_77@users.sourceforge.net>
   WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Return the platform on stdout for conditional statements in the
    Makefile. *)

(* Naive libusb location detection *)
let libusb () =
  if Sys.file_exists "/usr/include/libusb-1.0/libusb.h" then
    "/usr/include/libusb-1.0/"
  else ""

let () =
  let os = match Sys.os_type with
  | "Win32" | "Cygwin" -> "WIN32"
  | _ ->
      (* Distinguish between Unix and MacOS using uname *)
      let fh = Unix.open_process_in "uname -s" in
      let name = input_line fh in
      let os = if name = "Darwin" then "MACOS" else "UNIX" in
      ignore(Unix.close_process_in fh);
      os  in
  print_endline("OS_TYPE=" ^ os);
  print_endline("D_OS=-D" ^ os);
  let usb = libusb() in
  if usb <> ""  then (
    print_endline("D_HAS_USB=-DHAS_USB");
    print_endline("USB_INCLUDE=-I \"" ^ usb ^ "\"");
  );
  if Sys.word_size = 64 then print_endline "D_ARCH64=-DARCH64";
  print_endline("-include Makefile." ^ os)


(* Local Variables: *)
(* compile-command: "make -k make_os_type.cmo" *)
(* End: *)
