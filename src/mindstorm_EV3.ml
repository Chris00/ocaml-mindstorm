(* File: mindstorm_EV3.ml

   Copyright (C) 2015-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* References: LEGO MINDSTORMS EV3 "Communication Developer Kit" and
   "Firmware Developer Kit". *)

#ifndef MODULE_ERR
#define MODULE_ERR(err) STRINGIFY(Mindstorm.EV3: err)
#define MODULE(fn) STRINGIFY(Mindstorm.EV3.fn)
#endif

#include "mindstorm_common.ml"

(** Handling errors
 ***********************************************************************)

type error =
  | Unknown_handle
  | Handle_not_ready
  | Corrupt_file
  | No_handles_available
  | No_permission
  | Illegal_path
  | File_exists
  | End_of_file
  | Size_error
  | Unknown_error
  | Illegal_filename
  | Illegal_connection

exception Error of error

let error =
  let e = Array.make 256 (Failure MODULE_ERR(undocumented error)) in
  e.(0x00) <- Failure "Should not happen, contact the Mindstorm developer!";
  e.(0x01) <- Error Unknown_handle;
  e.(0x02) <- Error Handle_not_ready;
  e.(0x03) <- Error Corrupt_file;
  e.(0x04) <- Error No_handles_available;
  e.(0x05) <- Error No_permission;
  e.(0x06) <- Error Illegal_path;
  e.(0x07) <- Error File_exists;
  e.(0x08) <- Error End_of_file;
  e.(0x09) <- Error Size_error;
  e.(0x0A) <- Error Unknown_error;
  e.(0x0B) <- Error Illegal_filename;
  e.(0x0C) <- Error Illegal_connection;
  e

let check_status_as_exn status =
  if status <> '\x00' then raise(error.(Char.code status))

(** Abstract connection value
 ***********************************************************************)

type usb
type bluetooth = Unix.file_descr

(* The type parameter is because we want to distinguish usb and
   bluetooth connections as some commands are only available through USB. *)
type 'a conn = {
  fd : 'a;
  (* We need specialized function depending on the fact that the
     connection is USB or bluetooth because bluetooth requires a
     prefix of 2 bytes indicating the length of the packet. *)
  send : 'a -> Bytes.t -> unit;
  (* [send fd pkg] sends the package [pkg] over [fd].  [pkg] is
     supposed to come prefixed with 2 bytes indicating its length
     (since this is necessary for bluetooth) -- they will be stripped
     for USB. *)
  recv : 'a -> Bytes.t;
  (* [recv fd n] reads a package a length [n] and return it as a
     string.  For bluetooth, the prefix of 2 bytes indicating the
     length are also read but not returned (and not counted in [n]).
     [recv] checks the status byte and raise an exception accordingly
     (if needed). *)
  close : 'a -> unit;
  (* Close the connection. *)
  mutable msg_counter: int;
  (* Message counter. *)
}

let close conn = conn.close conn.fd

(** Connection
 ***********************************************************************)

let bt_send fd pkg = ignore(Unix.write fd pkg 0 (Bytes.length pkg))

let bt_recv fd =
  let size = uint16 (really_read fd 2) 0 in
  let pkg = really_read fd size in
  pkg

#ifdef MACOSX
(* Mac OS X *)
let connect_bluetooth tty =
  let fd = Unix.openfile tty [Unix.O_RDWR] 0o660 in
  { fd = fd;
    send = bt_send;
    recv = bt_recv;
    close = Unix.close;
    msg_counter = 0 }

#elif defined WIN32 || defined WIN64 || defined CYGWIN
(* Windows *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let connect_bluetooth addr =
  let fd = socket_bluetooth ("\\\\.\\" ^ addr) in
  { fd = fd;
    send = bt_send;
    recv = bt_recv;
    close = Unix.close;
    msg_counter = 0 }

#else
(* Unix *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let connect_bluetooth addr =
  let fd = socket_bluetooth addr in
  { fd = fd;  send = bt_send;
    recv = bt_recv;
    close = Unix.close;
    msg_counter = 0; }

#endif

let recv conn =
  let pkg = conn.recv conn.fd in
  check_status_as_exn (Bytes.get pkg 4);
  pkg

(** Sending commands
 ***********************************************************************)

module Cmd = struct
    let system_reply = '\x01'
    let system_no_reply = '\x81'
    let direct_reply = '\x00'
    let direct_no_reply = '\x80'

    (* [msg] is supposed to have space for the first 6 "control" bytes. *)
    let send conn cmd_type msg =
      let len = Bytes.length msg - 2 in
      assert(len > 0);
      copy_uint16 len msg 0;
      conn.msg_counter <- conn.msg_counter + 1;
      copy_uint16 conn.msg_counter msg 2;
      Bytes.set msg 4 cmd_type;
      Bytes.set msg 5 '\x00';
      Bytes.set msg 6 '\x00';
      conn.send conn.fd msg
  end

let primpar_short = 0x00
let primpar_long  = 0x80
let primpar_const = 0x00
let primpar_value = 0x3F
let primpar_1_byte  = 1
let primpar_2_bytes = 2
let primpar_4_bytes = 3
let primpar_string  = 4 (* zero terminated string *)

(* (v land primpar_value) lor primpar_short lor primpar_const *)
let set_lc0 s ofs v = Bytes.set s ofs (Char.unsafe_chr(v land primpar_value))

(* 2 bytes, v ∈ {-127,...,127} *)
let set_data8 s ofs v =
  Bytes.set s ofs '\x81'; (* PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_1_BYTE *)
  Bytes.set s (ofs+1) (Char.unsafe_chr(v land 0xFF))

(* 3 bytes, v ∈ {-32767,...,32767} *)
let set_data16 s ofs v =
  Bytes.set s ofs '\x82'; (* PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_2_BYTES *)
  copy_uint16 v s (ofs + 1)


module Sound = struct
  let tone conn ~vol ~freq ~ms =
    let m = Bytes.create 17 in
    Bytes.set m 7 '\x94';
    Bytes.set m 8 '\x01';
    set_data8 m 9 vol;
    set_data16 m 11 freq;
    set_data16 m 14 ms;
    Cmd.send conn Cmd.direct_no_reply m

end

;;
