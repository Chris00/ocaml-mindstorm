(* File: mindstorm.ml

   Copyright (C) 2007

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(*
  http://www.nabble.com/Bluetooth-Direct-and-System-Commands-t2288117.html
  http://mynxt.matthiaspaulscholz.eu/tools/index.html
  http://news.lugnet.com/robotics/nxt/nxthacking/?n=14
*)

type error =
    | No_more_handles
    | No_space
    | No_more_files
    | EOF_expected
(*     | EOF *)
    | Not_a_linear_file
    | File_not_found
    | Handle_already_closed
    | No_linear_space
    | Undefined_error
    | File_is_busy
    | No_write_buffers
    | Append_not_possible
    | File_is_full
    | File_exists
    | Module_not_found
    | Out_of_boundary
    | Illegal_file_name
    | Illegal_handle    (* SHOULD NOT HAPPEN *)

    (** command_error *)
    | Pending (** Pending communication transaction in progress *)
    | Empty_mailbox (** Specified mailbox queue is empty *)
    | Failed (** Request failed (i.e. specified file not found) *)
    | Unknown (** Unknown command opcode *)
    | Insane (** Insane packet *)
    | Out_of_range (** Data contains out-of-range values *)
    | Bus_error (** Communication bus error *)
    | Buffer_full (** No free memory in communication buffer *)
    | Invalid_conn (** Specified channel/connection is not valid *)
    | Busy_conn (** Specified channel/connection not configured or busy *)
    | No_program (** No active program *)
    | Bad_size (** Illegal size specified *)
    | Bad_mailbox (** Illegal mailbox queue ID specified *)
    | Bad_field (** Attempted to access invalid field of a structure *)
    | Bad_io (** Bad input or output specified *)
    | Out_of_memory (** Insufficient memory available *)
    | Bad_arg (** Bad arguments *)

exception Error of error

let undocumented_error = Failure "Mindstorm: undocumented error"

let error =
  let e = Array.create 256 undocumented_error in
  (* Communication protocol errors *)
  e.(0x81) <- Error No_more_handles;
  e.(0x82) <- Error No_space;
  e.(0x83) <- Error No_more_files;
  e.(0x84) <- Error EOF_expected;
  e.(0x85) <- End_of_file (* Error EOF *);
  e.(0x86) <- Error Not_a_linear_file;
  e.(0x87) <- Error File_not_found;
  e.(0x88) <- Error Handle_already_closed;
  e.(0x89) <- Error No_linear_space;
  e.(0x8A) <- Error Undefined_error;
  e.(0x8B) <- Error File_is_busy;
  e.(0x8C) <- Error No_write_buffers;
  e.(0x8D) <- Error Append_not_possible;
  e.(0x8E) <- Error File_is_full;
  e.(0x8F) <- Error File_exists;
  e.(0x90) <- Error Module_not_found;
  e.(0x91) <- Error Out_of_boundary;
  e.(0x92) <- Error Illegal_file_name;
  e.(0x93) <- Error Illegal_handle;
  (* Direct commands errors *)
  e.(0x20) <- Error Pending;
  e.(0x40) <- Error Empty_mailbox;
  e.(0xBD) <- Error Failed;
  e.(0xBE) <- Error Unknown;
  e.(0xBF) <- Error Insane;
  e.(0xC0) <- Error Out_of_range;
  e.(0xDD) <- Error Bus_error;
  e.(0xDE) <- Error Buffer_full;
  e.(0xDF) <- Error Invalid_conn;
  e.(0xE0) <- Error Busy_conn;
  e.(0xEC) <- Error No_program;
  e.(0xED) <- Error Bad_size;
  e.(0xEE) <- Error Bad_mailbox;
  e.(0xEF) <- Error Bad_field;
  e.(0xF0) <- Error Bad_io;
  e.(0xFB) <- Error Out_of_memory;
  e.(0xFF) <- Error Bad_arg;
  e

let check_status status =
  if status <> '\x00' then raise(error.(Char.code status))

(* ---------------------------------------------------------------------- *)
(** Helper functions *)

(* [really_input fd buf ofs len] reads [len] bytes from [fd] and store
   them into [buf] starting at position [ofs]. *)
let really_input =
  let rec loop fd buf i n =
    let r = Unix.read fd buf i n in
    if r < n then loop fd buf (i + r) (n - r) in
  fun fd buf ofs n -> loop fd buf ofs n

let really_read fd n =
  let buf = String.create n in
  really_input fd buf 0 n;
  buf

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding integer. *)
let int16 s i =
  Char.code s.[i] land (Char.code s.[i+1] lsl 8)

let copy_int16 i s ofs =
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  s.[ofs + 1] <- Char.unsafe_chr((i lsr 8) land 0xFF) (* MSB *)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding int.  Since OCaml int are 31 bits (on a 32 platform),
   raise an exn if the last bit is set. *)
let int32 s i =
  if s.[i + 3] >= '\x80' then failwith "Mindstorm.int32: int overflow";
  Char.code s.[i]
  land (Char.code s.[i + 1] lsl 8)
  land (Char.code s.[i + 2] lsl 16)
  land (Char.code s.[i + 3] lsl 32)

(* Copy the int [i] as 4 bytes (little endian) to [s] starting at
   position [ofs].   We assume [i < 2^32].*)
let copy_int32 i s ofs =
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  let i = i lsr 8 in
  s.[ofs + 1] <- Char.unsafe_chr(i land 0xFF);
  let i = i lsr 8 in
  s.[ofs + 2] <- Char.unsafe_chr(i land 0xFF);
  s.[ofs + 3] <- Char.unsafe_chr((i lsr 8) land 0xFF) (* MSB *)

let blit_filename : string -> string -> string -> int -> unit =
  (** [check_filename funname fname pkg ofs] raises
      [Invalid_argument] if the filename [fname] is not valid
      according to the brick limitations; otherwise copy it to [pkg]
      starting at [ofs].  *)
  fun funname fname pkg ofs ->
    let len = String.length fname in
    if len > 19 then invalid_arg funname;
    for i = 0 to String.length fname - 1 do
      if fname.[i] < ' ' || fname.[i] >= '\127' then invalid_arg funname;
    done;
    String.blit fname 0 pkg ofs len;
    (* All filenames must be 19 bytes long, pad if needed. *)
    String.fill pkg (ofs + len) (19 - len) '\000'


(* ---------------------------------------------------------------------- *)
(** Connection *)

type usb
type bluetooth

(* The type parameter is because we want to distinguish usb and
   bluetooth connections as some commands are only available through USB. *)
type 'a conn = {
  fd : Unix.file_descr;
  (* We need specialized function depending on the fact that the
     connection is USB or bluetooth because bluetooth requires a
     prefix of 2 bytes indicating the length of the packet. *)
  send : Unix.file_descr -> string -> unit;
  (* [send fd pkg] sends the package [pkg] over [fd].  [pkg] is
     supposed to come prefixed with 2 bytes indicating its length
     (since this is necessary for bluetooth) -- they will be stripped
     for USB. *)
  recv : Unix.file_descr -> int -> string;
  (* [recv fd n] reads a package a length [n] and return it as a
     string.  For bluetooth, the prefix of 2 bytes indicating the
     length is also read but not returned (and not counted in [n]).
     [recv] checks the status byte and raise an exception accordingly
     (if needed). *)
}


(* [conn_input conn hd buf ofs] reads a variable length package ;
   first read [hd] bytes for the length that must be in bytes [hd-1]
   (MSB) and [hd-2] (LSB), then reads the variable length data and
   store it into [buf] starting at position [ofs].  Returns the actual
   number of bytes read.  [read] checks the status byte and raise an
   exception accordingly (if needed).  *)
let conn_input conn hd buf ofs =
  let pkg = conn.recv conn.fd hd in
  let len = int16 pkg (hd-2) in
  assert(ofs + len <= String.length buf);
  really_input fd buf ofs len;
  len


(** USB ---------- *)

let usb_send fd pkg = ignore(Unix.write fd pkg 2 (String.length pkg - 2))

let usb_recv fd n =
  let pkg = really_read fd n in
  assert(pkg.[0] = '\x02');
  (* pkg.[1] is the cmd id, do we check it ?? *)
  check_status pkg.[2];
  pkg

let connect_usb socket =
  let fd = Unix.openfile socket [] 0 (* FIXME *) in
  { fd = fd;  send = usb_send;  recv = usb_recv }

(** Bluetooth ---------- *)

external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let bt_send fd pkg = ignore(Unix.write fd pkg 0 (String.length pkg))

let bt_recv fd n =
  let size = really_read fd 2 in
  assert(int16 size 0 = n);
  usb_recv fd n

let connect_bluetooth addr =
  let fd = socket_bluetooth addr in
  { fd = fd;  send = bt_send;  recv = bt_recv }


(* ---------------------------------------------------------------------- *)
(** System commands *)

type in_channel = {
  in_fd : Unix.file_descr;
  in_send : Unix.file_descr -> string -> unit;
  in_recv : Unix.file_descr -> int -> string;
  in_handle : char; (* the handle given by the brick *)
  in_length : int; (* file size *)
}

let open_in conn fname =
  let pkg = String.create 23 in
  pkg.[0] <- '\021'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x80'; (* OPEN READ *)
  blit_filename "Mindstorm.open_in" fname pkg 4;
  conn.send conn.fd pkg;
  let ans = conn.recv conn.fd 8 in
  { in_fd = conn.fd;
    in_send = conn.send;
    in_recv = conn.recv;
    in_handle = ans.[3];
    in_length = int32 ans 4;
  }

let in_channel_length ch = ch.in_length

let close_in ch =
  let pkg = String.create 5 in
  pkg.[0] <- '\003'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x84'; (* CLOSE *)
  pkg.[4] <- ch.in_handle;
  ch.in_send ch.in_fd pkg;
  ignore(ch.in_recv ch.in_fd 4) (* check status *)

let input ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFF then
    invalid_arg "Mindstorm.input";
  let pkg = String.create 7 in
  pkg.[0] <- '\005'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x82'; (* READ *)
  pkg.[4] <- ch.in_handle;
  pkg.[5] <- Char.unsafe_chr(len land 0xFF); (* LSB *)
  pkg.[6] <- Char.unsafe_chr(len lsr 8); (* MSB *)
  ch.in_send ch.in_fd pkg;
  ch.in_input ch.in_fd 6 buf ofs


type out_channel = {
  out_fd : Unix.file_descr;
  out_send : Unix.file_descr -> string -> unit;
  out_recv : Unix.file_descr -> int -> string;
  out_handle : char; (* the handle given by the brick *)
  out_length : int; (* size provided by the user of the brick *)
}

type out_flag =
    [ `File of int
    | `Linear of int
    | `Data of int
    | `Append
    ]

(* FIXME: On 64 bits, one must check [length < 2^32] => ARCH64 macro*)
let open_out_gen conn flag_byte length fname =
  if length < 0 then invalid_arg "Mindstorm.open_out";
  let pkg = String.create 28 in
  pkg.[0] <- '\026'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- flag_byte;
  blit_filename "Mindstorm.open_out" fname pkg 4;
  copy_int32 length pkg 24;
  conn.send conn.fd pkg;
  let ans = conn.recv conn.fd 4 in
  { out_fd = conn.fd;
    out_send = conn.send;
    out_recv = conn.recv;
    out_handle = ans.[3];
    out_length = length
  }

let open_out_append conn fname =
  let pkg = String.create 24 in
  pkg.[0] <- '\022'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x8C'; (* APPEND DATA *)
  blit_filename "Mindstorm.open_out" fname pkg 4;
  conn.send conn.fd pkg;
  let ans = conn.recv conn.fd 8 in
  { out_fd = conn.fd;
    out_send = conn.send;
    out_recv = conn.recv;
    out_handle = ans.[3];
    out_length = int32 ans 4;
  }

let open_out conn (flag: out_flag) fname =
  match flag with
  | `File len -> open_out_gen conn '\x81' len fname (* OPEN WRITE *)
  | `Linear len -> open_out_gen conn '\x89' len fname (* OPEN WRITE LINEAR *)
  | `Data len -> open_out_gen conn '\x8B' len fname (* OPEN WRITE DATA *)
  | `Append -> open_out_append conn fname

let out_channel_length ch = ch.out_length

let close_out ch =
  let pkg = String.create 5 in
  pkg.[0] <- '\003'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x84'; (* CLOSE *)
  pkg.[4] <- ch.out_handle;
  ch.out_send ch.out_fd pkg;
  ignore(ch.out_recv ch.out_fd 4) (* check status *)

let output ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFC then
    invalid_arg "Mindstorm.output";
  let pkg = String.create 5 in
  copy_int16 (len + 3) pkg 0; (* 2 BT length bytes; len+3 <= 0xFFFF *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x83'; (* WRITE *)
  pkg.[4] <- ch.out_handle;
  ch.out_send ch.out_fd pkg;
  (* FIXME: how does USB know the length of the data? *)
  ignore(Unix.write ch.out_fd buf ofs len);
  let ans = ch.out_recv ch.out_fd 6 in
  int16 ans 4


(* ---------------------------------------------------------------------- *)
(** Direct commands *)

(* Generic function to send a command without answer (or which answer
   only consists of a success/failure code). *)
let cmd conn ~response ~byte1 ~n f =
  assert(n <= 63);
  let buf = String.make n (if response then '\x00' else '\x80') in
  buf.(1) <- byte1;
  f buf;
  (* send buf on conn *)
  if response then (
    (* Return package expected *)
    let ret = "" (* read return package *) in
    if String.length ret <> 3 || ret.(0) <> '\x02' || ret.(1) <> byte1 then
      failwith "Mindstorm.Command: invalid return package";
    
  )


module Program =
struct
  let start conn name =
    () 

  let stop conn =
    () 

  let name conn =
    "" 
end


module Motor =
struct
  type t = [ `A | `B | `C ]

  type mode = [ `Motor_on | `Brake | `Regulated ]
  type regulation = [ `Idle | `Motor_speed | `Motor_sync ]
  type run_state = [ `Idle | `Ramp_up | `Running | `Ramp_down ]

  type state = {
    power : int;
    mode : mode;
    regulation : regulation;
    turn_ratio : int;
    run_state : run_state;
    tacho_limit : int;
  }

  let set conn num state =
    () 

  let get conn num =
    assert false

  let reset_pos conn num =
    ()
end


module Sensor =
struct
  type t = [ `One | `Two | `Three | `Four ]
  type sensor_type =
      [ `No_sensor
      | `Switch
      | `Temperature
      | `Reflection
      | `Angle
      | `Light_active
      | `Light_inactive
      | `Sound_db
      | `Sound_dba
      | `Custom
      | `Lowspeed
      | `Lowspeed_9v
      | `No_of_sensor_types ]
  type sensor_mode =
      [ `Raw
      | `Boolean
      | `Transition_cnt
      | `Period_counter
      | `Pct_full_scale
      | `Celsius
      | `Fahrenheit
      | `Angle_steps
      | `Slope_mask
      | `Mode_mask ]

  let set conn num sensor_type sensor_mode =
    () 

  let get conn num =
    assert false

  (** {3 Low speed} *)

  let get_status conn num =
    assert false
  let write conn num tx_data =
    ()
  let read conn num =
    "" 
end

module Sound =
struct
  let play conn ?(loop=false) filename =
    () 

  let stop conn =
    () 

  let play_tone conn freq duration =
    ()
end

let message_write conn mailbox msg =
  ()
let message_read conn ?(remove=false) mailbox =
  "" 

let battery_level conn =
  0 
