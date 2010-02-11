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

(* Implementation based on the "Bluetooth Developer Kit" available at
   http://mindstorms.lego.com/Overview/NXTreme.aspx

   See also:

   http://www.nabble.com/Bluetooth-Direct-and-System-Commands-t2288117.html
   http://mynxt.matthiaspaulscholz.eu/tools/index.html
   http://news.lugnet.com/robotics/nxt/nxthacking/?n=14
*)

(* TODO:
   - optional timeouts (for reading and receiving status)?
*)


(* Specialised implementation for speed *)
let min i j = if (i:int) < j then i else j


type error =
  | No_more_handles
  | No_space
  | No_more_files
  | EOF_expected
      (*     | End_of_file *) (* use the std exception *)
  | Not_a_linear_file
(*   | File_not_found *) (* separated *)
(*   | Handle_already_closed *)  (* SHOULD NOT HAPPEN *)
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
(*  | Illegal_handle*)    (* SHOULD NOT HAPPEN *)

  (** command_error *)
  | Pending (** Pending communication transaction in progress *)
  | Empty_mailbox (** Specified mailbox queue is empty *)
  | Failed (** Request failed (i.e. specified file not found) *)
  | Unknown (** Unknown command opcode *)
  | Insane (** Insane packet *)
  | Out_of_range (** Data contains out-of-range values *)
  | Bus_error (** Communication bus error.  *)
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

exception File_not_found

let success_char = '\x00'
let eof_char = '\x85'

let error =
  let e = Array.create 256 (Failure "Mindstorm: undocumented error") in
  (* Communication protocol errors *)
  e.(0x81) <- Error No_more_handles;
  e.(0x82) <- Error No_space;
  e.(0x83) <- Error No_more_files;
  e.(0x84) <- Error EOF_expected;
  e.(0x85) <- End_of_file (* Error EOF *);
  e.(0x86) <- Error Not_a_linear_file;
  e.(0x87) <- File_not_found;
  e.(0x88) <- Failure("Error Handle_already_closed");
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
  e.(0x93) <- Failure("Error Illegal_handle");
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

let check_status_as_exn status =
  if status <> success_char then raise(error.(Char.code status))

(* ---------------------------------------------------------------------- *)
(** Helper functions *)

(* [really_input fd buf ofs len] reads [len] bytes from [fd] and store
   them into [buf] starting at position [ofs]. *)
IFDEF WIN32 THEN
let really_input_fd =
  let rec loop ntries fd buf i n =
    if ntries > 50 && i = 0 then
      (* Naive way of detecting that we are not connected -- because,
         when the brick is off, windows connects and "send" the data
         but always reads 0 bytes back. *)
      raise(Unix.Unix_error(Unix.EHOSTDOWN,
                            "Mindstrorm.connect_bluethooth", ""));
    let r = Unix.read fd buf i n in
    if r < n then (
      (* Unix.select implemented on windows: [fd] is a file handle, not
         a socket. *)
      loop (ntries+1) fd buf (i + r) (n - r)
    ) in
  fun fd buf ofs n -> loop 1 fd buf ofs n

ELSE
(* Unix & Mac OS X *)
let really_input_fd =
  let rec loop fd buf i n =
    let r = Unix.read fd buf i n in
    if r < n then (
      (* The doc says 60ms are needed to switch from receive to
         transmit mode. *)
      ignore(Unix.select [fd] [] [] 0.060);
      loop fd buf (i + r) (n - r)
    ) in
  fun fd buf ofs n -> loop fd buf ofs n

ENDIF

let really_read fd n =
  let buf = String.create n in
  really_input_fd fd buf 0 n;
  buf

(* Char of a signed int, 2's complement.  All uses of this function
   are for values in the range -100 .. 100, so if outside the value is
   mapped to the closer endpoint. *)
let signed_chr i =
  let i = if i < -100 then -100 else if i > 100 then 100 else i in
  Char.unsafe_chr(if i >= 0 then i else 256 + i)

(* int of a char, seen as a signed int *)
let signed_code c =
  if Char.code c <= 127 then Char.code c else Char.code c - 256

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding UNSIGNED integer. *)
let uint16 s i =
  assert(i + 1 < String.length s);
  Char.code s.[i] lor (Char.code s.[i+1] lsl 8)

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding SIGNED integer. *)
let int16 s i =
  assert(i + 1 < String.length s);
  let x = Char.code s.[i] lor (Char.code s.[i+1] lsl 8) in
  if x land 0x8000 = 0 then (* positive *) x else
    (* negative, complete with 1 the higher bits *)
    x lor (-0x10000)

let copy_uint16 i s ofs =
  assert(ofs + 1 < String.length s);
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  s.[ofs + 1] <- Char.unsafe_chr((i lsr 8) land 0xFF) (* MSB *)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding UNSIGNED int.  Used when the spec specifies a ULONG.
*)
let uint32 s i =
  assert(i + 3 < String.length s);
  IFNDEF ARCH64 THEN
    (* OCaml int are 31 bits (on a 32 bits platform), thus raise an
       exception if the last bit is set. *)
    if s.[i + 3] >= '\x40' then failwith "Mindstorm.uint32: overflow (32 bits)";
  ELSE () (* For camlp4 3.09 *)
  ENDIF;
  Char.code s.[i]
  lor (Char.code s.[i + 1] lsl 8)
  lor (Char.code s.[i + 2] lsl 16)
  lor (Char.code s.[i + 3] lsl 24)


(* [fill32 = -0x1_0000_0000] but in a way that is accepted by camlp4
   even on 32 bits platforms (no range overflow).  The goal is to add
   enough 1 in front of a 32 bits integer so it is properly
   interpreted as a 63 bits one. *)
let fill32 = -(1 lsl 32)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding SIGNED int.  Used when the spec specifies a SLONG.
   Since OCaml int are 31 bits (on a 32 bits platform), raise an exn
   if the last bit is set. *)
let int32 s i =
  assert(i + 3 < String.length s);
  let msb = Char.code s.[i + 3] in
  if msb >= 0x80 then (
    (* negative number *)
    IFNDEF ARCH64 THEN
      (* 32 bits architecture *)
      if msb land 0x40 = 0 then failwith "Mindstorm.int32: overflow (32 bits)";
    ELSE () (* For camlp4 3.09 *)
    ENDIF;
    let x = Char.code s.[i]
      lor (Char.code s.[i + 1] lsl 8)
      lor (Char.code s.[i + 2] lsl 16)
      lor (msb lsl 24) in
    IFDEF ARCH64 THEN
      (* bits 0 .. 31 are set by x ; complete by setting to 1 the bits
         32 to 62 (Caml ints are 63 bits). *)
      x lor fill32
    ELSE x (* "sign bit" set because [msb land 0x40 = 1] *)  ENDIF
  )
  else (
    (* positive number *)
    IFNDEF ARCH64 THEN
      if msb >= 0x40 then failwith "Mindstorm.int32: overflow (32 bits)";
    ELSE () (* For camlp4 3.09 *)
    ENDIF;
    Char.code s.[i]
    lor (Char.code s.[i + 1] lsl 8)
    lor (Char.code s.[i + 2] lsl 16)
    lor (msb lsl 24)
  )

(* Copy the int [i] as 4 bytes (little endian) to [s] starting at
   position [ofs].  Used when the spec specifies a ULONG. *)
let copy_uint32 i s ofs =
  assert(i >= 0);
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  s.[ofs + 1] <- Char.unsafe_chr((i lsr 8) land 0xFF);
  s.[ofs + 2] <- Char.unsafe_chr((i lsr 16) land 0xFF);
  s.[ofs + 3] <- Char.unsafe_chr((i lsr 24) land 0xFF) (* MSB *)

(* Extracts the filename in [s.[ofs .. ofs+19]] *)
let get_filename s ofs =
  try
    let i = String.index_from s ofs '\000' in
    if i > ofs + 19 then
      failwith "Mindstorm: invalid filename send by the brick!";
    String.sub s ofs (i - ofs)
  with Not_found ->
    failwith "Mindstorm: invalid filename send by the brick!"

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
    (* All filenames must be 19 bytes long + null terminator, pad if
       needed.  *)
    String.fill pkg (ofs + len) (20 - len) '\000'

let usleep sec =
  ignore(Unix.select [] [] [] sec)


(* ---------------------------------------------------------------------- *)
(** Connection *)

type usb
type bluetooth = Unix.file_descr

(* The type parameter is because we want to distinguish usb and
   bluetooth connections as some commands are only available through USB. *)
type 'a conn_send = 'a -> string -> unit
type 'a conn_recv = 'a -> int -> string
type 'a conn_really_input = 'a -> string -> int -> int -> unit
type 'a conn = {
  fd : 'a;
  (* We need specialized function depending on the fact that the
     connection is USB or bluetooth because bluetooth requires a
     prefix of 2 bytes indicating the length of the packet. *)
  send : 'a conn_send;
  (* [send fd pkg] sends the package [pkg] over [fd].  [pkg] is
     supposed to come prefixed with 2 bytes indicating its length
     (since this is necessary for bluetooth) -- they will be stripped
     for USB. *)
  recv : 'a conn_recv;
  (* [recv fd n] reads a package a length [n] and return it as a
     string.  For bluetooth, the prefix of 2 bytes indicating the
     length are also read but not returned (and not counted in [n]).
     [recv] checks the status byte and raise an exception accordingly
     (if needed). *)
  really_input : 'a conn_really_input;
  (* [really_input fd buf ofs len] reads [len] characters from [fd]
     and puts thrm in [buf] starting at position [ofs].  Do NOT read
     the bluetooth prefix bytes, so should not be used for packages
     but only for additional data. *)
  close : 'a -> unit;
  (* Close the connection. *)
  check_status : bool;
  (* Default value of the [check_status] optional arg. *)
}

let close conn = conn.close conn.fd

let recv conn n =
  let pkg = conn.recv conn.fd n in
  check_status_as_exn pkg.[2];
  pkg

let default_check_status conn = function
  | None -> conn.check_status
  | Some s -> s


(** USB -------------------- *)
module USB =
struct
  type device (* a handle to a USB LEGO device. *)

  IFDEF HAS_USB THEN
  IFDEF MACOS THEN
  (* Mac OS X *)
  let bricks () = []
  let connect ?(check_status=false) socket = failwith "Not yet implemented"
    (* libusb should work *)

  ELSE
  IFDEF WIN32 THEN
  (* Windows *)
  let bricks () = []
  let connect ?(check_status=false) socket = failwith "Not yet implemented"
    (* See http://www.microsoft.com/whdc/connect/usb/winusb_howto.mspx *)

  ELSE
  (* Unix *)
  external bricks : unit -> device list = "ocaml_mindstorm_bricks"
  external exit_libusb : unit -> unit = "ocaml_mindstorm_exit"
  external connect_device : device -> usb = "ocaml_mindstorm_connect_usb"
  external close : usb -> unit = "ocaml_mindstorm_close_usb"
  external write : usb -> string -> int -> int -> unit
    = "ocaml_mindstorm_usb_write"
  external really_input : usb -> string -> int -> int -> unit
    = "ocaml_mindstorm_usb_really_input"

  let () = at_exit exit_libusb

  let recv usb n =
    let buf = String.create n in
    really_input usb buf 0 n;
    buf

  (* Ignore the first 2 bytes of [pkg] that are for bluetooth only *)
  let send fd pkg = write fd pkg 2 (String.length pkg - 2)

  let connect ?(check_status=false) dev =
    let fd = connect_device dev in
    { fd = fd;  send = send;
      recv = recv;  really_input = really_input;
      close = close;
      check_status = check_status }

  ENDIF
  ENDIF
  ELSE
  (* No USB libary *)
  let bricks () = []
  let connect ?(check_status=false) socket =
    failwith "The Mindstorm module was compliled without USB support"
  ENDIF
end

(** Bluetooth -------------------- *)

let bt_send fd pkg = ignore(Unix.write fd pkg 0 (String.length pkg))

let bt_recv fd n =
  let _size = really_read fd 2 in
  let pkg = really_read fd n in
  assert(pkg.[0] = '\x02');
  (* pkg.[1] is the cmd id, do we check it ?? *)
  (* We wanted to check the status and raise the corresponding
     exception here but we cannot because of the behavior of [input]. *)
  pkg
;;

IFDEF MACOS THEN
(* Mac OS X *)
let connect_bluetooth ?(check_status=false) tty =
  let fd = Unix.openfile tty [Unix.O_RDWR] 0o660 in
  { fd = fd;  send = bt_send;
    recv = bt_recv;  really_input = really_input_fd;
    close = Unix.close;
    check_status = check_status }

ELSE
IFDEF WIN32 THEN
(* Windows *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let connect_bluetooth ?(check_status=false) addr =
  let fd = socket_bluetooth ("\\\\.\\" ^ addr) in
  { fd = fd;  send = bt_send;
    recv = bt_recv;  really_input = really_input_fd;
    close = Unix.close;
    check_status = check_status }

ELSE
(* Unix *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let connect_bluetooth ?(check_status=false) addr =
  let fd = socket_bluetooth addr in
  { fd = fd;  send = bt_send;
    recv = bt_recv;  really_input = really_input_fd;
    close = Unix.close;
    check_status = check_status }

ENDIF
ENDIF


(* ---------------------------------------------------------------------- *)
(** System commands *)

type 'a in_channel = {
  in_fd : 'a;
  in_send : 'a conn_send;
  in_recv : 'a conn_recv;
  in_really_input : 'a conn_really_input;
  in_handle : char; (* the handle given by the brick *)
  in_length : int; (* file size *)
  mutable in_left : int; (* number of bytes left to be read,
                            = 0 iff EOF
                            < 0 iff the channel is closed *)
}

let open_in conn fname =
  let pkg = String.create 24 in
  pkg.[0] <- '\022'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x80'; (* OPEN READ *)
  blit_filename "Mindstorm.open_in" fname pkg 4;
  conn.send conn.fd pkg;
  let ans = recv conn 8 in
  let len = uint32 ans 4 in (* len <= 64Kb of RAM *)
  { in_fd = conn.fd;
    in_send = conn.send;
    in_recv = conn.recv;
    in_really_input = conn.really_input;
    in_handle = ans.[3];
    in_length = len;
    in_left = len;
  }

let in_channel_length ch =
  if ch.in_left < 0 then raise(Sys_error "Closed NXT in_channel");
  ch.in_length

let close_in ch =
  if ch.in_left >= 0 then begin
    (* Channel not yet closed. *)
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x84'; (* CLOSE *)
    pkg.[4] <- ch.in_handle;
    ch.in_send ch.in_fd pkg;
    let ans = ch.in_recv ch.in_fd 4 in
    ch.in_left <- -1;
    check_status_as_exn ans.[2];
  end

let input ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFF then
    invalid_arg "Mindstorm.input";
  if ch.in_left < 0 then raise(Sys_error "Closed NXT in_channel");
  if ch.in_left = 0 then raise End_of_file;
  if len = 0 then 0
  else begin
    let len_to_read = min len ch.in_left (* > 0 *) in
    let pkg = String.create 7 in
    pkg.[0] <- '\005'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x82'; (* READ *)
    pkg.[4] <- ch.in_handle;
    copy_uint16 len_to_read pkg 5;
    ch.in_send ch.in_fd pkg;
    (* Variable length return package.  The number of bytes that was
       requested [len_to_read] is always returned.  Beware that if we
       read the last bytes -- even if there were indeed bytes to read --
       the status will indicate EOF. *)
    let ans = ch.in_recv ch.in_fd 6 in
    let r = uint16 ans 4 in (* # bytes read *)
    assert(r = len_to_read);
    ch.in_really_input ch.in_fd buf ofs len_to_read;
    ch.in_left <- ch.in_left - len_to_read;
    let status = ans.[2] in
    (* We manage EOF ourselves to respect OCaml conventions: *)
    if status = success_char || status = eof_char then len_to_read
    else (check_status_as_exn status; 0)
  end

type 'a out_channel = {
  out_fd : 'a;
  out_send : 'a conn_send;
  out_recv : 'a conn_recv;
  out_handle : char; (* the handle given by the brick *)
  out_length : int; (* size provided by the user of the brick *)
  mutable out_closed : bool;
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
  pkg.[3] <- flag_byte; (* type of open *)
  blit_filename "Mindstorm.open_out" fname pkg 4;
  copy_uint32 length pkg 24; (* length <= 64Kb of RAM *)
  conn.send conn.fd pkg;
  let ans = recv conn 4 in
  { out_fd = conn.fd;
    out_send = conn.send;
    out_recv = conn.recv;
    out_handle = ans.[3];
    out_length = length;
    out_closed = false;
  }

let open_out_append conn fname =
  let pkg = String.create 24 in
  pkg.[0] <- '\022'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x8C'; (* OPEN APPEND DATA *)
  blit_filename "Mindstorm.open_out" fname pkg 4;
  conn.send conn.fd pkg;
  let ans = recv conn 8 in
  { out_fd = conn.fd;
    out_send = conn.send;
    out_recv = conn.recv;
    out_handle = ans.[3];
    out_length = uint32 ans 4; (* <= 64Kb of RAM *)
    out_closed = false;
  }

let open_out conn (flag: out_flag) fname =
  match flag with
  | `File len -> open_out_gen conn '\x81' len fname (* OPEN WRITE *)
  | `Linear len -> open_out_gen conn '\x89' len fname (* OPEN WRITE LINEAR *)
  | `Data len -> open_out_gen conn '\x8B' len fname (* OPEN WRITE DATA *)
  | `Append -> open_out_append conn fname

let out_channel_length ch =
  if ch.out_closed then raise(Sys_error "Closed NXT out_channel");
  ch.out_length

let close_out ch =
  if not ch.out_closed then begin
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x84'; (* CLOSE *)
    pkg.[4] <- ch.out_handle;
    ch.out_send ch.out_fd pkg;
    let ans = ch.out_recv ch.out_fd 4 in
    ch.out_closed <- true; (* let the channel be closed even in case of error *)
    check_status_as_exn ans.[2];
  end

let output ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFC then
    invalid_arg "Mindstorm.output";
  if ch.out_closed then raise(Sys_error "Closed NXT out_channel");
  let pkg = String.create (5 + len) in
  copy_uint16 (len + 3) pkg 0; (* 2 BT length bytes; len+3 <= 0xFFFF *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x83'; (* WRITE *)
  pkg.[4] <- ch.out_handle;
  String.blit buf ofs pkg 5 len;
  ch.out_send ch.out_fd pkg;
  let ans = ch.out_recv ch.out_fd 6 in
  check_status_as_exn ans.[2];
  uint16 ans 4


let remove conn fname =
  let pkg = String.create 24 in
  pkg.[0] <- '\022'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x85'; (* DELETE *)
  blit_filename "Mindstorm.remove" fname pkg 4;
  conn.send conn.fd pkg;
  ignore(recv conn 23) (* check status *)


module Find =
struct
  type 'a iterator = {
    it_fd : 'a;
    it_send : 'a conn_send;
    it_recv : 'a conn_recv;
    it_handle : char;
    mutable it_closed : bool;
    mutable it_fname : string; (* current filename *)
    mutable it_flength : int; (* current filename length. *)
  }

  let close it =
    if not it.it_closed && it.it_flength >= 0 then begin
      (* The iterator is not closed and has requested a handle. *)
      let pkg = String.create 5 in
      pkg.[0] <- '\003'; (* size, LSB *)
      pkg.[1] <- '\000'; (* size, MSB *)
      pkg.[2] <- '\x01';
      pkg.[3] <- '\x84'; (* CLOSE *)
      pkg.[4] <- it.it_handle;
      it.it_send it.it_fd pkg;
      let ans = it.it_recv it.it_fd 4 in
      it.it_closed <- true; (* close even if an exception is raised *)
      check_status_as_exn ans.[2];
    end

  let patt conn fpatt =
    let pkg = String.create 24 in
    pkg.[0] <- '\022'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x86'; (* FIND FIRST *)
    blit_filename "Mindstorm.find" fpatt pkg 4;
    conn.send conn.fd pkg;
    let ans = recv conn 28 in (* might raise File_not_found *)
    { it_fd = conn.fd;
      it_send = conn.send;
      it_recv = conn.recv;
      it_handle = ans.[3];
      it_closed = false;
      it_fname = get_filename ans 4;
      it_flength = uint32 ans 24; (* length <= 64Kb of RAM *)
    }

  let current i =
    if i.it_closed then raise(Sys_error "Closed NXT file_iterator");
    i.it_fname

  let current_size i =
    if i.it_closed then raise(Sys_error "Closed NXT file_iterator");
    i.it_flength

  let next i =
    if i.it_closed then raise(Sys_error "Closed NXT file_iterator");
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x87'; (* FIND NEXT *)
    pkg.[4] <- i.it_handle;
    i.it_send i.it_fd pkg;
    let ans = i.it_recv i.it_fd 28 in
    i.it_fname <- get_filename ans 4;
    i.it_flength <- uint32 ans 24; (* length <= 64Kb of RAM *)
    (* In the case the status is File_not_found, the doc says the
       handle is closed by the brick. (FIXME: confirm?) *)
    if ans.[2] = eof_char then i.it_closed <- true;
    check_status_as_exn ans.[2]

  let iter conn ~f fpatt =
    match (try Some(patt conn fpatt) with File_not_found -> None) with
    | None -> ()
    | Some i ->
        try
          let has_more = ref true in
          while !has_more do
            f i.it_fname i.it_flength;
            (try next i with File_not_found -> has_more := false)
          done
        with e ->
          close i; raise e (* exn raised by [f] must close the iterator *)

  let fold conn ~f fpatt a0 =
    match (try Some(patt conn fpatt) with File_not_found -> None) with
    | None -> a0
    | Some i ->
        let a = ref a0 in
        try
          let has_more = ref true in
          while !has_more do
            a := f i.it_fname i.it_flength !a;
            (try next i with File_not_found -> has_more := false)
          done;
          !a
        with e -> close i; raise e

  let map conn ~f patt =
    let l = ref [] in
    iter conn ~f:(fun name length -> l := f name length :: !l) patt;
    List.rev !l
end

(* ---------------------------------------------------------------------- *)
(** Brick info *)

let firmware_version conn =
  conn.send conn.fd "\002\000\x01\x88";
  let ans = recv conn 7 in
  (Char.code ans.[4], Char.code ans.[3],
   Char.code ans.[6], Char.code ans.[5])

let boot conn =
  let arg = "Let's dance: SAMBA" in
  let len = String.length arg in
  let pkg = String.create 23 in
  pkg.[0] <- '\021';
  pkg.[1] <- '\000';
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x97'; (* BOOT COMMAND *)
  String.blit arg 0 pkg 4 len;
  String.fill pkg (4 + len) (19 - len) '\000';
  conn.send conn.fd pkg;
  ignore(recv conn 7)

let set_brick_name ?check_status conn name =
  let check_status = default_check_status conn check_status in
  let len = String.length name in
  if len > 15 then
    invalid_arg "Mindstorm.set_brick_name: name too long (max 15 chars)";
  for i = 0 to len - 1 do
    if name.[i] < ' ' || name.[i] >= '\127' then
      invalid_arg "Mindstorm.set_brick_name: name contains invalid chars";
  done;
  let pkg = String.create 20 in
  pkg.[0] <- '\018'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- if check_status then '\x01' else '\x81';
  pkg.[3] <- '\x98'; (* SET BRICK NAME *)
  String.blit name 0 pkg 4 len;
  String.fill pkg (4 + len) (16 - len) '\000'; (* pad if needed *)
  conn.send conn.fd pkg;
  if check_status then ignore(recv conn 3)


type brick_info = {
  brick_name : string;
  bluetooth_addr : string;
  signal_strength : int;
  free_user_flash : int;
}

let get_brick_name s i0 i1 =
  (** Extract the brick name of "" if it fails (should not happen). *)
  try
    let j = min i1 (String.index_from s i0 '\000') in
    String.sub s i0 (j - i0)
  with Not_found -> ""

let string_of_bluetooth_addr =
  let u s i = Char.code(String.unsafe_get s i) in
  fun addr ->
    assert(String.length addr = 6);
    Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
      (u addr 0) (u addr 1) (u addr 2) (u addr 3) (u addr 4) (u addr 5)

let get_device_info conn =
  conn.send conn.fd "\002\000\x01\x9B"; (* GET DEVICE INFO *)
  let ans = recv conn 33 in
  { brick_name = get_brick_name ans 3 17; (* 14 chars + null *)
    bluetooth_addr = (* ans.[18 .. 24], drop null terminator *)
      string_of_bluetooth_addr(String.sub ans 18 6);
    signal_strength = uint32 ans 25; (* always return 0! *)
    free_user_flash = uint32 ans 29; (* <= 64Kb of RAM *)
  }

let delete_user_flash conn =
  conn.send conn.fd "\002\000\x01\xA0"; (* DELETE USER FLASH *)
  ignore(recv conn 3)

let bluetooth_reset conn =
  conn.send conn.fd "\002\000\x01\xA4"; (* BLUETOOTH FACTORY RESET *)
  ignore(recv conn 3)

let char_of_buffer_type = function
  | `Poll_buffer -> '\x00'
  | `High_speed_buffer -> '\x01'

let poll_length conn buf =
 let pkg = String.create 5 in
  pkg.[0] <- '\003'; (* 2 bluetooth bytes *)
  pkg.[1] <- '\000';
  pkg.[2] <- '\x01';
  pkg.[3] <- '\xA1'; (* POLL COMMAND LENGTH *)
  pkg.[4] <- char_of_buffer_type buf;
  conn.send conn.fd pkg;
  let ans = recv conn 5 in
  Char.code ans.[4]

let poll_command conn buf len =
 let pkg = String.create 6 in
  pkg.[0] <- '\004'; (* 2 bluetooth bytes *)
  pkg.[1] <- '\000';
  pkg.[2] <- '\x01';
  pkg.[3] <- '\xA2'; (* POLL COMMAND *)
  pkg.[4] <- char_of_buffer_type buf;
  pkg.[5] <- char_of_int len;
  conn.send conn.fd pkg;
  let ans = recv conn 65 in
  (Char.code ans.[4], String.sub ans 5 60) (* FIXME: Null terminator? *)


let keep_alive conn =
  conn.send conn.fd "\002\000\x00\x0D"; (* KEEPALIVE *)
  let ans = recv conn 7 in
  uint32 ans 3 (* FIXME: # of miliseconds can overflow 30 bits? *)


let battery_level conn =
  conn.send conn.fd "\002\000\x00\x0B"; (* GETBATTERYLEVEL *)
  let ans = recv conn 5 in
  uint16 ans 3


(* ---------------------------------------------------------------------- *)
(** Direct commands *)

(* More documentation about the system commands is provided in the
   "Executable File and Bytecode Reference" downloadable from
   http://mindstorms.lego.com/Overview/NXTreme.aspx.  *)

(* Generic function to send a command of [n] bytes without an answer
   (but with the option of checking the return status).  [fill] is
   responsible for filling [pkg] according to the command.  BEWARE
   that because of the 2 BT bytes, all indexes must be shifted by +2
   w.r.t. the spec. *)
let cmd conn ~check_status ~byte1 ~n fill =
  assert(n <= 0xFF); (* all fixed length commands *)
  let pkg = String.create (n + 2) in
  pkg.[0] <- Char.unsafe_chr n; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- if check_status then '\x00' else '\x80';
  pkg.[3] <- byte1;
  fill pkg;
  conn.send conn.fd pkg;
  if check_status then ignore(recv conn 3)


module Program =
struct
  let start ?check_status conn name =
    let check_status = default_check_status conn check_status in
    cmd conn ~check_status ~byte1:'\x00' ~n:22  begin fun pkg ->
      blit_filename "Mindstorm.Program.start" name pkg 4
    end

  let stop ?check_status conn =
    let check_status = default_check_status conn check_status in
    cmd conn ~check_status ~byte1:'\x01' ~n:2 (fun _ -> ())

  let name conn =
    conn.send conn.fd "\002\000\x00\x11"; (* GETCURRENTPROGRAMNAME *)
    let ans = recv conn 23 in
    get_filename ans 3
end


module Motor =
struct
  type port = char
  let a = '\x00'
  let b = '\x01'
  let c = '\x02'
  let all = '\xFF'

  type regulation = [ `Idle | `Motor_speed | `Motor_sync ]
      (* It is a bit strange one does not seem to be allowed to specify
         [`Motor_speed] and [`Motor_sync] at the same time... but the
         "NXT Executable File Specification" says clearly "Unlike the
         MODE property, REG_MODE is not a bitfield. You can set only
         one REG_MODE value at a time." *)
  type run_state = [ `Idle | `Ramp_up | `Running | `Ramp_down ]

  type state = {
    speed : int;
    motor_on : bool; (* FIXME: do we remove this and set
                        motor_on = (speed <> 0) ? *)
    brake : bool;
    regulation : regulation;
    turn_ratio : int;
    run_state : run_state;
    tach_limit : int;
  }

  let speed ?(tach_limit=0) ?(brake=true) ?(sync=false) ?(turn_ratio=0) s =
    {
      speed = s;   motor_on = s <> 0;  brake = brake;
      regulation = (if sync then `Motor_sync else `Motor_speed);
      turn_ratio = turn_ratio;
      run_state = `Running;
      tach_limit = tach_limit; (* 0 -> run forever *)
    }



  let set ?check_status conn port st =
    let check_status = default_check_status conn check_status in
    if st.tach_limit < 0 then
      invalid_arg "Mindstorm.Motor.set: state.tach_limit must be >= 0";
    (* SETOUTPUTSTATE *)
    cmd conn ~check_status ~byte1:'\x04' ~n:13   begin fun pkg ->
      pkg.[4] <- port;
      pkg.[5] <- signed_chr st.speed;
      let mode = 0x00 (* COAST mode *) in
      let mode = if st.motor_on then mode lor 0x01 else mode in
      let mode = if st.brake then mode lor 0x02 else mode in
      (* [Regulate]: Enables active power regulation according to
         value of REG_MODE (interactive motors only).  You must use
         the REGULATED bit in conjunction with the REG_MODE property =>
         [regulate] influences 2 bytes send to the brick *)
      let mode, regulation = (match st.regulation with
                              | `Idle -> mode, '\x00'
                              | `Motor_speed -> mode lor 0x04, '\x01'
                              | `Motor_sync  -> mode lor 0x04, '\x02') in
      pkg.[6] <- Char.unsafe_chr mode;
      pkg.[7] <- regulation;
      pkg.[8] <- signed_chr st.turn_ratio;
      pkg.[9] <- (match st.run_state with
                  | `Idle -> '\x00' | `Ramp_up -> '\x10'
                  | `Running -> '\x20' | `Ramp_down -> '\x40');
      copy_uint32 st.tach_limit pkg 10; (* bytes 8-11 (bug in the spec) *)
    end

  let get conn motor =
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* BT bytes *)
    pkg.[1] <- '\000';
    pkg.[2] <- '\x00'; (* get an answer *)
    pkg.[3] <- '\x06'; (* GETOUTPUTSTATE *)
    pkg.[4] <- motor;
    conn.send conn.fd pkg;
    let ans = recv conn 25 in
    let mode = Char.code ans.[5] in
    let st =
      { speed = signed_code ans.[4];
        motor_on = (mode land 0x01 <> 0);
        brake = (mode land 0x02 <> 0);
        regulation = (if mode land 0x04 = 0 then `Idle
                      else match ans.[6] with
                      | '\x00' -> `Idle
                      | '\x01' -> `Motor_speed
                      | '\x02' -> `Motor_sync
                      | _ -> `Idle);
        turn_ratio = signed_code ans.[7];
        run_state = (match ans.[8] with
                     | '\x00' -> `Idle | '\x10' -> `Ramp_up
                     | '\x20' -> `Running | '\x40' -> `Ramp_down
                     | _ -> `Idle);
        tach_limit = uint32 ans 9;
      }
    and tach_count = int32 ans 13
    and block_tach_count = int32 ans 17
    and rotation_count = int32 ans 21
      (* The Exec. File Spec. says ROTATION_COUNT Legal value range is
         [-2147483648, 2147483647] so all 32 bits may be used. *) in
    (st, tach_count, block_tach_count, rotation_count)


  let reset_pos ?check_status conn ?(relative=false) port =
    let check_status = default_check_status conn check_status in
    (* RESETMOTORPOSITION *)
    cmd conn ~check_status ~byte1:'\x0A' ~n:4 (fun pkg ->
      pkg.[4] <- port;
      pkg.[5] <- if relative then '\x01' else '\x00';
    )
end


module Sensor =
struct
  type t
  type port = [ `S1 | `S2 | `S3 | `S4 ]
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
      | `Highspeed
      | `Color_full
      | `Color_red
      | `Color_green
      | `Color_blue
      | `Color_none
      ]
  type mode =
      [ `Raw
      | `Bool
      | `Transition_cnt
      | `Period_counter
      | `Pct_full_scale
      | `Celsius
      | `Fahrenheit
      | `Angle_steps
      | `Slope_mask
      (*| `Mode_mask*)
      ]


  let char_of_port = function
    | `S1 -> '\000' | `S2 -> '\001'
    | `S3 -> '\002' | `S4 -> '\003'

  let set ?check_status conn port sensor_type sensor_mode =
    let check_status = default_check_status conn check_status in
    cmd conn ~check_status ~byte1:'\x05' ~n:5 begin fun pkg ->
      pkg.[4] <- char_of_port port;
      pkg.[5] <- (match sensor_type with
                  | `No_sensor	 -> '\x00'
                  | `Switch	 -> '\x01'
                  | `Temperature -> '\x02'
                  | `Reflection	 -> '\x03'
                  | `Angle	 -> '\x04'
                  | `Light_active -> '\x05'
                  | `Light_inactive -> '\x06'
                  | `Sound_db	 -> '\x07'
                  | `Sound_dba	 -> '\x08'
                  | `Custom	 -> '\x09'
                  | `Lowspeed	 -> '\x0A'
                  | `Lowspeed_9v -> '\x0B'
                  | `Highspeed	 -> '\x0C'
                  (* From the Lejos "SensorsConstants.java": *)
                  | `Color_full   -> '\x0D'
                  | `Color_red    -> '\x0E'
                  | `Color_green  -> '\x0F'
                  | `Color_blue   -> '\x10'
                  | `Color_none   -> '\x11');
      pkg.[6] <- (match sensor_mode with
                  | `Raw	 -> '\x00'
                  | `Bool	 -> '\x20'
                  | `Transition_cnt -> '\x40'
                  | `Period_counter -> '\x60'
                  | `Pct_full_scale -> '\x80'
                  | `Celsius	 -> '\xA0'
                  | `Fahrenheit	 -> '\xC0'
                  | `Angle_steps -> '\xE0'
                  | `Slope_mask	 -> '\x1F'
                  | `Mode_mask	 -> '\xE0' (* = `Angle_steps !! *)
                 );
    end

  type data = {
    sensor_type : sensor_type;
    mode : mode;
    valid : bool;
    (* is_calibrated : bool; *)
    raw : int;
    normalized : int;
    scaled : int;
    (* calibrated: int *)
  }

  let get conn port =
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* BT bytes *)
    pkg.[1] <- '\000';
    pkg.[2] <- '\x00'; (* get a reply *)
    pkg.[3] <- '\x07'; (* GETINPUTVALUES *)
    pkg.[4] <- char_of_port port;
    conn.send conn.fd pkg;
    let ans = recv conn 16 in
    { valid = ans.[4] <> '\x00';
      sensor_type = (match ans.[6] with
                     | '\x00' -> `No_sensor
                     | '\x01' -> `Switch
                     | '\x02' -> `Temperature
                     | '\x03' -> `Reflection
                     | '\x04' -> `Angle
                     | '\x05' -> `Light_active
                     | '\x06' -> `Light_inactive
                     | '\x07' -> `Sound_db
                     | '\x08' -> `Sound_dba
                     | '\x09' -> `Custom
                     | '\x0A' -> `Lowspeed
                     | '\x0B' -> `Lowspeed_9v
                     | '\x0C' -> `Highspeed
                     | '\x0D' -> `Color_full
                     | '\x0E' -> `Color_red
                     | '\x0F' -> `Color_green
                     | '\x10' -> `Color_blue
                     | '\x11' -> `Color_none
                     | _ -> raise(Error Out_of_range));
        mode = (match ans.[7] with
                | '\x00' -> `Raw
                | '\x20' -> `Bool
                | '\x40' -> `Transition_cnt
                | '\x60' -> `Period_counter
                | '\x80' -> `Pct_full_scale
                | '\xA0' -> `Celsius
                | '\xC0' -> `Fahrenheit
                | '\xE0' -> `Angle_steps
                | '\x1F' -> `Slope_mask
                (*| '\xE0' -> `Mode_mask*)
                | _ -> raise(Error Out_of_range));
      raw = uint16 ans 8;
      normalized = uint16 ans 10;
      scaled = int16 ans 12;
      (* calibrated = int16 and 14; *)
    }

  let color_of_scaled_tab =
    [| `Black (* unused *) ; `Black; `Blue; `Green; `Yellow; `Red; `White |]
  let color_of_data data =
    if data.sensor_type <> `Color_full then
      invalid_arg "Mindstorm.Sensor.color_of_scaled: the sensor type must \
	be `Color_full";
    if data.scaled < 1 || data.scaled > 6 then
      invalid_arg "Mindstorm.Sensor.color_of_scaled: scaled data out of range";
    color_of_scaled_tab.(data.scaled)


  let reset_scaled ?check_status conn port =
    let check_status = default_check_status conn check_status in
    (* RESETINPUTSCALEDVALUE *)
    cmd conn ~check_status ~byte1:'\x08' ~n:3  begin fun pkg ->
      pkg.[4] <- char_of_port port
    end

  (** {3 Low speed} *)

  let get_status conn port =
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* 2 BT bytes *)
    pkg.[1] <- '\000';
    pkg.[2] <- '\x00';
    pkg.[3] <- '\x0E'; (* LSGETSTATUS *)
    pkg.[4] <- char_of_port port;
    conn.send conn.fd pkg;
    let ans = recv conn 4 in
    Char.code ans.[3]

  let write ?check_status conn port ?(rx_length=0) tx_data =
    let check_status = default_check_status conn check_status in
    let n = String.length tx_data in
    if n > 255 then invalid_arg "Mindstorm.Sensor.write: length tx_data > 255";
    if rx_length < 0 || rx_length > 255 then
      invalid_arg "Mindstorm.Sensor.write: length rx_length not in 0 .. 255";
    let pkg = String.create (7 + n) in
    copy_uint16 (n + 5) pkg 0; (* 2 bluetooth bytes *)
    pkg.[2] <- if check_status then '\x00' else '\x80';
    pkg.[3] <- '\x0F'; (* LSWRITE *)
    pkg.[4] <- char_of_port port;
    pkg.[5] <- Char.unsafe_chr n; (* tx bytes (# bytes sent) *)
    pkg.[6] <- Char.unsafe_chr rx_length;
    String.blit tx_data 0 pkg 7 n;
    conn.send conn.fd pkg;
    if check_status then ignore(recv conn 3)

  let read conn port =
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* 2 bluetooth bytes *)
    pkg.[1] <- '\000';
    pkg.[2] <- '\x00';
    pkg.[3] <- '\x10'; (* LSREAD *)
    pkg.[4] <- char_of_port port;
    conn.send conn.fd pkg;
    let ans = recv conn 20 in
    let rx_length = min (Char.code ans.[3]) 16 in
    String.sub ans 4 rx_length

  (** Ultrasonic sensor *)
  (* Specification of the I2C protocol for the ultrasonic sensor given
     in the Appendix 7 of "Hardware Developer Kit" available at
     http://mindstorms.lego.com/Overview/NXTreme.aspx *)
  module Ultrasonic =
  struct
    type 'a t = {
      u_fd : 'a;
      u_send : 'a conn_send;
      u_recv : 'a conn_recv;
      port : char;
      ls_status : string; (* share the string across all status calls *)
    }

    let make conn port =
      (* We need to let the I2C time to init, so better to check the
         return status. *)
      set ~check_status:true conn port `Lowspeed_9v `Raw;
      let port = char_of_port port in
      let ls_status = String.create 5 in
      ls_status.[0] <- '\003'; (* 2 BT bytes *)
      ls_status.[1] <- '\000';
      ls_status.[2] <- '\x00';
      ls_status.[3] <- '\x0E'; (* LSGETSTATUS *)
      ls_status.[4] <- port;
      { u_fd = conn.fd;
        u_send = conn.send;
        u_recv = conn.recv;
        port = port;
        ls_status = ls_status;
      }

    let write_cmd ~check_status us byte2 byte3 =
      (* Special write because the string length is statically known *)
      let pkg = String.create 10 in
      pkg.[0] <- '\008'; pkg.[1] <- '\000'; (* 2 BT bytes *)
      pkg.[2] <- if check_status then '\x00' else '\x80';
      pkg.[3] <- '\x0F'; (* LSWRITE *)
      pkg.[4] <- us.port;
      pkg.[5] <- '\003'; (* tx bytes (# bytes sent) *)
      pkg.[6] <- '\000'; (* rx bytes (length answer) *)
      pkg.[7] <- '\x02'; (* 1st byte of command: I2C dev *)
      pkg.[8] <- byte2;  (* 2nd byte of command *)
      pkg.[9] <- byte3;  (* 3rd byte of command *)
      us.u_send us.u_fd pkg;
      if check_status then begin
        let ans = us.u_recv us.u_fd 3 in
        check_status_as_exn ans.[2]
      end

    let write_val ~check_status us cmd byte2 v =
      if v < 0 || v > 255 then invalid_arg(Printf.sprintf "Mindstorm.Sensor.\
		Ultrasonic.set: %s arg not in 0 .. 255" cmd);
      write_cmd ~check_status us byte2 (Char.unsafe_chr v)

    let set ?(check_status=true) us cmd =
      match cmd with
      | `Off ->       write_cmd ~check_status us '\x41' '\x00'
      | `Meas ->      write_cmd ~check_status us '\x41' '\x01'
      | `Meas_cont -> write_cmd ~check_status us '\x41' '\x02'
      | `Event ->     write_cmd ~check_status us '\x41' '\x03'
      | `Reset ->     write_cmd ~check_status us '\x41' '\x04'
      | `Meas_interval i -> write_val ~check_status us "`Meas_interval" '\x40' i
      | `Zero z -> write_val ~check_status us "`Zero" '\x50' z
      | `Scale_mul m -> write_val ~check_status us "`Scale_mul" '\x51' m
      | `Scale_div d -> write_val ~check_status us "`Scale_div" '\x52' d

    (* See [read] above *)
    let lsread us =
      let pkg = String.create 5 in
      pkg.[0] <- '\003'; (* 2 BT bytes *)
      pkg.[1] <- '\000';
      pkg.[2] <- '\x00';
      pkg.[3] <- '\x10'; (* LSREAD *)
      pkg.[4] <- us.port;
      us.u_send us.u_fd pkg;
      let ans = us.u_recv us.u_fd 20 in
      check_status_as_exn ans.[2];
      ans (* I2C data starts at byte 4 *)

    let lswrite us addr =
      let pkg = String.create 9 in
      pkg.[0] <- '\007'; pkg.[1] <- '\000'; (* 2 BT bytes *)
      pkg.[2] <- '\x00'; (* Request answer *)
      pkg.[3] <- '\x0F'; (* LSWRITE *)
      pkg.[4] <- us.port;
      pkg.[5] <- '\002'; (* tx bytes (2 bytes sent) *)
      pkg.[6] <- '\001'; (* rx bytes (1 bytes to read) *)
      pkg.[7] <- '\x02'; (* 1st byte of command: I2C dev *)
      pkg.[8] <- addr;
      (* 'Restart Messaging + 0x03', is sent by the brick itself. *)
      us.u_send us.u_fd pkg;
      let ans = us.u_recv us.u_fd 3 in
      check_status_as_exn ans.[2]

    let data_ready us =
      us.u_send us.u_fd us.ls_status;
      let ans = us.u_recv us.u_fd 4 in
      check_status_as_exn ans.[2];
      ans.[3] <> '\000'

    let get_state us =
      lswrite us '\x41'; (* Read command state *)
      match (lsread us).[4] with
      | '\x00' -> `Off
      | '\x01' -> `Meas
      | '\x02' -> `Meas_cont
      | '\x03' -> `Event
      | '\x04' -> `Reset
      | _ -> failwith "Mindstorm.Sensor.Ultrasonic.get_state"

    let get us var =
      (* Retry any pending garbage bytes in the NXT buffers.  FIXME:
         when is this needed?  It can even stall the program if no
         bytes are to be read!  *)
      (* ignore(lsread us); *)
      (* Retrieve the data of [var] *)
      lswrite us (match var with (* 2nd byte of command: var to read *)
                  | `Byte0 -> '\x42'
                  | `Byte1 -> '\x43'
                  | `Byte2 -> '\x44'
                  | `Byte3 -> '\x45'
                  | `Byte4 -> '\x46'
                  | `Byte5 -> '\x47'
                  | `Byte6 -> '\x48'
                  | `Byte7 -> '\x49'
                  | `Meas_interval -> '\x40'
                  | `Zero -> '\x50'
                  | `Scale_mul -> '\x51'
                  | `Scale_div -> '\x52'
                 );
      (* Check the status of I2C message channel until idle, timeout or
         an error occurs. FIXME: until? needed? *)
(*    if not(data_ready us) then failwith "Mindstorm.Sensor.Ultrasonic.get"; *)
      (* Read sensor data *)
      let data = lsread us in
      Char.code data.[4]

  end
end

module Sound =
struct
  let play ?check_status conn ?(loop=false) fname =
    let check_status = default_check_status conn check_status in
    cmd conn ~check_status ~byte1:'\x02' ~n:23 (fun pkg ->
      pkg.[4] <- if loop then '\x01' else '\x00';
      blit_filename "Mindstorm.Sound.play" fname pkg 5
    )

  let stop ?check_status conn =
    let check_status = default_check_status conn check_status in
    cmd conn ~check_status ~byte1:'\x0C' ~n:2 (fun _ -> ())

  let play_tone ?check_status conn freq duration =
    let check_status = default_check_status conn check_status in
    if freq < 200 || freq > 14000 then
      invalid_arg "Mindstorm.Sound.play_tone: frequency not in 200 .. 14000";
    cmd conn ~check_status ~byte1:'\x03' ~n:6 (fun pkg ->
      copy_uint16 freq pkg 4;
      copy_uint16 duration pkg 6
    )
end

module Message =
struct
  type mailbox = [`B0 | `B1 | `B2 | `B3 | `B4 | `B5 | `B6 | `B7 | `B8 | `B9]
  type remote = [`R0 | `R1 | `R2 | `R3 | `R4 | `R5 | `R6 | `R7 | `R8 | `R9]

  let char_of_mailbox = function
    | `B0 -> '\000' | `B1 -> '\001' | `B2 -> '\002'
    | `B3 -> '\003' | `B4 -> '\004' | `B5 -> '\005'
    | `B6 -> '\006' | `B7 -> '\007' | `B8 -> '\008'
    | `B9 -> '\009'
    | `R0 -> '\010' | `R1 -> '\011' | `R2 -> '\012'
    | `R3 -> '\013' | `R4 -> '\014' | `R5 -> '\015'
    | `R6 -> '\016' | `R7 -> '\017' | `R8 -> '\018'
    | `R9 -> '\019'

  let write ?(check_status=true) conn mailbox msg =
    let len = String.length msg in
    if len > 58 then invalid_arg "Mindstorm.Message.write: message length > 58";
    let pkg = String.create (len + 7) in
    copy_uint16 (len + 5) pkg 0; (* cmd length = 4 + msg length + one '\000' *)
    pkg.[2] <- if check_status then '\x00' else '\x80';
    pkg.[3] <- '\x09';
    pkg.[4] <- char_of_mailbox mailbox;
    pkg.[5] <- Char.unsafe_chr len;
    String.blit msg 0 pkg 6 len;
    pkg.[len+6] <- '\000';
    conn.send conn.fd pkg;
    if check_status then ignore(recv conn 3)

  let read conn ?(remove=false) mailbox =
    let pkg = String.create 7 in
    pkg.[0] <- '\005'; (* 2 bluetooth bytes *)
    pkg.[1] <- '\000';
    pkg.[2] <- '\x00'; (* request answer *)
    pkg.[3] <- '\x13'; (* MESSAGEREAD *)
    pkg.[4] <- char_of_mailbox mailbox; (* remote inbox *)
    pkg.[5] <- '\000'; (* local inbox; unused.  FIXME: normal? *)
    pkg.[6] <- if remove then '\x01' else '\x00';
    conn.send conn.fd pkg;
    let ans = recv conn 64 in
    let len = try String.index_from ans 5 '\000' - 5 with Not_found -> 59 in
    String.sub ans 5 len
end

