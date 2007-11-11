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


(* Specialised implementation for speed *)
let min i j = if (i:int) < j then i else j


type error =
    | No_more_handles
    | No_space
    | No_more_files
    | EOF_expected
(*     | EOF *)
    | Not_a_linear_file
(*     | File_not_found *)
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

exception File_not_found

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
  e.(0x87) <- File_not_found;
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
    if r < n then (
      (* The doc says 60ms are needed to switch from receive to
         transmit mode. *)
      ignore(Unix.select [fd] [] [] 0.060); (* FIXME: harmful on windows? *)
      loop fd buf (i + r) (n - r)
    ) in
  fun fd buf ofs n -> loop fd buf ofs n

let really_read fd n =
  let buf = String.create n in
  really_input fd buf 0 n;
  buf

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding integer. *)
let int16 s i =
  assert(i + 1 < String.length s);
  Char.code s.[i] lor (Char.code s.[i+1] lsl 8)

let copy_int16 i s ofs =
  assert(ofs + 1 < String.length s);
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  s.[ofs + 1] <- Char.unsafe_chr((i lsr 8) land 0xFF) (* MSB *)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding int.  Since OCaml int are 31 bits (on a 32 platform),
   raise an exn if the last bit is set. *)
let int32 s i =
  if s.[i + 3] >= '\x80' then failwith "Mindstorm.int32: int overflow";
  Char.code s.[i]
  lor (Char.code s.[i + 1] lsl 8)
  lor (Char.code s.[i + 2] lsl 16)
  lor (Char.code s.[i + 3] lsl 32)

(* Copy the int [i] as 4 bytes (little endian) to [s] starting at
   position [ofs].   We assume [i < 2^32].*)
let copy_int32 i s ofs =
  s.[ofs] <- Char.unsafe_chr(i land 0xFF); (* LSB *)
  let i = i lsr 8 in
  s.[ofs + 1] <- Char.unsafe_chr(i land 0xFF);
  let i = i lsr 8 in
  s.[ofs + 2] <- Char.unsafe_chr(i land 0xFF);
  s.[ofs + 3] <- Char.unsafe_chr((i lsr 8) land 0xFF) (* MSB *)

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
       needed. *)
    String.fill pkg (ofs + len) (20 - len) '\000'


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
     length are also read but not returned (and not counted in [n]).
     [recv] checks the status byte and raise an exception accordingly
     (if needed). *)
}

let close conn =
  Unix.close conn.fd


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

let bt_send fd pkg = ignore(Unix.write fd pkg 0 (String.length pkg))

let bt_recv fd n =
  let size = really_read fd 2 in
  assert(int16 size 0 = n);
  usb_recv fd n
;;

IFDEF MACOSX THEN
(* Mac OSX *)
let connect_bluetooth tty =
  let fd = Unix.openfile tty [Unix.O_RDWR] 0o660 in
  { fd = fd;  send = bt_send;  recv = bt_recv }

ELSE
(* Windows and Unix *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

let connect_bluetooth addr =
  let fd = socket_bluetooth addr in
  { fd = fd;  send = bt_send;  recv = bt_recv }

ENDIF


(* ---------------------------------------------------------------------- *)
(** System commands *)

type in_channel = {
  in_fd : Unix.file_descr;
  in_send : Unix.file_descr -> string -> unit;
  in_recv : Unix.file_descr -> int -> string;
  in_handle : char; (* the handle given by the brick *)
  in_length : int; (* file size *)
  mutable in_closed : bool;
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
    in_closed = false;
  }

let in_channel_length ch =
  if ch.in_closed then raise(Sys_error "Closed NXT in_channel");
  ch.in_length

let close_in ch =
  if not ch.in_closed then begin
    let pkg = String.create 5 in
    pkg.[0] <- '\003'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x84'; (* CLOSE *)
    pkg.[4] <- ch.in_handle;
    ch.in_send ch.in_fd pkg;
    ignore(ch.in_recv ch.in_fd 4); (* check status *)
    ch.in_closed <- true;
  end

let input ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFF then
    invalid_arg "Mindstorm.input";
  if ch.in_closed then raise(Sys_error "Closed NXT in_channel");
  let pkg = String.create 7 in
  pkg.[0] <- '\005'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x82'; (* READ *)
  pkg.[4] <- ch.in_handle;
  copy_int16 len pkg 5;
  ch.in_send ch.in_fd pkg;
  (* Variable length return package *)
  let ans = ch.in_recv ch.in_fd 6 in
  let r = int16 ans 4 in (* # bytes read *)
  assert(ofs + r <= len);
  really_input ch.in_fd buf ofs len;
  r


type out_channel = {
  out_fd : Unix.file_descr;
  out_send : Unix.file_descr -> string -> unit;
  out_recv : Unix.file_descr -> int -> string;
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
  pkg.[3] <- flag_byte;
  blit_filename "Mindstorm.open_out" fname pkg 4;
  copy_int32 length pkg 24;
  conn.send conn.fd pkg;
  let ans = conn.recv conn.fd 4 in
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
  pkg.[3] <- '\x8C'; (* APPEND DATA *)
  blit_filename "Mindstorm.open_out" fname pkg 4;
  conn.send conn.fd pkg;
  let ans = conn.recv conn.fd 8 in
  { out_fd = conn.fd;
    out_send = conn.send;
    out_recv = conn.recv;
    out_handle = ans.[3];
    out_length = int32 ans 4;
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
    ignore(ch.out_recv ch.out_fd 4); (* check status *)
    ch.out_closed <- true;
  end

let output ch buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length buf || len > 0xFFFC then
    invalid_arg "Mindstorm.output";
  if ch.out_closed then raise(Sys_error "Closed NXT out_channel");
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


let remove conn fname =
  let pkg = String.create 24 in
  pkg.[0] <- '\026'; (* size, LSB *)
  pkg.[1] <- '\000'; (* size, MSB *)
  pkg.[2] <- '\x01';
  pkg.[3] <- '\x85'; (* DELETE *)
  blit_filename "Mindstorm.remove" fname pkg 4;
  ignore(conn.recv conn.fd 22) (* check status *)


module Find =
struct
  type iterator = {
    it_fd : Unix.file_descr;
    it_send : Unix.file_descr -> string -> unit;
    it_recv : Unix.file_descr -> int -> string;
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
      ignore(it.it_recv it.it_fd 4); (* check status *)
      it.it_closed <- true
    end

  let patt conn fpatt =
    let pkg = String.create 24 in
    pkg.[0] <- '\022'; (* size, LSB *)
    pkg.[1] <- '\000'; (* size, MSB *)
    pkg.[2] <- '\x01';
    pkg.[3] <- '\x86'; (* FIND FIRST *)
    blit_filename "Mindstorm.find" fpatt pkg 4;
    conn.send conn.fd pkg;
    let ans = conn.recv conn.fd 28 in (* might raise File_not_found *)
    { it_fd = conn.fd;
      it_send = conn.send;
      it_recv = conn.recv;
      it_handle = ans.[3];
      it_closed = false;
      it_fname = get_filename ans 4;
      it_flength = int32 ans 24;
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
    try
      let ans = i.it_recv i.it_fd 28 in
      i.it_fname <- get_filename ans 4;
      i.it_flength <- int32 ans 24
    with File_not_found ->
      (* In the case File_not_found is raised, the doc says the handle
         is closed by the brick (FIXME: confirm?) *)
      i.it_closed <- true;
      raise File_not_found

  let iter conn ~f fpatt =
    match (try Some(patt conn fpatt) with File_not_found -> None) with
    | None -> ()
    | Some i ->
        try
          while true do
            f i.it_fname i.it_flength;
            next i;
          done
        with
        | File_not_found -> ()
        | e -> close i; raise e (* exn raised by [f] must close the iterator *)

  let fold conn ~f fpatt a0 =
    match (try Some(patt conn fpatt) with File_not_found -> None) with
    | None -> a0
    | Some i ->
        let a = ref a0 in
        try
          while true do
            a := f i.it_fname i.it_flength !a;
            next i;
          done;
          assert false
        with
        | File_not_found -> !a
        | e -> close i; raise e

end

(* ---------------------------------------------------------------------- *)
(** Brick info *)

let firmware_version conn =
  failwith "TBD"

let boot conn =
  failwith "TBD"

let set_brick_name ?(check_status=false) conn name =
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
  if check_status then ignore(conn.recv conn.fd 3)


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
  let ans = conn.recv conn.fd 33 in
  { brick_name = get_brick_name ans 3 17; (* 14 chars + null *)
    bluetooth_addr = (* ans.[18 .. 24], drop null terminator *)
      string_of_bluetooth_addr(String.sub ans 18 6);
    signal_strength = int32 ans 25;
    free_user_flash = int32 ans 29;
  }

let delete_user_flash conn =
  failwith "TBD"

let bluetooth_reset conn =
  failwith "TBD"

let poll_length conn buf =
  failwith "TBD"

let poll_command conn buf len =
  failwith "TBD"


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
  if check_status then ignore(conn.recv conn.fd 3)


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
  type port = char
  let a = '\x00'
  let b = '\x01'
  let c = '\x02'
  let all = '\xFF'

  type mode = [ `Motor_on | `Brake | `Regulate of regulation ]
      (** [Regulate]: Enables active power regulation according to
          value of REG_MODE (interactive motors only).  You must use
          the REGULATED bit in conjunction with the REG_MODE property
          => [regulation] was made a param of [Regulate] *)
  and regulation = [ `Idle | `Motor_speed | `Motor_sync ]
  type run_state = [ `Idle | `Ramp_up | `Running | `Ramp_down ]

  type state = {
    power : int;
    mode : mode list;
    regulation : regulation;
    turn_ratio : int;
    run_state : run_state;
    tacho_limit : int;
  }

  let state = {
    power = 0;   mode = [];   regulation = `Idle;
    turn_ratio = 0;   run_state = `Idle;  tacho_limit = 0 (* run forever *)
  }


  (* If [ml = []], then motors are in COAST mode (0x00). *)
  let chars_of_mode_list ml =
    let update (mode, reg) (m: mode) = match m with
      | `Motor_on ->    mode lor 0x01, reg
      | `Brake ->       mode lor 0x02, reg
      | `Regulate r ->
          let reg = match r with
            | `Idle -> '\x00' | `Motor_speed -> '\x01' | `Motor_sync -> '\x02' in
          mode lor 0x04, reg
    in
    let mode, reg = List.fold_left update (0x00, '\x00') ml in
    (Char.unsafe_chr mode, reg)

  let set conn ?(check_status=false) port st =
    if st.power < -100 || st.power > 100 then
      invalid_arg "Mindstorm.Motor.set: state.power not in [-100, 100]";
    if st.turn_ratio < -100 || st.turn_ratio > 100 then
      invalid_arg "Mindstorm.Motor.set: state.turn_ratio not in [-100, 100]";
    if st.tacho_limit < 0 then
      invalid_arg "Mindstorm.Motor.set: state.tacho_limit must be >= 0";
    (* SETOUTPUTSTATE *)
    cmd conn ~check_status ~byte1:'\x04' ~n:12 (fun pkg ->
      pkg.[4] <- port;
      pkg.[5] <- Char.unsafe_chr(127 + st.power);
      let mode, regulation = chars_of_mode_list st.mode in
      pkg.[6] <- mode;
      pkg.[7] <- regulation;
      pkg.[8] <- Char.unsafe_chr(127 + st.turn_ratio);
      pkg.[9] <-
        (match st.run_state with
        | `Idle -> '\x00' | `Ramp_up -> '\x10'
        | `Running -> '\x20' | `Ramp_down -> '\x40');
      copy_int32 st.tacho_limit pkg 10;
    )

  let get motor =
    assert false

  let reset_pos conn ?(check_status=false) ?(relative=false) port =
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
      | `Highspeed ] (* aka NO_OF_SENSOR_TYPES *)
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
    failwith "TBD"

  let get conn num =
    failwith "TBD"

  let touch conn num =
    failwith "TBD"

  let sound conn num =
    failwith "TBD"

  let light conn num =
    failwith "TBD"

  let ultrasonic conn num =
    failwith "TBD"


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
  let play ?(check_status=false) conn ?(loop=false) fname =
    cmd conn ~check_status ~byte1:'\x02' ~n:23 (fun pkg ->
      pkg.[4] <- if loop then '\x01' else '\x00';
      blit_filename "Mindstorm.Sound.play" fname pkg 5
    )

  let stop ?(check_status=false) conn =
    cmd conn ~check_status ~byte1:'\x0C' ~n:2 (fun _ -> ())

  let play_tone ?(check_status=false) conn freq duration =
    if freq < 200 || freq > 14000 then
      invalid_arg "Mindstorm.Sound.play_tone: frequency not in [200, 14000]";
    cmd conn ~check_status ~byte1:'\x03' ~n:6 (fun pkg ->
      copy_int16 freq pkg 4;
      copy_int16 duration pkg 6
    )
end

module Message =
struct
  let write conn mailbox msg =
    ()

  let read conn ?(remove=false) mailbox =
    "" 
end


let keep_alive conn =
  conn.send conn.fd "\002\000\x00\x0D";
  let ans = conn.recv conn.fd 7 in
  int32 ans 3


let battery_level conn =
  conn.send conn.fd "\002\000\x00\x0B";
  let ans = conn.recv conn.fd 5 in
  int16 ans 3
