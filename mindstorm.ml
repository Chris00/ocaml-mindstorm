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

type usb
type bluetooth

(* hciconfig - displays status information on your bluetooth interface
   (similar to ifconfig)

   hcitool - used to query all kinds of information about bluetooth
   devices in your piconet

   sdptool - used to create and query "service description protocol"
   bluetooth services, like SP=serial port

   rfcomm - establishes connections to remote bluetooth services or
   listens for incomming service connections
*)

(* the type parameter is because we want to distinguish usb and
   bluetooth connections as some commands are only available through USB. *)
type 'a conn = {
  is_usb : bool;
  (* We need to know whether a connection is USB or bluetooth because
     bluetooth requires a prefix of 2 bytes indicating the length of
     the packet. *)
  fd : Unix.file_descr;
}


let filename_is_valid : string -> bool =
  (** Returns true if the filename is valid according to the brick
      limitations. *)
  fun fname ->
    true


type error =
    | No_more_handles
    | No_space
    | No_more_files
    | EOF_expected
    | EOF
    | Not_a_linear_file
    | File_not_found
    | Handle_all_ready_closed
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
    | Illegal_handle

    (** command_error *)
    | Pending (** Pending communication transaction in progress *)
    | Empty_mailbox (** Specified mailbox queue is empty *)
    | Failed (** Request failed (i.e. specified file not found) *)
(*     | Unknown (\** Unknown command opcode *\) CANNOT HAPPEN*)
    | Insane (** Insane packet *)
    | Out_of_range (** Data contains out-of-range values *)
    | Bus_error (** Communication bus error *)
    | Buffer_full (** No free memory in communication buffer *)
    | Invalid_conn (** Specified channel/connection is not valid *)
    | Busy_conn (** Specified channel/connection not configured or busy *)
(*     | No_program (\** No active program *\) CANNOT HAPPEN *)
    | Bad_size (** Illegal size specified *)
    | Bad_mailbox (** Illegal mailbox queue ID specified *)
    | Bad_field (** Attempted to access invalid field of a structure *)
(*     | Bad_io (\** Bad input or output specified *\) CANNOT HAPPEN *)
    | Out_of_memory (** Insufficient memory available *)
    | Bad_arg (** Bad arguments *)

exception Error of error

let connect_usb socket =
  Unix.openfile socket [] 0


let connect_bluetooth socket =
  Unix.openfile socket [] 0


(* Generic wrapper for commands with a reply packet *)
let cmd_with_reply conn ~byte1 ~n f ~reply_n g =
  assert(n <= 63);
  let buf = String.make n '\x00' in
  buf.(1) <- byte1;
  f buf;
  ignore(Unix.write conn buf 0 n);
  (* read the anwser *)
  let ret = String.make reply_n '\000' in
  really_read conn ret 0 reply_n;
  (* Check for errors and tranform them into exceptions *)
  match Char.code(ret.(1)) with
  | 0 -> g ret
  | 0x20 -> raise 


(* ---------------------------------------------------------------------- *)
(** System commands *)





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
