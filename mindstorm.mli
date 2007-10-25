(* File: mindstorm.mli

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

type 'a conn
    (** Abstract type representing a connection to a LEGO® mindstorm
        brick.  The type parameter indicates whether this connection
        is a USB or a bluetooth one. *)

val connect_usb : string -> usb conn
val connect_bluetooth : string -> bluetooth conn


(** {1 Error exception} *)

type error =
    | No_more_handles
    | No_space
    | No_more_files
    | EOF_expected
    | EOF
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
    | Illegal_handle

exception Error of error


(* ---------------------------------------------------------------------- *)
(** {1 Direct commands} *)

type command_error =
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
    | Bad_size (** Illegal size specified *)
    | Bad_mailbox (** Illegal mailbox queue ID specified *)
    | Bad_field (** Attempted to access invalid field of a structure *)
(*     | Bad_io (\** Bad input or output specified *\) *)
    | Out_of_memory (** Insufficient memory available *)
    | Bad_arg (** Bad arguments *)

exception Command of command_error
  (** This exception can be raised by any of the functions below
      except when the optional argument ~check_error is set to false.
      Note that checking for errors leads to up to approximately a
      60ms latency between two commands.  *)

(** Starting and stopping programs on the brick. *)
module Program :
sig
  val start : 'a conn -> string -> unit
    (** [start_program conn pgm] starts the program named [pgm]. *)
  val stop : 'a conn -> unit
    (** [stop_program conn] stops the currently running program if any. *)
  val name : 'a conn -> string
    (** Return the name of the current program or [""] if none. *)
end


(** {2 Output ports} *)
module Motor :
sig
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

  val set : 'a conn -> t -> state -> unit

  val get : 'a conn -> t -> state * int * int * int

  val reset_pos : 'a conn -> t -> unit
end

(** {2 Input ports} *)
module Sensor :
sig
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

  val set : 'a conn -> t -> sensor_type -> sensor_mode -> unit

  val get : 'a conn -> t -> sensor_type * sensor_mode

  (** {3 Low speed} *)

  val get_status : 'a conn -> t -> int
  val write : 'a conn -> t -> string -> unit (* Rx??? *)
  val read : 'a conn -> t -> string
end

(** {2 Sounds} *)
module Sound :
sig
  val play : 'a conn -> ?loop:bool -> string -> unit
    (** [play_soundfile conn file] *)
  val stop : 'a conn -> unit
    (** Stop the current playback.  Does nothing if no sound file is
        playing. *)
  val play_tone : 'a conn -> int -> int -> unit
    (** [play_tone conn freq duration] *)
end

val message_write : 'a conn -> int -> string -> unit
val message_read : 'a conn -> ?remove:bool -> int -> string
val battery_level : 'a conn -> int
