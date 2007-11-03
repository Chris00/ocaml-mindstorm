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


(** OCaml-mindstorm is a library that enables you to drive Lego
    mindsotrm NXT bricks from OCaml. Communication with the NXT brick is
    done through bluetooth (and possibly eventually USB).  *)

type usb
type bluetooth

type 'a conn
    (** Abstract type representing a connection to a LEGO® mindstorm
        brick.  The type parameter indicates whether this connection
        is a USB or a bluetooth one. *)

val connect_bluetooth : ?retry:int -> string -> bluetooth conn
  (** [connect_bluetooth bdaddr] connects through bluetooth to the
      brick with bluetooth address [bdaddr].  *)

val connect_usb : string -> usb conn


(** {2 Exception for errors} *)

type error =
    | No_more_handles
    | No_space
    | No_more_files
    | EOF_expected
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

    (** Command error *)
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
(*     | Bad_io (\** Bad input or output specified *\) *)
    | Out_of_memory (** Insufficient memory available *)
    | Bad_arg (** Bad arguments *)


exception Error of error
  (** This exception can be raised by any of the functions below
      except when the optional argument [~check_error] is set to
      false.  Note that checking for errors leads to up to
      approximately a 60ms latency between two commands.  *)


(* ---------------------------------------------------------------------- *)
(** {2 System commands} *)

(** {3 Files} *)

type in_channel
    (** Handle for reading from the brick. *)

val open_in : 'a conn -> string -> in_channel
  (** [open_in conn fname] opens the file named [fname] on the brick
      for reading.  The channel must be closed with
      {!Mindstorm.close_in}.  Close it as soon as possible as channels
      are a scarce resource.

      @raise Invalid_argument if [fname] is not a ASCIIZ string with
      maximum 15.3 characters.  *)

val in_channel_length : in_channel -> int
  (** [in_channel_length ch] returns the length of the channel [ch]. *)

val close_in : in_channel -> unit
  (** [close_in ch] closes the channel [ch]. *)

val input : in_channel -> string -> int -> int -> int
  (** [input ch buf ofs len] reads a block of data of length [len]
      from the channel [ch] and write it to [buf] starting at position
      [ofs].

      @raise End_of_file if there is no more data to read. *)

type out_channel
    (** Handle for writing data to the brick. *)

(** The standard NXT firmware requires that executable files and icons
    are linear but all other types of files (including sound files)
    can be non-contiguous (i.e., fragmented).  *)
type out_flag =
    [ `File of int (** Default file, the parameter is its length. *)
    | `Linear of int (** Write a linear file, the parameter is its length. *)
    | `Data of int
    | `Append
    ]

val open_out : 'a conn -> out_flag -> string -> out_channel
  (** [open_out conn flag fname] opens the file [fname] for writing.
      The channel must be closed with {!Mindstorm.close_in}.  Close it
      as soon as possible as channels are a scarce resource.

      If the the file exists, [Error(File_exists,...,...)] is raised.

      @param linear Default: [false]. *)

val close_out : out_channel -> unit
  (** [close_out ch] closes the channel [ch]. *)

val output : out_channel -> string -> int -> int -> int
  (** [output ch buf ofs len] ouputs the substring [buf.[ofs
      .. ofs+len-1]] to the channel [fd].  Returns the number of bytes
      actually written.  *)

val remove : 'a conn -> string -> unit
  (** [remove conn fname] remove the file [fname] from the brick. *)


type file_iterator

val find : 'a conn -> string -> file_iterator

val filename : file_iterator -> string
val size : file_iterator -> int
val next : file_iterator -> unit
val close_iterator : file_iterator -> unit


(** {3 Brick information} *)

val firmware_version : 'a conn -> int * int * int * int
val boot : usb conn -> unit
val set_brick_name : 'a conn -> string -> unit

type brick_info = {
  brick_name : string;
  bluetooth_addr : string; (* ??? *)
  signal_strength : int;
  free_user_flash : int;
}
val get_device_info : 'a conn -> brick_info

val delete_user_flash : 'a conn -> unit
val bluetooth_reset : usb conn -> unit

(** {3 Polling} *)

val poll_length : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int
  (** Returns the number of bytes for a command in the low-speed
      buffer or the high-speed buffer (0 = no command is ready).  *)
val poll_command : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int
  -> int * string
  (** Reads bytes from the low-speed or high-speed buffer. *)


(* ---------------------------------------------------------------------- *)
(** {2 Direct commands} *)

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


(** {3 Output ports} *)
module Motor :
sig
  type t
  type port = [ `A | `B | `C ]

  val make : 'a conn -> port -> t

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

  val set : t -> state -> unit

  val get : t -> state * int * int * int

  val reset_pos : t -> unit
end

(** Input ports. *)
module Sensor :
sig
  type t
  type port = [ `In1 | `In2 | `In3 | `In4 ]

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

  val set : 'a conn -> port -> sensor_type -> sensor_mode -> t

  val get : 'a conn -> port -> sensor_type * sensor_mode

  (** {4 Low speed} *)
  (** Commands dealing with the I2C bus available on every sensor.
      (The port 4 may also be high speed.) *)

  val get_status : 'a conn -> port -> int
  val write : 'a conn -> port -> string -> unit (* Rx??? *)
  val read : 'a conn -> port -> string
end

(** Play sounds. *)
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

(** Read and write messages from the 10 message queues.  This can be
    thought as advanced direct commands.  *)
module Message :
sig
  val write : 'a conn -> int -> string -> unit
  val read : 'a conn -> ?remove:bool -> int -> string
end

val battery_level : 'a conn -> int
  (** [battery_level conn] return the voltages in millivolts of the
      battery on the brick. *)
