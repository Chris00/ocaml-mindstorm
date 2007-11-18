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
    done through bluetooth (and possibly eventually USB).
*)

type usb
type bluetooth

type 'a conn
    (** Abstract type representing a connection to a LEGO� mindstorm
        brick.  The type parameter indicates whether this connection
        is a USB or a bluetooth one. *)

val connect_bluetooth : string -> bluetooth conn
  (** [connect_bluetooth bdaddr] connects through bluetooth to the
      brick with bluetooth address [bdaddr].

      @raise Unix.Unix_error in case of a connection problem.  In
      particular, [Unix.Unix_error(Unix.EHOSTDOWN, _,_)] is raised if
      the brick is not turned on.  *)

val connect_usb : string -> usb conn

val close : 'a conn -> unit
  (** [close conn] closes the connection [conn] to the brick. *)


(** {2 Exception for errors} *)

type error =
    | No_more_handles (** All 16 handles are in use. *)
    | No_space
    | No_more_files
    | EOF_expected
    | Not_a_linear_file
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
    | Bad_io (** Bad input or output specified *)
    | Out_of_memory (** Insufficient memory available *)
    | Bad_arg (** Bad arguments *)


exception Error of error
  (** This exception can be raised by any of the functions below
      except when the optional argument [~check_error] is set to
      false.  Note that checking for errors leads to up to
      approximately a 60ms latency between two commands.  *)

exception File_not_found
  (** Raised to indicate that a file is not present on the brick. *)


(* ---------------------------------------------------------------------- *)
(** {2 Direct commands} *)

(** Starting and stopping programs (.rxe files) on the brick. *)
module Program :
sig
  val start : ?check_status:bool -> 'a conn -> string -> unit
    (** [start_program conn pgm] starts the program named [pgm].

        @param check_status whether to check the status returned by
        the brick.  Default: [false]. *)
  val stop : ?check_status:bool -> 'a conn -> unit
    (** [stop_program conn] stops the currently running program if any.
        If no program is running and [check_status=true], the exception
        {!Mindstorm.Error}[(No_program)] is raised.

        @param check_status whether to check the status returned by
        the brick.  Default: [false]. *)
  val name : 'a conn -> string
    (** Return the name of the current program or raise
        {!Mindstorm.Error}[(No_program)] if no program is running. *)
end


(** Output ports. *)
module Motor :
sig
  type port
      (** The three motor ports (immutable).  For more readability of
          your program, it is recommended you give appropriate aliases
          to the ports of interest at the beginning of your program,
          e.g. [let motor_right = Mindstorm.Motor.a]. *)
  val a : port (** The motor port A. *)
  val b : port (** The motor port B. *)
  val c : port (** The motor port C. *)
  val all : port (** Special value representing all 3 ports. *)

  type regulation = [ `Idle | `Motor_speed | `Motor_sync ]
      (** Regulation mode.
          - [`Idle]: No regulation will be enabled.
          - [`Motor_speed]: enable power control: auto adjust PWM duty
          cycle if motor is affected by physical load.
          - [`Motor_sync]: enable synchronization: attempt to keep
          rotation in sync with another motor.  You typically use this
          mode is to maintain a straight path for a vehicle robot
          automatically.  You also can use this mode with the
          {!Mindstorm.Motor.state} [turn_ratio] property to provide
          proportional turning.  You must set [`Motor_sync] on at
          least two motor ports to have the desired affect.  If it is
          set on all three motor ports, only the first two (A and B)
          are synchronized.  . *)
  type run_state = [ `Idle | `Ramp_up | `Running | `Ramp_down ]
      (** Specifies how to perform the transition to the new [speed]
          given in {!Mindstorm.Motor.state}.

          - [`Idle]: The motor is not run.
          - [`Running] enables power to any output device connected to
          the specified port(s).
          - [`Ramp_up] enables automatic ramping to a new speed
          set-point that is greater than the current speed set-point.
          When you use [`Ramp_up] in conjunction with appropriate
          [tach_limit] and [speed] values, the NXT firmware attempts
          to increase the actual power smoothly to the [speed]
          set-point over the number of degrees specified by
          [tach_limit].
          - [`Ramp_down] enables automatic ramping to a new speed
          set-point that is less than the current speed set-point.
          When you use [`Ramp_down] in conjunction with appropriate
          [tach_limit] and [speed] values, the NXT firmware attempts
          to smoothly decrease the actual power to the [speed]
          set-point over the number of degrees specified by
          [tach_limit].  *)

  type state = {
    speed : int; (** Power set-point.  Range: -100 .. 100. *)
    motor_on : bool; (** if [true], turns the motor on: enables
                         pulse-width modulation (PWM) power according
                         to speed. *)
    brake : bool; (** Use run/brake instead of run/float.  "Braking"
                      in this sense means that the output voltage is not
                      allowed to float between active PWM pulses.
                      Electronic braking improves the accuracy of
                      motor output, but uses slightly more battery
                      power.  *)
    regulation : regulation; (** Turns on the chosen regulation. *)
    turn_ratio : int; (** Range: -100 .. 100.
                          See {!Mindstorm.Motor.regulation}. *)
    run_state : run_state; (** See {!Mindstorm.Motor.run_state}. *)
    tach_limit : int; (** Number of degrees to rotate; [0]: run forever.
                          Range: 0 .. 4294967295 (unsigned 32 bits).
                          See also {!Mindstorm.Motor.run_state}. *)
  }
      (** The absolute value of [speed] is used as a percentage of the
          full power capabilities of the motor.  The sign of [speed]
          specifies rotation direction.  You must set some other
          properties appropriately for the [speed] set-point to have the
          desired effect:
          - [motor_on] must be [true].
          - [run_state] must be set to a non-[`Idle] value.

          If [not motor_on && not brake], motors are in COAST mode:
          motors connected to the specified port(s) will rotate
          freely.  *)


  val set : ?check_status:bool -> 'a conn -> port -> state -> unit
    (** [set conn p st] sets the state of the motor connected to the
        port [p] to [st].

        @param check_status whether to check the status returned by
        the brick.  Default: [false]. *)

  val get : 'a conn -> port -> state * int * int * int
    (** [get conn p] returns [(state, tach_count, block_tach_count,
        rotation_count)] where
        - [state] is the current state of the motors;
        - [tach_count] is the number of counts since the last reset of the
        motor counter (the reset occurs when a new [tach_limit] is set);
        - [block_tach_count] is the current position relative to the
        last programmed movement.
        - [rotation_count] is the current position relative to the last
        reset of the rotation sensor for motor [p].
    *)

  val reset_pos : ?check_status:bool -> 'a conn -> ?relative:bool -> port -> unit
    (** [reset_pos conn p] resets the position of the motor connected
        to port [p].

        @param relative if [true], relative to the last movement,
        otherwise absolute position.  Default: [false].

        @param check_status whether to check the status returned by
        the brick.  Default: [false].  *)
end


(** Input ports.

    The NXT brick also accepts the sensors for the previous version of
    the mindstorm brick, called RCX, so several options refer to RCX.
 *)
module Sensor :
sig
  type t
  type port = [ `S1 | `S2 | `S3 | `S4 ]
      (** The four sensor ports, labeled 1 to 4 on the brick.  It is
          recommended you give meaningful names to values of type
          [port] through let bindings.  *)

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
      | `Highspeed ]
  (** Sensor type for a port.  The sensor type primarily affects
      scaling factors used to calculate the normalized sensor value
      [`Raw], but some values have other side effects.

      - [`No_sensor]:      No sensor configured
      - [`Switch]:	  NXT or RCX touch sensor
      - [`Temperature]:    RCX temperature sensor
      - [`Reflection]:     RCX light sensor
      - [`Angle]:          RCX rotation sensor
      - [`Light_active]:   NXT light sensor with floodlight enabled
      - [`Light_inactive]: NXT light sensor with floodlight disabled
      - [`Sound_db]:       NXT sound sensor; includes sounds too high
                          or too low for our ears.
      - [`Sound_dba]:      NXT sound sensor; focuses on sounds within
                          human hearing
      - [`Custom]:
      - [`Lowspeed]:     I2C digital sensor
      - [`Lowspeed_9v]:  I2C digital sensor, 9V power (e.g. ultrasonic) *)

  type mode =
      [ `Raw
      | `Bool
      | `Transition_cnt
      | `Period_counter
      | `Pct_full_scale
      | `Celsius
      | `Fahrenheit
      | `Angle_steps
      | `Slope_mask ]
        (** Sensor mode.

            - [`Raw]: Report scaled value equal to raw value.
            - [`Bool]: Report scaled value as 1 (TRUE) or 0 (FALSE).
              The firmware uses inverse Boolean logic to match the
              physical characteristics of NXT sensors.  Readings
              are FALSE if raw value exceeds 55% of total range;
              readings are TRUE if raw value is less than 45% of
              total range.
            - [`Transition_cnt]: Report scaled value as number of
            transitions between TRUE and FALSE.
            - [`Period_counter]: Report scaled value as number of
                            transitions from FALSE to TRUE, then back
                            to FALSE.
            - [`Pct_full_scale]: Report scaled value as percentage of full
                            scale reading for configured sensor type.
            - [`Celsius]: Scale temperature reading to degrees Celsius.
            - [`Fahrenheit]: Scale temperature reading to degrees Fahrenheit.
            - [`Angle_steps]: Report scaled value as count of ticks on
            RCX-style rotation sensor.
        *)

  (** Data read from sensors. *)
  type data = {
    sensor_type : sensor_type;
    mode : mode;
    valid : bool; (** [true] if new data value should be seen as valid *)
    raw : int; (** Raw A/D value.  Device dependent.  Range: 0 .. 1023 *)
    normalized : int; (** Normalized A/D value.  Range: 0 .. 1023 *)
    scaled : int;
    (** Scaled value.  Its range depend on the {!Mindstorm.Sensor.mode}
        chosen:
        - [`Raw]: 0 .. 1023
        - [`Boolean]: 0 or 1
        - [`Transition_cnt]: 0 .. 65535
        - [`Period_counter]: 0 .. 65535
        - [`Pct_full_scale]: 0 .. 100
        - [`Celsius]: -200 .. 700 (10th of degree Celsius)
        - [`Fahrenheit]: -400 .. 1580 (10th of degree Fahrenheit)
        - [`Angle_steps]: 0 .. 65535
    *)
  }

  val set : ?check_status:bool -> 'a conn -> port -> sensor_type -> mode -> unit
    (** [set conn p ty m] set the sensor connected to port [p] to type
        [ty] and mode [m].

        @param check_status whether to check the status returned by
        the brick.  Default: [false].  *)

  val get : 'a conn -> port ->  data
    (** [get conn p] returns the data read on port [p]. *)

  val reset_scaled : ?check_status:bool -> 'a conn -> port -> unit
    (** [reset_scaled conn port]

       @param check_status whether to check the status returned by
        the brick.  Default: [false]. *)


  (** {4 Low speed} *)
  (** Commands dealing with the I2C bus available on every sensor.
      (The port number 4 may also be high speed.) *)

  val get_status : 'a conn -> port -> int
    (** [get_status conn port] returns the number of bytes ready to be
        read. *)

  val write : ?check_status:bool ->
    'a conn -> port -> ?rx_length:int -> string -> unit
    (** [write conn port data] writes [data] to lowspeed I2C sensor
        connected to the [port].  This is the protocol (e.g. for
        talking to the ultrasonic sensor).

        @param rx_length gives the number of bytes to receive.
        Default: [0] i.e. no answer expected.

        @param check_status whether to check the status returned by
        the brick.  Default: [false].  *)
  val read : 'a conn -> port -> string
    (** Read data from from lowspeed I2C port (e.g. for receiving data
        from the ultrasonic sensor).  *)


  (** Ultrasonic sensor.  Convenience functions to interact with the
      ultrasonic sensor through the I2C protocol. *)
  module Ultrasonic :
  sig
    type t
      (** Represent an initialized ultrasonic sensor. *)

    val make : ?check_status:bool -> 'a conn -> port -> t
      (** [make conn port] initialize the sensor on port [port] as
          being an ultrasonic one. *)

    val set : ?check_status:bool -> t ->
      [ `Off | `Meas | `Meas_cont | `Event | `Reset
      | `Meas_interval of int | `Zero of int
      | `Scale_mul of int | `Scale_div of int ] -> unit
      (** [Ultrasonic.set conn port cmd]

          - [`Off]: turns the ultrasonic sensor off.
          - [`Meas]: single shot command.  In this mode, the
          ultrasonic sensor will only make a new measurement every
          time the command byte is send to the sensor.  The sensor
          will measure distances for up to 8 objects and save the
          distances within the [`Byte0] .. [`Byte7] variables.
          - [`Meas_cont] Continuous measurement command.  This is the
          default mode, where the sensor continuously makes new
          measurement with the specified interval (see
          [`Meas_interval] below).
          - [`Event]: Event capture command.  Within this mode the
          sensor will measure whether any other ultrasonic sensors are
          within the vicinity.  With this information a program can
          evaluate when it is best to make a new measurement which
          will not conflict with other ultrasonic sensors.

          - [`Meas_interval t]: set continuous measurment interval.
          - [`Zero z]: set the actual zero.
          - [`Scale_mul m]: set the actual scale factor.
          - [`Scale_div d]: set the actual scale divisor.

          - [`Reset]: resets the ultrasonic sensor.  *)

    val get : ?check_status:bool -> t ->
      [ `Byte0 | `Byte1 | `Byte2 | `Byte3 | `Byte4 | `Byte5 | `Byte6 | `Byte7
      | `State | `Meas_interval | `Zero | `Scale_mul | `Scale_div
      ] -> int
      (** [Ultrasonic.get conn port var] returns the content of the
          variable [var] on the sensor (detailed underneath).  All values
          are between 0 and 255.

          - [`Byte0] .. [`Byte7] are the 8 variables containing the
          distances measured with the [`Meas] command.

          - [`State] returns the command state (FIXME: what is it)

          - [`Meas_interval] returns the interval (in 0.01 sec)
          between two consecutive measurments when the sensor is in
          continuous measurment (command [`Meas_cont]).

          - [`Zero] returns the value of the actual zero (settable
          with the [`Zero] command).

          - [`Scale_mul] (resp.�[`Scale_div]) return the current
          scaling factor (resp.�divisor).  It is settable with the
          [`Scale_mul] (resp.�[`Scale_div]) command.  *)
  end
end


(** Play sounds (.rso files) and tones. *)
module Sound :
sig
  val play : ?check_status:bool -> 'a conn -> ?loop:bool -> string -> unit
    (** [play_soundfile conn file] plays the sound file named [file].
        The sound files extension, namely ".rso", must be part of [file].

        @param loop if [true] repeat the play indefinitely.
        Default: [false].

        @param check_status whether to check the status returned by the brick.
        Default: [false].  *)
  val stop : ?check_status:bool -> 'a conn -> unit
    (** Stop the current playback.  Does nothing if no sound file is
        playing.

        @param check_status whether to check the status returned by the brick.
        Default: [false]. *)
  val play_tone : ?check_status:bool -> 'a conn -> int -> int -> unit
    (** [play_tone conn freq duration] play a tone with [freq] Hz
        lasting [duration] miliseconds. *)
end


(** Read and write messages from the 10 message queues.  This can be
    thought as advanced direct commands.

    Interesting information concerning this can be found in Sivan
    Toledo's paper:
    {{:http://www.tau.ac.il/~stoledo/lego/btperformance.html}Analysis
    of the NXT Bluetooth-Communication Protocol}.  *)
module Message :
sig
  type mailbox = [`B0 | `B1 | `B2 | `B3 | `B4 | `B5 | `B6 | `B7 | `B8 | `B9]
      (** The 10 available mailboxes on the NXT brick. *)

  val write : ?check_status:bool -> 'a conn -> mailbox -> string -> unit
    (** [write conn box msg] writes the message [msg] to the inbox
        [box] on the NXT.  This is used to send messages to a
        currently running program.

        @param check_status whether to request a status code from the
        brick.  If [true] (the default for this fonction), the NXT
        only send a reply when it is able to queue the message without
        overflowing the queue (this prevent message loss).  If
        [false], the message may delete the oldest message in the NXT
        queue if it is full (the NXT queues are 5 messages long).  *)

  val read : 'a conn -> ?remove:bool -> mailbox -> string
    (** [read conn box] returns the message from the inbox [box] on
        the NXT.
        @param remove if true, clears the message from the remote inbox.
        Default: [false]. *)
end


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
  (** [close_in ch] closes the channel [ch].  Closing an already
      closed channel does nothing.  *)

val input : in_channel -> string -> int -> int -> int
  (** [input ch buf ofs len] reads a block of data of length [len]
      from the channel [ch] and write it to [buf] starting at position
      [ofs].

      @raise End_of_file if there is no more data to read. *)

type out_channel
    (** Handle for writing data to the brick. *)

(** The standard NXT firmware requires that executable files and icons
    are linear but all other types of files (including sound files)
    can be non-contiguous (i.e., fragmented).

    - [`File length]: Default file, the parameter is its [length].
    - [`Linear length]: Write a linear file, the parameter is its [length].
*)
type out_flag =
    [ `File of int
    | `Linear of int
    | `Data of int
    | `Append ]

val open_out : 'a conn -> out_flag -> string -> out_channel
  (** [open_out conn flag fname] opens the file [fname] for writing.
      The channel must be closed with {!Mindstorm.close_in}.  Close it
      as soon as possible as channels are a scarce resource.

      If the the file exists, [Error(File_exists,_,_)] is raised.

      @param linear Default: [false]. *)

val close_out : out_channel -> unit
  (** [close_out ch] closes the channel [ch].  Closing an already
      closed channel does nothing. *)

val output : out_channel -> string -> int -> int -> int
  (** [output ch buf ofs len] ouputs the substring [buf.[ofs
      .. ofs+len-1]] to the channel [fd].  Returns the number of bytes
      actually written.  *)

val remove : 'a conn -> string -> unit
  (** [remove conn fname] remove the file [fname] from the brick. *)

(** List files on the brick matching a given pattern. *)
module Find :
sig
  type iterator
      (** An iterator to allow to enumerate files on the brick. *)

  val patt : 'a conn -> string -> iterator
    (** [Find.patt conn fpatt] returns an iterator listing the filenames
        mathing the pattern [fpatt].  The following types of wildcards
        are accepted:
        - filename.extension
        - *.\[file type name\]
        - filename.*
        - *.*

        @raise File_not_found if no file was found *)
  val current : iterator -> string
    (** [Find.current i] returns the current filename. *)
  val current_size : iterator -> int
    (** [Find.current_size i] returns the current filename size
        (number of bytes). *)
  val next : iterator -> unit
    (** Execute a new request to the brick to retrieve the next
        filename matching the pattern.

        @raise File_not_found if no more file was found.  When this
        exception is raised, the iterator is closed. *)
  val close : iterator -> unit
    (** [close_iterator i] closes the iterator [i].  Closing an
        already closed iterator does nothing. *)

  val iter : 'a conn -> f:(string -> int -> unit) -> string -> unit
    (** [iter f fpatt] iterates [f name size] on all the filenames
        matching the pattern [fpatt] (see {!Mindstorm.Find.patt} for
        the accepted patterns).  *)
  val map : 'a conn -> f:(string -> int -> 'b) -> string -> 'b list
    (** [map f fpatt] maps [f name size] on all the filenames matching
        the pattern [fpatt] and return the list formed of those.  (See
        {!Mindstorm.Find.patt} for the accepted patterns.)  *)
  val fold : 'a conn -> f:(string -> int -> 'b -> 'b) -> string -> 'b -> 'b
    (** [fold f fpatt a0] folds [f] on all the filenames matching the
        pattern [fpatt] (see {!Mindstorm.Find.patt} for the accepted
        patterns).  *)
end


(** {3 Brick information} *)

val firmware_version : 'a conn -> int * int * int * int
  (** [firmware_version conn] returns a tuple [(p1, p0, f1, f0)] where
      [p1] is the major version of the protocol, [p0] is the minor
      version of the protocol, [f1] is the major version of the
      firmware, [f0] is the minor version of the firmware, *)

val set_brick_name : ?check_status:bool -> 'a conn -> string -> unit
  (** [set_brick_name conn name] change the name to which one is
      connected through [conn] to [name].

      @param check_status whether to check the status returned by the
      brick (and raise [Error] accordingly.  Default: [false].  *)

type brick_info = {
  brick_name : string;   (** NXT name (set with {!Mindstorm.set_brick_name}) *)
  bluetooth_addr : string; (** Bluetooth address *)
  signal_strength : int; (** Bluetooth signal strength (for some reason
                             is always 0) *)
  free_user_flash : int; (** Free user FLASH *)
}

val get_device_info : 'a conn -> brick_info
  (** [get_device_info conn] returns some informations about the brick
      connected through [conn]. *)

val keep_alive : 'a conn -> int
  (** [keep_alive conn] returns the current sleep time limit in
      milliseconds. *)

val battery_level : 'a conn -> int
  (** [battery_level conn] return the voltages in millivolts of the
      battery on the brick. *)

val delete_user_flash : 'a conn -> unit
val bluetooth_reset : usb conn -> unit

val boot : usb conn -> unit



(** {3 Polling} *)

val poll_length : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int
  (** Returns the number of bytes for a command in the low-speed
      buffer or the high-speed buffer (0 = no command is ready).  *)
val poll_command : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int
  -> int * string
  (** Reads bytes from the low-speed or high-speed buffer. *)

