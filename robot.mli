(* File: robot.mli

   Copyright (C) 2008

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


(** Module implementing a simple event loop to dialog with a mindstorm
    robot. *)


type t
  (** Mutable robot state to be used in an event loop. *)

val make : unit -> t
  (** Make a new robot (with its own event loop). *)

val run : t -> unit
  (** [run r] runs the robot [r] (i.e. starts its event loop).  To
      stop the robot, raise the exception [Exit] from an event
      callback. *)

val stop : t -> unit
  (** Stop the event loop of the robot (and turns off sensors it knows
      about). *)


(** {2 Measures} *)

type 'a meas
  (** Holds a measure of type ['a] from the robot. *)

val meas : t -> (unit -> 'a) -> 'a meas
  (** [meas r get] define a new measure for the robot [r], [get()]
      being executed when this measure is needed by [r].  If [get()]
      raises an exception, it is interpreted as failure to get the
      data.  A measure can be bound to many events; in fact, you
      should try to reuse measures as much as possible to avoid
      querying the robot several times for the same information. *)

val map : ('a -> 'b) -> 'a meas -> 'b meas
  (** [map f m] returns a new measure [m'] whose values are [f x]
      where [x] is the value of [m].  The main advantage of this is
      that querying [m'] does not require to fetch again the value
      from the robot.  *)

val touch : 'a Mindstorm.conn -> Mindstorm.Sensor.port ->
  t -> bool meas
  (** [touch conn port r] returns a measure reporting whether the
      touch sensor connected to [port] is pressed. *)

val touch_count : 'a Mindstorm.conn -> Mindstorm.Sensor.port ->
  ?transition:bool -> t -> int meas
  (** [touch_count conn port r] returns a measure counting number of
      times the touch sensor connected to [port] has been pressed.

      @param transition if [false] (the default), "pressed" means
      pushed AND released; if [true], "pressed" means pushed OR
      released. *)

val light : 'a Mindstorm.conn -> Mindstorm.Sensor.port ->
  ?on:bool -> t -> int meas
  (** [light conn port r] returns a measure reporting the intensity of
      light detected.

      @param on if [true] (the default), turns on the light on the
      sensor.  The sensor light is turned off if the robot exits
      through an exception or via {!Robot.stop}. *)

val sound : 'a Mindstorm.conn -> Mindstorm.Sensor.port ->
  ?human:bool -> t -> int meas
  (** [sound conn port r] returns a measure reporting the sound
      intensity measured by the sensor connected to [port].

      @param human if [true] focuses on sounds within human
      hearing.  Default: [false]. *)

val ultrasonic : 'a Mindstorm.conn -> Mindstorm.Sensor.port ->
  t -> int meas
  (** [ultrasonic conn port r] returns a measure reporting the
      distance measured by the ultrasonic sensor connected to [port].
      The sensor is turned off if the robot exits through an exception
      or via {!Robot.stop}. *)

val always : t -> bool meas
  (** A callback bound to this measure will always be executed,
      regardless of the condition given in {!Robot.event}.  To be
      useful, the execution of the callback will not erase other
      events.  Thus the callback will be executed once for every
      "event loop".  This can be used, for example, to repeatedly
      collect data until another event takes place.  *)

val read : ?retry:int -> 'a meas -> 'a option
  (** [read m] returns the current (up to date) value of the measure
      [m].  If the reading fails after [retry] times, return
      [None].

      @param retry number of times to retry if a reading fails.
      [0] means not to rety -- hence try only once to read the value.
      Default: [3]. *)


(** {2 Events} *)

val event : ?retry:int ->
  'a meas -> ('a option -> bool) -> ('a option -> unit) -> unit
  (** [event m cond f] schedules [f v] to be executed when the value
      [v] of the measure [m] satisfies [cond v].  The events are tried
      in the order they are registered.  This first condition that is
      [true] erases all other events and executes its associated
      callback (with the exception of the {!Robot.always} measures --
      and its derived measures with [map]).

      @param retry number of times to retry before returning [None] to
      [cond] and [f] to indicate an unavailable value.  *)

val event_is : ?retry:int -> bool meas -> (unit -> unit) -> unit
  (** [event_is m f] is a useful shortcut that triggers the action [f]
      if the value of [m] is [Some true] and does noting otherwise
      (i.e. when the value is [Some false] or [None]).

      @param retry number of times to retry before returning [None] to
      [cond] and [f] to indicate an unavailable value. *)
