(* File: robot.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

     Julie de Pril <julie_87@users.sourceforge.net>
     Dany Maslowski <dan_86@users.sourceforge.net>
     Marc Ducobu <el_marcu@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf

type meas_common = {
  robot : t; (* robot to which the measure is associated *)
  get_value : unit -> unit; (* how to get a new value *)
  mutable is_needed : bool; (* iff is bound to >= 1 event (for the
                               current loop).  if [true], the value
                               must be fetched by the event loop. *)
  mutable is_up_to_date : bool; (* updated by the latest event loop
                                   reading or by "read". *)
  is_constant : bool; (* the measure is constant ; thus also executing a
                         callback bound to it does NOT erase events. *)
}
and t = {
  mutable meas : meas_common list;
  (* Measures associated to the robot (see [meas] below).  These
     measures will need updating if their value is requested (because,
     for example, they are bound to an event). *)
  mutable events : (unit -> bool) list;
  (* conditions-callbacks to execute (see [event] below). *)
  mutable at_exit : (unit -> unit) list;
}

(* The value has to be cached in [meas] because the robot (of type
   [t]) needs a uniform access to all data fetching. *)
type 'a meas = {
  common : meas_common;
  mutable value : 'a option; (* The fetched value, if any *)
}


let make () = { meas = [];  events = [];  at_exit = [] }

let stop r =
  raise Exit (* quit the event loop and turn off sensors -- see [run]. *)

(* Declaring & reading measures
 ***********************************************************************)

(* New measure *)
let meas r get =
  let rec meas = { value = None; (* no value yet *)
                   common = meas_common }
  and meas_common = { robot = r;
                      get_value = get_value;
                      is_needed = false;
                      is_up_to_date = false;
                      is_constant = false;
                    }
  and get_value() =
    meas.value <- Some(get());
    meas_common.is_up_to_date <- true in
  r.meas <- meas_common :: r.meas;
  meas

(* Read the value on demand. *)
let single_read m =
  try
    let c = m.common in
    if not c.is_up_to_date then c.get_value(); (* => up to date *)
    m.value                                    (* may be None if unavailable *)
  with _ ->
    None
;;

let map f m =
  let c = m.common in
  let r = c.robot in
  let rec meas = { value = None;
                   common = meas_common }
  and meas_common = { robot = r;
                      get_value = get_value;
                      is_needed = false;
                      is_up_to_date = false;
                      is_constant = c.is_constant;
                    }
  and get_value() =
    (* Fetch the value of [m] and transform it. *)
    meas.value <- (match single_read m with
                   | Some v -> Some(f v)
                   | None -> None);
    meas_common.is_up_to_date <- true  in
  if not c.is_constant then r.meas <- meas_common :: r.meas;
  meas
;;

let rec read_loop retry m =
  match single_read m with
  | Some _ as v -> v
  | None -> if retry > 0 then read_loop (retry - 1) m else None

let read ?(retry=3) m = read_loop retry m


let touch conn port r =
  Mindstorm.Sensor.set conn port `Switch `Bool; (* Transition_cnt?? *)
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled = 1)

let touch_count conn port ?(transition=false) r =
  let mode = if transition then `Transition_cnt else `Period_counter in
  Mindstorm.Sensor.set conn port `Switch mode;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let light conn port ?(on=true) r =
  let ty = if on then `Light_active else `Light_inactive in
  Mindstorm.Sensor.set conn port ty `Pct_full_scale;
  let turn_off() = Mindstorm.Sensor.set conn port `No_sensor `Raw in
  r.at_exit <- turn_off :: r.at_exit;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let sound conn port ?(human=false) r =
  let ty = if human then `Sound_dba else `Sound_db in
  Mindstorm.Sensor.set conn port ty `Pct_full_scale;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let ultrasonic conn port r =
  let u = Mindstorm.Sensor.Ultrasonic.make conn port in
  Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
  let turn_off() = Mindstorm.Sensor.Ultrasonic.set u `Off in
  r.at_exit <- turn_off :: r.at_exit;
  meas r (fun () -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

(* This measure always returns [true]. *)
let always r =
  { value = Some true;
    common = { robot = r;
               get_value = (fun () -> ());
               is_needed = false; (* does not matter *)
               is_up_to_date = true; (* no need not add it to [r.meas] *)
               is_constant = true;
             }
  }

(* Declaring events
 ***********************************************************************)

let remove_events r =
  List.iter (fun m -> m.is_needed <- false) r.meas;
  r.events <- []

let event ?retry meas cond f =
  let c = meas.common in
  c.is_needed <- true;
  (* [exec] returns [true] if the condition succeeded and executes the
     associated callback.  It returns [false] if the contition failed. *)
  let exec =
    if c.is_constant then
      (* Fetch the value once only and do not erase other events --
         thus return [false] to allow the execution of subsequent events. *)
      let v = read ?retry meas in
      (fun () -> f v; false)
    else
      (fun () ->
         (* Fetch the value only if the event is triggered. *)
         let v = read ?retry meas in
         if cond v then (
           remove_events c.robot;
           f v;
           true (* do not process further events *)
         )
         else false)
  in
  c.robot.events <- exec :: c.robot.events
;;


let event_is ?retry m f =
  event ?retry m (function Some true -> true | _ -> false) (fun _ -> f())


let rec exec_first = function
  | [] -> ()
  | e :: tl -> if not(e()) then exec_first tl

let run r =
  try
    while true do
      (* New loop => none of the measures is up_to_date anymore. *)
      List.iter (fun m -> m.is_up_to_date <- false) r.meas;
      if r.events = [] then
        failwith "Robot.run: no events declared (avoid infinite loop)";
      try
        exec_first(List.rev r.events)
      with Unix.Unix_error(Unix.EINTR, f, _) ->
        (* Often happens with an important effort (temporarily)
           draining the battery down. *)
        eprintf "Communication (%s) stopped by signal, \
		trying to continue...\n%!" f
    done
  with e ->
    (* Turn off sensors we know about (whenever possible). *)
    List.iter (fun f -> try f() with _ -> ()) r.at_exit;
    match e with
    | Exit -> () (* considered as an acceptable way to stop. *)
    | Unix.Unix_error(Unix.ENOTCONN, _, _)
    | Unix.Unix_error(Unix.ECONNRESET, _, _) -> failwith "Robot disconnected"
    | Failure _ as e -> raise e
    | e ->
        eprintf "Uncaught exception: %s\n%!" (Printexc.to_string e)



(* Local Variables: *)
(* compile-command: "make -k robot.cmo" *)
(* End: *)
