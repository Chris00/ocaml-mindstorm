open Printf
open Mindstorm.Sensor
open Mindstorm.Motor


let port_ultra = `S4
let port_light = `S1
let motor_right = a
(*let motor_left = Motor.b*)
let dir = -1

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct

  let robot = Robot.make()
(*
  let ultra =
    let u = Mindstorm.Sensor.Ultrasonic.make C.conn port_ultra in
    Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas robot (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)
*)
  let light =
    Mindstorm.Sensor.set C.conn port_light `Light_active `Pct_full_scale;
    let donne_lumiere = Mindstorm.Sensor.get C.conn port_light in
    Robot.meas robot (fun _ -> donne_lumiere)

  let return_fst (a, b, c, d) = a

  let parcourt =
    let tab = Array.make 7 0 in
    for i=0 to 6 do
      set C.conn motor_right (speed ~tach_limit:119 (-15));
      Robot.event light (fun d -> true);
      tab.(i) <-
        match (Robot.read ~retry:5 light) with
        | Some v -> v.raw
        | none -> -1
    done;
    tab

  let speed motor ?tach_limit sp =
    set C.conn motor (speed ?tach_limit (-sp))

  let turn tl sp =
    speed motor_right ~tach_limit:tl sp

  let run() =
    (* Motor.set C.conn motor_right (Motor.speed (15*dir));*)
    (* Motor.set C.conn motor_left (Motor.speed (30*dir));*)
   (* turn 30 20;*)
    (*turn 15 20;*)
      (*Motor.set C.conn motor_right (Motor.speed ~tach_limit:120 15);*)
    let a = parcourt in
    for i=0 to 6 do
      print_string (string_of_int(a.(i))^"\n")
    done;
    Robot.run robot


end;;

let () =
 let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
