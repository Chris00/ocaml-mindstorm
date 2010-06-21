open Printf
open Mindstorm.Sensor
module Motor = Mindstorm.Motor

let port_ultra = `S4
let port_micro = `S2
let motor = Motor.a
let port_light = `S3
let dir = -1

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot va tout droit; s'il touche un obstacle, il s'arrête. *)

  let robot = Robot.make()
  let ultra =
    let u = Mindstorm.Sensor.Ultrasonic.make C.conn port_ultra in
    Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas robot (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let light =
    Mindstorm.Sensor.set C.conn port_light `Light_active `Pct_full_scale;
    let donne_lumiere = Mindstorm.Sensor.get C.conn port_light in
    Robot.meas robot (fun _ -> donne_lumiere)

  let micro =
    Mindstorm.Sensor.set C.conn port_micro `Sound_db `Pct_full_scale;
    let donne_son = Mindstorm.Sensor.get C.conn port_micro in
    Robot.meas robot (fun _ -> donne_son)

  let rec stop _ =
    Motor.set C.conn motor (Motor.speed 0);
     let cara = match Robot.read ~retry:5 ultra with
       | Some v -> v
       | none -> -1;
     in printf "%i\n" cara;
     let niveau_gris =
       match Robot.read ~retry:5 light with
       | Some v -> v.raw
       | none -> -1;
     in printf "%i\n" niveau_gris;
     let niveau_son =
       match Robot.read ~retry:5 micro with
       | Some v -> v.raw
       | none -> -1;
     in printf "%i\n" niveau_son;
     Mindstorm.Sensor.set C.conn port_light `Light_inactive `Pct_full_scale;
     raise Exit

  let wait () =
    printf "Waiting signal...\n";
    (*Motor.set C.conn Motor.all (Motor.speed (30 * dir));*)
    let f x =
      match x with
      | Some v -> v < 10
      | none -> false;
    in Robot.event ultra f stop

  let run() =
    printf "Start...\n";
    wait();
    Robot.run robot
end;;

let () =
  let bt =
    if Array.length Sys.argv < 3 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) and bt2 = Sys.argv.(2) in
  let conn = Mindstorm.connect_bluetooth bt
  and conn2 = Mindstorm.connect_bluetooth bt2 in
  let module R = Run(struct let conn = conn end) in
  let module S = Run(struct let conn = conn2 end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run();
  S.run()

