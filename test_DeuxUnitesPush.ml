open Printf
open Mindstorm.Sensor
module Motor = Mindstorm.Motor

let port_ultra = `S4
let light = `S3
let push = `S1
let motor_left = Motor.a
let motor_right = Motor.b
let dir = -1

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn
                  val conn2 : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  
  let robot = Robot.make()
  let ultra =
    let u = Mindstorm.Sensor.Ultrasonic.make C.conn port_ultra in
    Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas robot (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let ultra2 =
    let u = Mindstorm.Sensor.Ultrasonic.make C.conn2 port_ultra in
    Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas robot (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

 (* let pushed =
    Mindstorm.Sensor.set C.conn2 push `Switch `Bool;
    let v = Mindstorm.Sensor.get C.conn2 push in
    Robot.meas robot (fun _ -> v.Mindstorm.Sensor.scaled = 1)*)

  let touch = Robot.touch C.conn2 push robot

  let lightval =
    Mindstorm.Sensor.set C.conn2 light `Light_active `Pct_full_scale;
    let donne_lumiere = Mindstorm.Sensor.get C.conn light in
    Robot.meas robot (fun _ -> donne_lumiere)

  let stop3 _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    raise Exit

  let rec run3() =
    Motor.set C.conn Motor.all (Motor.speed (30*dir));
    wait3()

  and wait3() =
    Robot.event ultra (fun d-> d < Some(20)) stop3

  let stop2 _ =
    Mindstorm.Sensor.set C.conn2 light `Light_inactive `Pct_full_scale;
    Motor.set C.conn2 Motor.all (Motor.speed 0);
    run3()

  let rec run1() =
    Motor.set C.conn2 motor_left (Motor.speed (30*dir));
    Motor.set C.conn2 motor_right (Motor.speed (30*dir));
    wait2()

  and wait2() =
    Robot.event_is touch stop2

  let stop _ =
    run1()

  let wait() =
    Robot.event ultra (fun d -> d < Some(20)) stop

  let run() =
    wait();
    Robot.run robot


end;;

let () =
  let (bt,bt2) =
    if Array.length Sys.argv < 3 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1), Sys.argv.(2)) in
  let conn = Mindstorm.connect_bluetooth bt
  and conn2 = Mindstorm.connect_bluetooth bt2 in
  let module R = Run(struct let conn = conn and conn2 = conn2 end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
