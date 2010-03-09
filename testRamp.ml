open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let switch_port = `S1


module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct

  let r = Robot.make()

  let touch = Robot.touch C.conn switch_port r

  let stop _ =
   Motor.set C.conn Motor.all (Motor.speed 0)


  (*le moteur accélère tout au long de la tach_limit
    jusqu'à la vitesse [speed]   *)
  let go_motor angle speed=
    Robot.event_is touch stop;
    let st = {speed = speed; motor_on = true; brake = true;
              regulation = `Motor_speed; turn_ratio = 100;
              run_state = `Ramp_up; tach_limit = angle} in
    Motor.set C.conn Motor.a st


  let run () =
    go_motor 1000 80;
    Robot.run r

end

let () =
  let (bt)=
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1)) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
