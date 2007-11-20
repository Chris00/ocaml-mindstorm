open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

(* `S1 : touch sensor
   `S4 : ultrasonic sensor
   Motors connected to B and C (like the basic robot of the LEGO® set). *)

let usleep s = ignore(Unix.select [] [] [] s)

let run conn =
  let speed b c  =
    Motor.set conn Motor.b (Motor.speed b);
    Motor.set conn Motor.c (Motor.speed c) in

  Sensor.set conn `S1 `Switch `Bool;
  let ultra = Sensor.Ultrasonic.make conn `S4 in
  let cnt = ref 0 in
  while true do
    incr cnt;
    let switch = Sensor.get conn `S1 in
    if switch.Sensor.scaled = 0 then begin
      Sensor.Ultrasonic.set ultra `Meas;
      let dist = min 50 (Sensor.Ultrasonic.get ultra `Byte0) in
      printf "%4i: dist = %i\r%!" !cnt dist;
      speed dist dist;  (*(2 * dist - 50)*)
      usleep 0.2;
    end
    else begin
      printf "\nSwitch pressed => stop\n";
      speed 0 0;
      Mindstorm.close conn;
      exit 0
    end
  done

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  printf "Press the button on the robot to stop.\n%!";
  run conn
