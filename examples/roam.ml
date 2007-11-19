open Printf
module Sensor = Mindstorm.Sensor
module U = Mindstorm.Sensor.Ultrasonic
module Motor = Mindstorm.Motor

(* `S1 : touch sensor
   `S4 : ultrasonic sensor
   Motors connected to A and B. *)

let usleep s = ignore(Unix.select [] [] [] s)

let run conn =
  let speed a b  =
    Motor.set conn Motor.a (Motor.speed a);
    Motor.set conn Motor.b (Motor.speed b) in

  Sensor.set conn `S1 `Switch `Raw;
  let ultra = Sensor.Ultrasonic.make conn `S4 in
  Sensor.Ultrasonic.set ultra `Meas_cont;
  while true do
    let switch = Sensor.get conn `S1 in
    if switch.Sensor.scaled = 0 then begin
      let dist = min 50 (Sensor.Ultrasonic.get ultra `Byte0) in
      printf "dist = %i\r%!" dist;
      speed dist (2 * dist - 50);
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
