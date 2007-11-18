open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

let usleep s = ignore(Unix.select [] [] [] s)


let switch = `S1
let ultrasonic = `S4
let left = Motor.a
and right = Motor.b

let run conn =
  Sensor.set conn switch `Switch `Raw;
  let ultra = Sensor.Ultrasonic.make conn ultrasonic in
  Sensor.Ultrasonic.set ultra `Meas_cont;
  while true do
    let sw = Sensor.get conn switch in
    printf "Switch = %i\n%!" sw.Sensor.raw;
    if sw.Sensor.normalized > 500 then begin
      let dist = min 50 (Sensor.Ultrasonic.get ultra `Byte0) in
      Motor.set conn left (Motor.speed dist);
      Motor.set conn right (Motor.speed (2 * dist - 50));
      (*       Unix.sleep 1; *)
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
  run conn;
  Mindstorm.close conn
