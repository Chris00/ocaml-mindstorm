open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)


let switch = `S1
let ultrasonic = `S4
let left = Motor.a
and right = Motor.b

let speed s =
  { Motor.speed = s;  motor_on = s <> 0;  brake = false;
    regulation = `Idle;
    turn_ratio = 0; run_state = `Running; tach_limit = 0  }

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  Sensor.set conn switch `Switch `Raw;
  Sensor.Ultrasonic.set conn ultrasonic;
  Motor.set conn left ;
  let rec run () =
    let sw = Sensor.get conn switch in
    printf "Switch = %i\n%!" sw.Sensor.raw;
    if sw.Sensor.normalized > 500 then begin
      let dist = (Sensor.get conn ultrasonic).Sensor.raw in
      let dist = min 50 (max 0 dist) in
      Motor.set conn left (speed dist);
      Motor.set conn right (speed (2 * dist - 50));
(*       Unix.sleep 1; *)
    end in
  run ();
  Mindstorm.close conn
