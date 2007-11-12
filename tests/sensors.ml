open Printf
module Sensor = Mindstorm.Sensor

let bt =
  if Array.length Sys.argv < 2 then begin
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  end
  else Sys.argv.(1)

let wait_for_ENTER () = ignore(read_line())


let () =
  printf "Please connect sensors as follow and press a ENTER:\n";
  printf "- port 1: touch sensor\n";
  printf "- port 2: sound sensor\n";
  printf "- port 3: light sensor\n";
  printf "- port 4: ultrasonic sensor\n";
  wait_for_ENTER();
  let conn = Mindstorm.connect_bluetooth bt in
  Sensor.set conn `S1 `Switch `Bool;

  printf "- Press the touch sensor, then press ENTER.%!\n";
  wait_for_ENTER();
  let data = Sensor.get conn `S1 in
  printf "  Touch: raw = %-4i,\tnormalized = %i,\t scaled = %i\n"
    data.Sensor.raw data.Sensor.normalized data.Sensor.scaled;

  (*   and sound = Mindstorm.Sensor.sound conn `In2 *)
  (*   and light = Mindstorm.Sensor.light conn `In3 *)
  (*   and ultrasonic = Mindstorm.Sensor.ultrasonic conn `In4 in *)

  Mindstorm.close conn
