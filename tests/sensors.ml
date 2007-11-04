open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let () =
  printf "Please connect sensors as follow and press a key:\n";
  printf "- port 1: touch sensor\n";
  let conn = Mindstorm.connect_bluetooth bt in
(*   let touch = Mindstorm.Sensor.touch conn `In1 *)
(*   and sound = Mindstorm.Sensor.sound conn `In2 *)
(*   and light = Mindstorm.Sensor.light conn `In3 *)
(*   and ultrasonic = Mindstorm.Sensor.ultrasonic conn `In4 in *)
  printf "Press the touch sensor: %!";
  
  Mindstorm.close conn
