open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

 
let color_port = `S1

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =

struct

  let robot = Robot.make()

  let run () =
    for i=1 to 10 do
      Mindstorm.Sensor.set C.conn color_port `Color_full `Pct_full_scale;
      Unix.sleep 1;
      let data  = Mindstorm.Sensor.get C.conn color_port in
      let color_of_data data = match Sensor.color_of_data data with
        | `Black  -> "black " | `Blue -> "blue  " | `Green -> "green "
        | `Yellow -> "yellow" | `Red  -> "red   " | `White -> "white " in
      let color = color_of_data data in
      Printf.printf "%s" color;
      Printf.printf "\n";
      Unix.sleep 1;
      Mindstorm.Sensor.set C.conn color_port `No_sensor `Raw;
      Unix.sleep 1
    done;
end;;

let () =
  let bt =
    if Array.length Sys.argv < 2 then
      (
        printf "%s <bluetooth addr>\n" Sys.argv.(0);
        exit 1;
      )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in                                                                                                                                                                                                                                                                                                      
  R.run()









