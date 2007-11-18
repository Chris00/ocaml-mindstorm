(* Example based on
   http://www.mactech.com/articles/mactech/Vol.23/23.04/LegoNXTontheMac/index.html
*)
open Printf
module Motor = Mindstorm.Motor
module U = Mindstorm.Sensor.Ultrasonic

let usleep s = ignore(Unix.select [] [] [] s)

let speed conn s turn_ratio =
  Motor.set conn Motor.a (Motor.speed ~sync:true ~turn_ratio s);
  Motor.set conn Motor.b (Motor.speed ~sync:true ~turn_ratio s)

let run conn =
  let ultra = U.make conn `S4 in
  U.set ultra `Meas_cont;
  let i = ref 0 in
  while true do
    incr i;
    let dist = U.get ultra `Byte0 in
    if dist > 63 then (
      printf "%4i: Forward;     dist = %i\r%!" !i dist;
      speed conn 75 0
    )
    else (
      printf "%4i: Turn;        dist = %i\r%!" !i dist;
      speed conn 25 (-100)
    );
    usleep 0.2;
  done


let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);  exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let stop () =
    speed conn 0 0;
    Mindstorm.close conn;
    exit 0 in
  Sys.set_signal Sys.sigint (Sys.Signal_handle(fun _ -> stop()));
  printf "Press Ctrl-c to quit.\n%!";
  run conn
