open Printf
module Sensor = Mindstorm.Sensor

let bt =
  if Array.length Sys.argv < 2 then begin
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  end
  else Sys.argv.(1)

let repeat_till_ENTER f =
  let params = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
    { params with Unix.c_icanon = false; c_echo = false;
        c_vmin = 0; c_vtime = 0; };
  (* FIXME: We should also catch signals *)
  let no_key_pressed() = Unix.read Unix.stdin " " 0 1 = 0 in
  try
    let i = ref 0 in
    while no_key_pressed() do f !i; incr i done;
    (* restore params *)
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params;
    raise e


let () =
  printf "It is assumed that sensors are connected as follows:\n";
  printf "- port 1: touch sensor\n";
  printf "- port 2: light sensor\n";
  printf "- port 3: sound sensor\n";
  printf "- port 4: ultrasonic sensor\n%!";

  let conn = Mindstorm.connect_bluetooth bt in

  let test_sensor name port =
    printf "- %s sensor; when finished press ENTER.\n%!" name;
    repeat_till_ENTER begin fun i ->
      let data = Sensor.get conn port in
      printf "%4i:\t raw = %4i   normalized = %4i   scaled = %-5i\r%!"
        i data.Sensor.raw data.Sensor.normalized data.Sensor.scaled;
    end;
    printf "\n" in

  Sensor.set conn `S1 `Switch `Bool;
  test_sensor "Touch (bool)" `S1;
  Sensor.set conn `S1 `Switch `Transition_cnt;
  test_sensor "Touch (transition)" `S1;
  Sensor.reset_scaled conn `S1;
  Sensor.set conn `S1 `Switch `Period_counter;
  test_sensor "Touch (period)" `S1;

  Sensor.set conn `S2 `Light_active `Pct_full_scale;
  (*   Sensor.set conn `S2 `Switch `Light_inactive; *)
  test_sensor "Light" `S2;
  Sensor.set conn `S2 `No_sensor `Pct_full_scale;

  Sensor.set conn `S3 `Sound_db `Pct_full_scale;
  (*   Sensor.set conn `S3 `Sound_dba `Pct_full_scale; *)
  test_sensor "Sound" `S3;

  let us = Sensor.Ultrasonic.make conn `S4 in
  printf "- Ultrasonic sensor; when finished press ENTER.\n%!";
  Sensor.Ultrasonic.set us `Meas_cont ~check_status:true;
  repeat_till_ENTER begin fun i ->
    try
      let dist = Sensor.Ultrasonic.get us `Byte0 in
      printf "%4i:\t                    dist = %i\r%!" i dist
    with e ->
      printf "%4i:\t %s\r%!" i (Printexc.to_string e);
      Unix.sleep 1
  end;
  printf "\n";

  Mindstorm.close conn
