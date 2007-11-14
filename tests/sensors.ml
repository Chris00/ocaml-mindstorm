open Printf
module Sensor = Mindstorm.Sensor

let bt =
  if Array.length Sys.argv < 2 then begin
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  end
  else Sys.argv.(1)

let wait_for_ENTER () = ignore(read_line())

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
  printf "Please connect sensors as follow:\n";
  printf "- port 1: touch sensor\n";
  printf "- port 2: light sensor\n";
  printf "- port 3: sound sensor\n";
  printf "- port 4: ultrasonic sensor\n";
  printf "Press ENTER. ";
  wait_for_ENTER();
  let conn = Mindstorm.connect_bluetooth bt in

  let test_sensor name port =
    printf "- %s sensor; when finished press ENTER.%!\n" name;
    repeat_till_ENTER begin fun i ->
      let data = Sensor.get conn port in
      printf "%4i:\t raw = %4i   normalized = %4i   scaled = %-5i\r%!"
        i data.Sensor.raw data.Sensor.normalized data.Sensor.scaled;
    end;
    printf "\n" in

  Sensor.set conn `S1 `Switch `Bool;
  test_sensor "Touch (bool)" `S1;
  (*   Sensor.set conn `S1 `Switch `Transition_cnt; *)
  Sensor.set conn `S1 `Switch `Period_counter;
  test_sensor "Touch (period)" `S1;

  Sensor.set conn `S2 `Light_active `Pct_full_scale;
  (*   Sensor.set conn `S2 `Switch `Light_inactive; *)
  test_sensor "Light" `S2;

  Sensor.set conn `S3 `Sound_db `Pct_full_scale;
  (*   Sensor.set conn `S3 `Sound_dba `Pct_full_scale; *)
  test_sensor "Sound" `S3;

  Sensor.set_ultrasonic conn `S4;
    printf "- Ultrasonic sensor; when finished press ENTER.%!\n";
    repeat_till_ENTER begin fun i ->
      let dist = Sensor.get_ultrasonic conn `S4 in
      printf "%4i:\t dist = %i\r%!" i dist
    end;
    printf "\n";

  Mindstorm.close conn
