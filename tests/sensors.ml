open Printf
module Sensor = Mindstorm.Sensor

type connection = Bluetooth of string | USB
let dev = ref None

let args = Arg.align [
  "--bt", Arg.String(fun n -> dev := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.Unit(fun () -> dev := Some USB), " Connects to a USB NXT brick";
]
let usage_msg = sprintf "%s (--by addr|--usb)" Sys.argv.(0)

IFDEF WIN32 THEN
(* Win32 command shell is poor *)
let repeat_till_ENTER msg f =
  printf "%s.\n%!" msg;
  let i = ref 0 in
  while !i < 200 do f !i; incr i done

ELSE
(* Unix & Mac OSX have proper terminals *)
let repeat_till_ENTER msg f =
  let params = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
    { params with Unix.c_icanon = false; c_echo = false;
        c_vmin = 0; c_vtime = 0; };
  (* FIXME: We should also catch signals *)
  let no_key_pressed() = Unix.read Unix.stdin " " 0 1 = 0 in
  printf "%s; when finished press ENTER.\n%!" msg;
  try
    let i = ref 0 in
    while no_key_pressed() do f !i; incr i done;
    (* restore params *)
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params;
    raise e
ENDIF

let sensors conn =
  printf "It is assumed that sensors are connected as follows:\n";
  printf "- port 1: touch sensor\n";
  printf "- port 2: light sensor\n";
  printf "- port 3: sound sensor\n";
  printf "- port 4: ultrasonic sensor\n%!";

  let test_sensor name port =
    repeat_till_ENTER (sprintf "- %s sensor" name) begin fun i ->
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

  Sensor.set conn `S2 `Light_active Pct_full_scale;
  (*   Sensor.set conn `S2 `Switch `Light_inactive; *)
  test_sensor "Light" `S2;
  Sensor.set conn `S2 `No_sensor `Pct_full_scale;

  Sensor.set conn `S3 `Sound_db `Pct_full_scale;
  (*   Sensor.set conn `S3 `Sound_dba `Pct_full_scale; *)
  test_sensor "Sound" `S3;

  let us = Sensor.Ultrasonic.make conn `S4 in
  Sensor.Ultrasonic.set us `Meas_cont ~check_status:true;
  repeat_till_ENTER "- Ultrasonic sensor" begin fun i ->
    try
      let dist = Sensor.Ultrasonic.get us `Byte0 in
      printf "%4i:\t                    dist = %i\r%!" i dist
    with e ->
      printf "%4i:\t %s\r%!" i (Printexc.to_string e);
      Unix.sleep 1
  end;
  printf "\n";

  Mindstorm.close conn

let () =
  Arg.parse args (fun a -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !dev with
  | Some(Bluetooth addr) -> sensors(Mindstorm.connect_bluetooth addr)
  | Some USB ->
      (match Mindstorm.USB.bricks() with
       | dev :: _ -> sensors(Mindstorm.USB.connect dev)
       | [] -> print_endline "No NXT brick connected to a USB port.")
  | None -> Arg.usage args usage_msg; exit 1
