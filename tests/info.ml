open Printf

type connection = Bluetooth of string | USB of string

let new_brick_name = ref None
let bt = ref None

let args = Arg.align [
  "--name", Arg.String(fun n -> new_brick_name := Some n),
  "n Set a new name for the brick";
  "--bt", Arg.String(fun n -> bt := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.String(fun n -> bt := Some(USB n)),
  "dev Connects to this USB device";
]
let usage_msg = sprintf "%s (--by addr|-usb dev)" Sys.argv.(0)


let print_info conn =
  printf "Connected!\n%!";
  begin match !new_brick_name with
  | None -> ()
  | Some name -> Mindstorm.set_brick_name conn name ~check_status:true;
  end;
  printf "Device info: \n%!";
  let i = Mindstorm.get_device_info conn in
  printf "- brick name = %S\n" i.Mindstorm.brick_name;
  printf "- bluetooth address = %S\n" i.Mindstorm.bluetooth_addr;
  printf "- signal strength = %i\n" i.Mindstorm.signal_strength;
  printf "- free user FLASH = %i bytes\n" i.Mindstorm.free_user_flash;
  let (p1, p0, f1, f0) = Mindstorm.firmware_version conn in
  printf "- protocol = %i.%i, firmware = %i.%02i\n" p1 p0 f1 f0;
  printf "Battery level: %!";
  let bat = Mindstorm.battery_level conn in
  printf "%i millivolts\n%!" bat;
  Mindstorm.close conn

let () =
  Arg.parse args (fun a -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !bt with
  | Some(Bluetooth addr) -> print_info(Mindstorm.connect_bluetooth addr)
  | Some(USB dev) -> print_info(Mindstorm.connect_usb dev)
  | None -> Arg.usage args usage_msg; exit 1
