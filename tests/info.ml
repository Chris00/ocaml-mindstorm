open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  printf "Connected!\n%!";
  Mindstorm.set_brick_name conn "UMH1" ~check_status:true;
  printf "Device info: \n%!";
  let i = Mindstorm.get_device_info conn in
  printf "- brick name = %S\n" i.Mindstorm.brick_name;
  printf "- bluetooth address = %S\n" i.Mindstorm.bluetooth_addr;
  printf "- signal strength = %i\n" i.Mindstorm.signal_strength;
  printf "- free user FLASH = %i bytes\n" i.Mindstorm.free_user_flash;
  let (p1, p0, f1, f0) = Mindstorm.firmware_version conn in
  printf "- protocol = %i.%i, firmware = %i.%02i\n" p1 p0 f1 f0;
  Mindstorm.close conn
