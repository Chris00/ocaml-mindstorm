open Printf

type connection = Bluetooth of string | USB
let dev = ref None

type t = {
  args : (Arg.key * Arg.spec * Arg.doc) list;
  f : 'a. 'a Mindstorm.conn -> unit;
}

let default_args = [
  "--bt", Arg.String(fun n -> dev := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.Unit(fun () -> dev := Some USB), " Connects to a USB NXT brick";
]

let and_do d =
  let args = Arg.align(default_args @ d.args) in
  let usage_msg = sprintf "%s (--by addr|--usb)" Sys.argv.(0) in
  Arg.parse args (fun a -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !dev with
  | Some(Bluetooth addr) ->
      let conn = Mindstorm.connect_bluetooth addr in
      d.f conn;
      Mindstorm.close conn
  | Some USB ->
      (match Mindstorm.USB.bricks() with
       | dev :: _ ->
           let conn = Mindstorm.USB.connect dev in
           d.f conn;
           Mindstorm.close conn
       | [] -> print_endline "No NXT brick connected to a USB port.")
  | None -> Arg.usage args usage_msg; exit 1
