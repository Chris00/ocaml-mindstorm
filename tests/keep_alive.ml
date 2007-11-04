open Printf

let bt = "00:16:53:03:A5:32"

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  printf "Current sleep time limit = %!";
  let bat = Mindstorm.keep_alive conn in
  printf "%i milliseconds\n%!" bat;
  Mindstorm.close conn
