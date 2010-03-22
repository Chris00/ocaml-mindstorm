open Printf

let (bt,col)=
  if Array.length Sys.argv < 3 then (
    printf "%s <bluetooth addr><co>\n" Sys.argv.(0);
    exit 1;
  )
  else (Sys.argv.(1), int_of_string Sys.argv.(2))

module Conn = struct
  let r = Robot.make()
  let conn_scan = Mindstorm.connect_bluetooth bt
end

let () =
  let module R = ScanPiece.Run(Conn) in
  printf "Press the button on the robot to stop.\n%!";

  R.scan col R.stop;
  Robot.run Conn.r



