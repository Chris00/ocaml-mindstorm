open Printf

let () =
  let bt=
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1)) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = ScanPiece.Run(struct let conn = conn end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
