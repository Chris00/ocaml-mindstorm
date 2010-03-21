open Printf

let () =
  let (bt2,col)=
    if Array.length Sys.argv < 3 then (
      printf "%s <bluetooth addr><co>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1), Sys.argv.(2)) in

  let  conn2 = Mindstorm.connect_bluetooth bt2
  and column = int_of_string col in
  let module R = Pincer.Run(struct let conn2 = conn2  end) in
  printf "Press the button on the robot to stop.\n%!";

  R.put_piece column R.stop;
  R.run()

