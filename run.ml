open Alphabeta
open Pincer

module Run(C: sig val conn1 : Mindstorm.bluetooth Mindstorm.conn
                  val conn2 : Mindstorm.bluetooth Mindstorm.conn end) =

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let play game color fst_player =
  if fst_player then
    let col_to_play = snd (alphabeta game color neg_infinity infinity 8 h) in
    put_piece col_to_play
end

let () =
  let (bt2,col)=
    if Array.length Sys.argv < 3 then (
      printf "%s <bluetooth addr><co>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1),Sys.argv.(2)) in

  let  conn2 = Mindstorm.connect_bluetooth bt2
  and column = int_of_string col in
  let module R = Run(struct let conn2 = conn2  end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run(column)
