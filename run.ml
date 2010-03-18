open Alphabeta
open Pincer
open scanPiece

module Run(C: sig val conn1 : Mindstorm.bluetooth Mindstorm.conn
                  val conn2 : Mindstorm.bluetooth Mindstorm.conn end) =
struct
(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
  let play game color fst_player =
    let rec step fst_move =
      if fst_player then
        if (*lautre joueur gagne ou match nul*) then
          Printf.printf "le joueur humain a gagner ou match nul"
        else
          let col_to_play = snd (alphabeta game color neg_infinity infinity 8 h)
          in
          Game.move game col_to_play color;
          if is_winning game col_to_play || is_draw game then
            put_piece col_to_play scanPiece.stop
          else put_piece col_to_play (scan_game game (step true))
      else
        scan_game game (step true)
    in step fst_player

  let run player = play (Game.make()) Game.yellow player
end

let () =
  let (bt1, bt2, if_computer)=
    if Array.length Sys.argv < 4 then (
      Printf.printf "%s <bluetooth addr><bluetooth addr><if_computer>\n"
        Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1),Sys.argv.(2),Sys.argv.(3)) in

  let  conn1 = Mindstorm.connect_bluetooth bt1
  and conn2 = Mindstorm.connect_bluetooth bt2
  and player =  bool_of_string if_computer in
  let module R = Run(struct let conn1 = conn1 and conn2 = conn2  end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run(player)
