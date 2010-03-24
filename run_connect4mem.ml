open Alphabetamem
open Board

let bt_pincer = ref "00:16:53:0C:84:49"
and bt_scan = ref "00:16:53:0A:F3:3C"
and if_computer = ref true

let spec = Arg.align [
  "--pince", Arg.Set_string bt_pincer,
  "<bt_address> set the bluetooth address of the brick which uses the pincer";
  "--scan", Arg.Set_string bt_scan,
  "<bt_address> set the bluetooth address of the brick which uses the scan";
  "--human_first", Arg.Clear if_computer,
  " set human as first player"]
let () =
  Arg.parse spec (fun _ -> raise (Arg.Bad "no anonymous arg"))
    "run_connect4 <options>"

module Conn =
struct
  let r = Robot.make()
  let conn_pincer = Mindstorm.connect_bluetooth !bt_pincer
  and conn_scan = Mindstorm.connect_bluetooth !bt_scan
  and fst_computer = !if_computer
end

module P = Pincer.Run(Conn)
module S = ScanPiece.Run(Conn)

let move game col = ignore(Gamemem.makemove game col)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec computer_play game =
  (* On cherche la colonne a jouer *)
  let _, col_to_play = Alphabetamem.alphabeta game 9 Gamemem.groupeval in
  Printf.printf "col_to_play = %i\n%!" col_to_play;
  move game col_to_play;
  Board.add_piece_to_board Graphics.yellow col_to_play;
  (*la pince va mettre la piece dans la colonne a jouer
    et on va scanner pour voir si le joueur a joue*)
  if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then (
    Printf.printf "c fini, on stoppe après avoir ajouter la piece\n%!";
    if Gamemem.draw game then Board.draw()
    else Board.yellow_success();
    Printf.printf "LE ROBOT GAGNE\n%!";
    P.put_piece col_to_play
      (fun () -> S.return_init_pos Board.close_when_clicked)
  )
  else
    P.put_piece col_to_play (fun () ->
                               S.add_piece col_to_play;
                               human_play game)

and human_play game =
  S.scan begin fun col ->
    move game col;
    Board.add_piece_to_board Graphics.red col;
    (* On verifie que le jeu n'est pas gagné ou match nul *)
    if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then (
      if Gamemem.draw game then Board.draw()
      else Board.red_success();
      Printf.printf "L'HUMAIN A GAGNE\n%!";
      S.return_init_pos Board.close_when_clicked
    )
    else computer_play game
  end

let () =
  Board.gameboard ();
  let game = Gamemem.make_board() in
  Gamemem.initboard game;
  (if Conn.fst_computer then computer_play else human_play) game;
  Robot.run Conn.r

