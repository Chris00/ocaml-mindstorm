open Alphabetamem
open Board

let bt_pincer = ref "00:16:53:0C:84:49"
and bt_scan = ref "00:16:53:0A:F3:3C"
and if_computer = ref true

let spec = Arg.align ["--pince", Arg.Set_string bt_pincer,
                      "<bt_address>set the bluetooth address of the brick
which uses the pincer";
                      "--scan", Arg.Set_string bt_scan,
                      "<bt_address>set the bluetooth address of the brick
which uses the scan";
                      "--human_first", Arg.Clear if_computer,
                      " set first player"]
let () = Arg.parse spec (fun _ -> raise (Arg.Bad "no anonymous arg"))
  "run_connect4 <option>"

module Conn =
struct
  let r = Robot.make()
  let conn_pincer, conn_scan, fst_computer =
    Mindstorm.connect_bluetooth !bt_pincer,
    Mindstorm.connect_bluetooth !bt_scan,
    !if_computer
end

module P = Pincer.Run(Conn)
module S = ScanPiece2.Run(Conn)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec step game col =
  if col <> -1 then
    (
        Gamemem.makemove game col;
        Board.add_piece_to_board Graphics.red col
    );
  (*on verifie que le jeu n'est pas gagné ou match nul*)
  if (col = -1) ||
    (not (Gamemem.opponent_connected game col >= 4) && not (Gamemem.draw game))
  then
    (
      (*on cherche la colonne a jouer*)
      let _, col_to_play = Alphabetamem.alphabeta game 9 Gamemem.groupeval in
      Printf.printf "%i\n%!" col_to_play;
      Gamemem.makemove game col_to_play;
      Board.add_piece_to_board Graphics.yellow col_to_play;
      (*la pince va mettre la piece dans la colonne a jouer
        et on va scanner pour voir si le joueur a joue*)
      if not(Gamemem.connected game col_to_play >= 4) && not(Gamemem.draw game)
      then
        P.put_piece col_to_play
          (fun () -> S.scan col_to_play (fun c -> step game c))
      else
        (
          Printf.printf"c fini, on stoppe après avoir ajouter la piece\n%!";
          if Gamemem.draw game then Board.draw()
          else Board.yellow_success();
          Board.close_when_clicked();
          P.put_piece col_to_play S.return_init_pos;
          Printf.printf"LE ROBOT GAGNE\n%!"
        )
    )
  else
    (
      if Gamemem.draw game then Board.draw()
      else Board.red_success();
      Board.close_when_clicked();
      S.return_init_pos ()
    )
let () =
  Board.gameboard ();
  let game = Gamemem.make_board() in
  Gamemem.initboard game;
  if Conn.fst_computer then step game (-1)
  else S.scan (-1) (fun c -> step game c);
  Robot.run Conn.r

