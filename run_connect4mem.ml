open Alphabetamem
open Board

let bt_pincer = ref "00:16:53:03:A5:32"
and bt_scan = ref "00:16:53:0C:84:49"
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

let move game col = ignore(Structure.makemove game col)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec computer_play game =
  (*on ecrit que c'est au tour du joueur jaune, donc l'ordi*)
  Board.write_player_turn Graphics.yellow;
  (*on lance alphabeta pour connaitre la colonne a jouer et on y joue*)
  let col_to_play = Ia.move_for game    in
  move game col_to_play;

  (*On test si la partie est finie*)
  if Structure.get_game_result game > -1 then
    (
      (* la partie est finie, on stoppe apres avoir ajouter la piece*)
      P.put_piece col_to_play
        (fun () ->
           Board.add_piece_to_board Graphics.yellow col_to_play;
           (* if Gamemem.draw game then Board.draw() *)
           (* else Board.yellow_success(); *)
           Sys.command "aplay win.wav";
           S.return_init_pos Board.close_when_clicked)
    )
  else
    (
      (*la partie continue, on met la piece et c'est au tour de l'autre joueur*)
      P.put_piece col_to_play
        (fun () ->
           Board.add_piece_to_board Graphics.yellow col_to_play;
           S.add_piece col_to_play;
           human_play game)
    )

and human_play game =
  (*on ecrit que c'est au tour du joueur rouge, donc l'humain*)
  Board.write_player_turn Graphics.red;
  (*on lance le scan pour connaitre la colonne qui a été jouée*)
  S.scan
    begin
      fun col ->
        move game col;
        Board.add_piece_to_board Graphics.red col;

        (* On verifie que la partie n'est pas finie *)
        if Structure.get_game_result game > -1 then
            (
              (* if Gamemem.draw game then Board.draw() *)
              (* else Board.red_success(); *)
              Sys.command "aplay Game_over.wav";
              S.return_init_pos Board.close_when_clicked
            )
        else
          (
            computer_play game
          )
    end

let () =
  Board.gameboard ();
  let game = Structure.make_board() in
  Structure.initboard game;
  (if Conn.fst_computer then computer_play else human_play) game;
  Robot.run Conn.r

