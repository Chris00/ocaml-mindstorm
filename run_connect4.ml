open Alphabeta

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

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec computer_play game =
  Display.write_player_turn Graphics.yellow;
  let _, col_to_play = alphabeta game Game.Yellow 8 heuristic in
  Game.move game col_to_play Game.Yellow;
  if Game.is_winning game col_to_play || Game.is_draw game then
    (
      P.put_piece col_to_play
        (fun () ->
           Display.add_piece_to_board Graphics.yellow col_to_play;
           if Game.is_draw game then Display.draw()
           else Display.yellow_success();
           S.return_init_pos Display.close_when_clicked)
    )
  else
    (
      P.put_piece col_to_play
        (fun () ->
           Display.add_piece_to_board Graphics.yellow col_to_play;
           S.add_piece col_to_play;
           human_play game)
    )

and human_play game =
  Display.write_player_turn Graphics.red;
  S.scan
    begin
      fun col ->
        Game.move game col Game.Red;
        Display.add_piece_to_board Graphics.red col;
        if Game.is_winning game col || Game.is_draw game then
          (
            if Game.is_draw game then Display.draw()
            else Display.red_success();
            S.return_init_pos Display.close_when_clicked
          )
    end

let () =
  Display.gameboard ();
  let game = Game.make() in
  (if Conn.fst_computer then computer_play else human_play) game;
  Robot.run Conn.r

