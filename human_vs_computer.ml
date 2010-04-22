let rec computer_play game =
  (* On cherche la colonne a jouer *)
  Board.write_player_turn Graphics.yellow;
  let _, col_to_play = Alphabetamem.alphabeta game 9 Gamemem.groupeval in
  Gamemem.makemove game col_to_play;
  if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then
    (
      Board.add_piece_to_board Graphics.yellow col_to_play;
      if Gamemem.draw game then Board.draw()
      else Board.yellow_success();
      Board.close_when_clicked ()
    )
  else
    (
      Board.add_piece_to_board Graphics.yellow col_to_play;
      human_play game
    )

and human_play game =
  Board.write_player_turn Graphics.red;
  let col = Board.play () in
  Gamemem.makemove game col;
  Board.add_piece_to_board Graphics.red col;
  if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then
    (
      if Gamemem.draw game then Board.draw()
      else Board.red_success();
      Board.close_when_clicked
    )
  else
    (
      computer_play game
    )


let () =
  let game = Gamemem.make_board() in
  Gamemem.initboard game;
  computer_play game

