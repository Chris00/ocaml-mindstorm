let rec computer_play game =
  (* On cherche la colonne a jouer *)
  Display.write_player_turn Graphics.yellow;
  let _, col_to_play = Alphabetamem.alphabeta game 9 Gamemem.groupeval in
  Gamemem.makemove game col_to_play;
  if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then
    (
      Display.add_piece_to_board Graphics.yellow col_to_play;
      if Gamemem.draw game then Display.draw()
      else Display.yellow_success();
      Display.close_when_clicked ()
    )
  else
    (
      Display.add_piece_to_board Graphics.yellow col_to_play;
      human_play game
    )

and human_play game =
  Display.write_player_turn Graphics.red;
  let col = Display.play () in
  Gamemem.makemove game col;
  Display.add_piece_to_board Graphics.red col;
  if Gamemem.get_game_result game = Gamemem.WIN || Gamemem.draw game then
    (
      if Gamemem.draw game then Display.draw()
      else Display.red_success();
      Display.close_when_clicked ()
    )
  else
    (
      computer_play game
    )


let () =
  Display.gameboard();
  let game = Gamemem.make_board() in
  Gamemem.initboard game;
  computer_play game

