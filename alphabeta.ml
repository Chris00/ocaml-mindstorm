open Game

let rec alphabeta current_game actor1 actor2 alpha beta =
  if(current_game.number_of_move <> 0 && isWin current_game) then
    if (actor1.fst_player) then (1., (List.hd current_game.list_event).col)
    else (-1., (List.hd current_game.list_event).col)

  else if (current_game.number_of_move = 42)
  then (0., (List.hd current_game.list_event).col)
  else
    (
      let game = copy_game current_game in
      if (actor1.fst_player) then
        (*faire un remove???*)
        let rec cut_beta a b value col_current i =
          if (i < 7) then
            (
              move game i actor1;
              let test_value = fst (alphabeta game actor2 actor1 a b) in
              let (value_temp, col) = if test_value > value then
                (test_value, i) else (value, col_current) in
              if value_temp > b then (value_temp, col)
              else cut_beta (max a value_temp) b value_temp col (i+1)
            )
          else (value, col_current) in
        cut_beta alpha beta neg_infinity 0 0;

      else let rec cut_alpha a b value col_current i =
        if (i < (Array.length game.tab)) then
          (
            move game i actor1;
            let test_value = fst (alphabeta game actor2 actor1 a b) in
            let (value_temp, col) = if test_value < value then
              (test_value, i) else (value, col_current) in
            if value_temp < a then (value_temp, col)
            else cut_alpha a (min b value_temp) value_temp col (i+1)
          )
        else (value, col_current) in
      cut_alpha alpha beta infinity 0 0
    )
;;

let g = make ();;
let player1 = {player = Human; pion = Yellow; fst_player = true};;
let player2 = {player = Human; pion = Red; fst_player = false};;
move g 0 player1;
move g 1 player2;
move g 1 player1;
move g 0 player2;
move g 0 player1;
move g 1 player2;
move g 1 player1;
move g 0 player2;
move g 0 player1;
move g 1 player2;
move g 1 player1;
move g 0 player2;
move g 3 player1;
move g 2 player2;
move g 2 player1;
move g 3 player2;
move g 3 player1;
move g 2 player2;
move g 2 player1;
move g 3 player2;
move g 3 player1;
move g 2 player2;
move g 2 player1;
move g 3 player2;
move g 5 player1;
move g 6 player2;
move g 6 player1;
move g 5 player2;
move g 5 player1;
move g 6 player2;
move g 6 player1;
move g 5 player2;
move g 5 player1;
move g 6 player2;
move g 6 player1;
move g 5 player2;
move g 4 player1;
move g 4 player2;
move g 4 player1;
move g 4 player2;
move g 4 player1;;

let cuple = alphabeta g player1 player2 (-.1.) 1.;;
