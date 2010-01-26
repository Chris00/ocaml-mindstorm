type player = Human | Computer
type actor = { player : player; pion : Game.slot_content; fst_player : bool }

let rec alphabeta current_game actor1 actor2 alpha beta =
  if Game.number_of_moves current_game <> 0 && Game.is_winning current_game then
(*on devrait renvoyé 1 car c'est actor1 qui gagne tjs, en faite c'est celui
    qui vient de jouer qui gagne, et donc dans ce cas c'est actor1*)
    if (actor1.fst_player) then (1., Game.last_move_col current_game)
    else (-1., Game.last_move_col current_game)

  else if Game.number_of_moves current_game = 42 then
    (0., Game.last_move_col current_game)
  else
    (
      let game = Game.copy current_game in
      if actor1.fst_player then
        (*faire un remove???*)
        let rec cut_beta a b value col_current i =
          if (i < 7) then
            (
              if Game.npieces_col game i < 6 then
                (
                  Game.move game i actor1.pion;
                  let test_value = fst (alphabeta game actor2 actor1 a b) in
                  let (value_temp, col) = if test_value > value then
                    (test_value, i) else (value, col_current) in
                  if value_temp > b then (value_temp, col)
                  else cut_beta (max a value_temp) b value_temp col (i+1)
                )
              else cut_beta a b value col_current (i+1);
            )
          else (value, col_current) in
        cut_beta alpha beta neg_infinity 0 0;

      else let rec cut_alpha a b value col_current i =
        if i < 7 then
          (
            if Game.npieces_col game i < 6 then
              (
                Game.move game i actor1.pion;
                let test_value = fst (alphabeta game actor2 actor1 a b) in
                let (value_temp, col) = if test_value < value then
                  (test_value, i) else (value, col_current) in
                if value_temp < a then (value_temp, col)
                else cut_alpha a (min b value_temp) value_temp col (i+1)
              )
            else cut_alpha a b value col_current (i+1);
          )
        else (value, col_current) in
      cut_alpha alpha beta infinity 0 0
    )
;;

let g = Game.make ();;
let player1 = {player = Human; pion = Game.Yellow; fst_player = true};;
let player2 = {player = Human; pion = Game.Red; fst_player = false};;

Game.move g 0 player1.pion;;
Game.move g 0 player1.pion;;
Game.move g 0 player1.pion;;
Game.move g 0 player2.pion;;
(* g.tab;; *)

Game.move g 0 player1.pion;
Game.move g 1 player2.pion;
Game.move g 1 player1.pion;
Game.move g 0 player2.pion;
Game.move g 0 player1.pion;
Game.move g 1 player2.pion;
Game.move g 1 player1.pion;
Game.move g 0 player2.pion;
Game.move g 0 player1.pion;
Game.move g 1 player2.pion;
(*move g 1 player1;*)
(*move g 0 player2;*)
Game.move g 3 player1.pion;
Game.move g 2 player2.pion;
Game.move g 2 player1.pion;
Game.move g 3 player2.pion;
Game.move g 3 player1.pion;
Game.move g 2 player2.pion;
Game.move g 2 player1.pion;
Game.move g 3 player2.pion;
Game.move g 3 player1.pion;
Game.move g 2 player2.pion;
(*move g 2 player1;*)
(*move g 3 player2;*)
Game.move g 5 player1.pion;
Game.move g 6 player2.pion;
Game.move g 6 player1.pion;
Game.move g 5 player2.pion;
Game.move g 5 player1.pion;
Game.move g 6 player2.pion;
Game.move g 6 player1.pion;
Game.move g 5 player2.pion;
Game.move g 5 player1.pion;
Game.move g 6 player2.pion;
(*move g 6 player1;*)
(*move g 5 player2;*)
Game.move g 4 player1.pion;
Game.move g 4 player2.pion;
Game.move g 4 player1.pion;
Game.move g 4 player2.pion;
Game.move g 4 player1.pion;;
(* g.tab;; *)

Game.move g 3 player1.pion;;

alphabeta g player1 player2 (-1.) 1.;;
(* g.tab;; *)

let cuple = alphabeta g player1 player2 (-.1.) 1.;;


