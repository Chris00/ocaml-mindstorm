type mode = Max | Min;;

let tab_game_1token =
  let tab = Array.init 14 (fun i -> Game.make()) in
  for i = 0 to 6 do
    Game.move tab.(i) i Game.Red;
    Game.move tab.(i+7) i Game.Yellow
  done;
  tab

let tab_game_2token =
  let tab = Array.init 14 (fun i -> Game.make()) in
  for i = 0 to 6 do
    Game.move (tab.(i)) 3 Game.Red;
    Game.move (tab.(i)) i Game.Yellow;
  done;
  for i = 7 to 13 do
    Game.move (tab.(i)) 3 Game.Yellow;
    Game.move (tab.(i)) (i mod 7) Game.Red;
  done;
  tab

let fst_moves game nbr_moves =
  match nbr_moves with
  |0 -> (1., 3)
  |1 ->
     (
       if Game.comparate game tab_game_1token.(0) = 0
         || Game.comparate game tab_game_1token.(7) = 0 then
           (1., 3)
       else if Game.comparate game tab_game_1token.(1) = 0
         || Game.comparate game tab_game_1token.(8) = 0 then
           (1., 2)
       else if Game.comparate game tab_game_1token.(2) = 0
         || Game.comparate game tab_game_1token.(9) = 0 then
           (0., 3)
       else (-1., 3)
     )
  |2 ->
     (
       if Game.comparate game tab_game_2token.(0) = 0
         || Game.comparate game tab_game_2token.(7) = 0
         || Game.comparate game tab_game_2token.(6) = 0
         || Game.comparate game tab_game_2token.(13) = 0 then
           (1., 3)
       else if Game.comparate game tab_game_2token.(1) = 0
         || Game.comparate game tab_game_2token.(8) = 0
         ||Game.comparate game tab_game_2token.(5) = 0
         || Game.comparate game tab_game_2token.(12) = 0 then
           (1., 1)
       else if Game.comparate game tab_game_2token.(2) = 0
         || Game.comparate game tab_game_2token.(9) = 0 then
           (1., 5)
       else if Game.comparate game tab_game_2token.(4) = 0
         || Game.comparate game tab_game_2token.(11) = 0 then
           (1., 1)
       else (1., 3)
     )

let alphabeta game color alpha beta level =
  let n = Game.nbr_token game in
  let rec ab nbr_token col g a b mode l colo =
    if Game.is_winning g col then
      if (mode = Min) then (1., col)
      else (-1., col)

    else if Game.is_draw g then (0., col)
    else if nbr_token < 3 then fst_moves game nbr_token
    else
      (
        if mode = Min then
          let col_win = Game.next_win g colo in
          if col_win < 7 then (-1., col_win)
          else
            (
              let rec cut_beta beta_p good_col j =
                if j > 6 then (beta_p, good_col)
                else
                  try
                    Game.move g j colo;
                    let value =
                      fst (ab (nbr_token+1) j g a (min b beta_p) Max (l-1)
                                       (Game.color_invers colo)) in
                    Game.remove g j colo;

                    let (new_beta, new_col) =
                      if beta_p > value then (value, j)
                      else (beta_p, good_col) in

                    if a >= new_beta then (new_beta, new_col)
                    else cut_beta new_beta new_col (j+1)
                  with Game.Column_full -> cut_beta beta_p good_col (j+1)
              in cut_beta 1. col 0
            )
        else
          let col_win = Game.next_win g colo in
          if col_win < 7 then (1., col_win)
          else
            (
              let rec cut_alpha alpha_p good_col j =
                if j > 6 then (alpha_p, good_col)
                else
                  try
                    Game.move g j colo;
                    let value =
                      fst (ab (nbr_token+1) j g (max a alpha_p) b Min (l-1)
                                       (Game.color_invers colo)) in
                    Game.remove g j colo;

                    let (new_alpha, new_col) =
                      if alpha_p < value then (value, j)
                      else (alpha_p, good_col) in

                    if new_alpha >= beta then (new_alpha, new_col)
                    else cut_alpha new_alpha new_col (j+1)
                  with Game.Column_full -> cut_alpha alpha_p good_col (j+1)
              in cut_alpha (-1.) col 0
            )
      )
  in ab n 0 game alpha beta Max level color

(*probleme avec good_col qui quand il revient retourne forcement 0*)
let rec node_min game color alpha beta beta_p good_col j =
  if Game.is_winning game good_col then
    (1., good_col)

  else if Game.is_draw game then (0., good_col)

  else
    let col_win = Game.next_win game color in
    if col_win < 7 then (-1., col_win)
    else
      (
        if j > 6 then (beta_p, good_col)
        else
          (
            try
              (
                Game.move game j color;
                let value = fst (node_max game (Game.color_invers color) alpha
                                   (min beta beta_p) 1. j 0) in

                Game.remove game j color;
                let (new_beta, new_col) =
                  if beta_p > value then (value, j)
                  else (beta_p, good_col) in
                if alpha >= new_beta then (new_beta, new_col)
                else node_min game color alpha beta new_beta new_col (j+1)
              )
            with Game.Column_full -> node_min game color alpha beta beta_p
              good_col (j+1)
          )
      )


and node_max game color alpha beta alpha_p good_col j =
  if Game.is_winning game good_col then
    (-1., good_col)

  else if Game.is_draw game then (0., good_col)

  else
    let col_win = Game.next_win game color in
    if col_win < 7 then (1., col_win)
    else
      (
        if j > 6 then (alpha_p, good_col)
        else
          (
            try
              (
                Game.move game j color;
                let value = fst (node_min game (Game.color_invers color)
                                   (max alpha alpha_p) beta
                                   (-1.) j 0) in

                Game.remove game j color;
                let (new_alpha, new_col) =
                  if alpha_p < value then (value, j)
                  else (alpha_p, good_col) in
                if new_alpha >= beta then (new_alpha, new_col)
                else node_max game color alpha beta new_alpha new_col (j+1)
              )
            with Game.Column_full -> node_max game color alpha beta alpha_p
              good_col (j+1)
          )
      )

let alphabetabis game color alpha beta =
  node_max game color alpha beta 1. 0 0

(*
let g = Game.make();;
Game.move g 3 Game.Yellow;;
Game.move g 3 Game.Red;;
Game.move g 3 Game.Yellow;;

Useful.print g;;

let a = alphabeta g Game.Red (-1.) 1. 32;;
Printf.printf "%f" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;*)

(*Game.move g 3 Game.Red;;
Game.move g 3 Game.Yellow;;
Game.move g 1 Game.Red;;
Game.move g 0 Game.Yellow;;
Game.move g 0 Game.Red;;
Game.move g 1 Game.Yellow;;
Game.move g 1 Game.Red;;
Game.move g 0 Game.Yellow;;
Game.move g 0 0;
Game.move g 1 1;
Game.move g 3 Game.Red;;
Game.move g 2 Game.Yellow;;
Game.move g 2 Game.Red;;
Game.move g 3 Game.Yellow;;
Game.move g 3 Game.Red;;
Game.move g 2 Game.Yellow;;
Game.move g 2 Game.Red;
Game.move g 3 Game.Yellow;;
Game.move g 3 0;
Game.move g 2 1;
Game.move g 5 Game.Red;;
Game.move g 6 Game.Yellow;;
Game.move g 6 Game.Red;;
Game.move g 5 Game.Yellow;;
Game.move g 5 0;
Game.move g 6 Game.Yellow;;
Game.move g 6 0;
Game.move g 5 Game.Yellow;;
Game.move g 5 0;
Game.move g 6 1;
Game.move g 4 Game.Red;
Game.move g 4 Game.Yellow;
Game.move g 4 Game.Red;
Game.move g 4 1;
Game.move g 4 *)

(*let a = alphabeta g Game.Red (-1.) 1. 42;;
Printf.printf "%f" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;*)

(*Printf.printf "%f" (fst (alphabeta g Game.Yellow (-1.) 1. 42))*)
