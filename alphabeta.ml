module Useful =
struct
let max_tab tab =
  let n = Array.length tab in
  let rec max max_current i_max i =
    if i > n-1 then (max_current, i_max)
    else if tab.(i) > max_current then max tab.(i) i (i+1)
    else max max_current i_max (i+1)
  in max 0. 0 0

let min_tab tab =
  let n = Array.length tab in
  let rec min min_current i_min i =
    if i > n-1 then (min_current, i_min)
    else if tab.(i) < min_current then min tab.(i) i (i+1)
    else min min_current i_min (i+1)
  in min 0. 0 0
end

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
  |0 -> (infinity, 3)
  |1 ->
     (
       if Game.comparate game tab_game_1token.(0) = 0
         || Game.comparate game tab_game_1token.(7) = 0 then
           (infinity, 3)
       else if Game.comparate game tab_game_1token.(1) = 0
         || Game.comparate game tab_game_1token.(8) = 0 then
           (infinity, 2)
       else if Game.comparate game tab_game_1token.(2) = 0
         || Game.comparate game tab_game_1token.(9) = 0 then
           (0., 3)
       else (neg_infinity, 3)
     )
  |2 ->
     (
       if Game.comparate game tab_game_2token.(0) = 0
         || Game.comparate game tab_game_2token.(7) = 0
         || Game.comparate game tab_game_2token.(6) = 0
         || Game.comparate game tab_game_2token.(13) = 0 then
           (infinity, 3)
       else if Game.comparate game tab_game_2token.(1) = 0
         || Game.comparate game tab_game_2token.(8) = 0
         ||Game.comparate game tab_game_2token.(5) = 0
         || Game.comparate game tab_game_2token.(12) = 0 then
           (infinity, 1)
       else if Game.comparate game tab_game_2token.(2) = 0
         || Game.comparate game tab_game_2token.(9) = 0 then
           (infinity, 5)
       else if Game.comparate game tab_game_2token.(4) = 0
         || Game.comparate game tab_game_2token.(11) = 0 then
           (infinity, 1)
       else (infinity, 3)
     )
  |_ -> raise (Failure "methode can be used only if the number of moves is
                         minder than 2")

let heuristic game color mode =
  let col_win_max = Game.next_win game color
  and col_win_min = Game.next_win game (Game.color_invers color) in

  if col_win_max < 7 then (infinity, col_win_max)
  else if col_win_min < 7 then (17., col_win_min)
  else (
    let weight = [|0.; 0.; 0.; 0.; 0.; 0.; 0.|] in
    for j = 0 to 6 do
      let row = Game.nbr_token_in_col game j in
      if row = 6 then weight.(j) <- 0.
      else (
        (* Max *)
        let avail_horiz, filled_horiz = Game.horizontal game color j
        and avail_vert, filled_vert = Game.vertical game color j
        and avail_diag_l, filled_diag_l = Game.left_diagonal game color j
        and avail_diag_r, filled_diag_r = Game.right_diagonal game color j in
        if avail_horiz >= 4 then
          if filled_horiz >= 2 then weight.(j) <- weight.(j) +. 4.
          else if filled_horiz >= 1 then weight.(j) <- weight.(j) +. 2.;
        if avail_vert >= 4 then
          if filled_vert >= 2 then weight.(j) <- weight.(j) +. 4.
          else if filled_vert >= 1 then weight.(j) <- weight.(j) +. 2.;
        if avail_diag_l >= 4 then
          if filled_diag_l >= 2 then weight.(j) <- weight.(j) +. 6.
          else if filled_diag_l >= 1 then weight.(j) <- weight.(j) +. 3.;
        if avail_diag_r >= 4 then
          if filled_diag_r >= 2 then weight.(j) <- weight.(j) +. 6.
          else if filled_diag_r >= 1 then weight.(j) <- weight.(j) +. 3.;

        (* Min *)
        let icolor = Game.color_invers color in
        let avail_horiz, filled_horiz = Game.horizontal game icolor j
        and avail_vert, filled_vert = Game.vertical game icolor j
        and avail_diag_l, filled_diag_l = Game.left_diagonal game icolor j
        and avail_diag_r, filled_diag_r = Game.right_diagonal game icolor j in
        if avail_horiz >= 4 then
          if filled_horiz >= 2 then weight.(j) <- weight.(j) +. 4.
          else if filled_horiz >= 1 then weight.(j) <- weight.(j) +. 2.;
        if avail_vert >= 4 then
          if filled_vert >= 2 then weight.(j) <- weight.(j) +. 4.
          else if filled_vert >= 1 then weight.(j) <- weight.(j) +. 2.;
        if avail_diag_l >= 4 then
          if filled_diag_l >= 2 then weight.(j) <- weight.(j) +. 6.
          else if filled_diag_l >= 1 then weight.(j) <- weight.(j) +. 3.;
        if avail_diag_r >= 4 then
          if filled_diag_r >= 2 then weight.(j) <- weight.(j) +. 6.
          else if filled_diag_r >= 1 then weight.(j) <- weight.(j) +. 3.;
      )
    done;
    let (max, imax) as h = Useful.max_tab weight in
    if mode = Max then h else (-. max, imax)
  )

let rec ab nbr_token col game alpha beta mode depth color heuristic =
    if Game.is_winning game col then
      if (mode = Min) then (infinity, col)
      else (neg_infinity, col)

    else if Game.is_draw game then (0., col)
    else if nbr_token < 3 then fst_moves game nbr_token
    else if depth = 0 then heuristic game color mode
    else
      (
        if mode = Min then
          let col_win = Game.next_win game color in
          if col_win < 7 then (neg_infinity, col_win)
          else
            (
              let rec cut_beta beta_p good_col j =
                if j > 6 then (beta_p, good_col)
                else
                  try
                    Game.move game j color;
                    let value, _ =
                       ab (nbr_token+1) j game alpha (min beta beta_p) Max
                             (depth-1) (Game.color_invers color) heuristic in
                    Game.remove game j color;

                    let (new_beta, new_col) =
                      if beta_p > value then (value, j)
                      else (beta_p, good_col) in

                    if alpha >= new_beta then (new_beta, new_col)
                    else cut_beta new_beta new_col (j+1)
                  with Game.Column_full -> cut_beta beta_p good_col (j+1)
              in cut_beta infinity col 0
            )
        else
          let col_win = Game.next_win game color in
          if col_win < 7 then (infinity, col_win)
          else
            (
              let rec cut_alpha alpha_p good_col j =
                if j > 6 then (alpha_p, good_col)
                else
                  try
                    Game.move game j color;
                    let value, _ =
                      ab (nbr_token+1) j game (max alpha alpha_p) beta Min
                             (depth-1) (Game.color_invers color) heuristic in
                    Game.remove game j color;

                    let (new_alpha, new_col) =
                      if alpha_p < value then (value, j)
                      else (alpha_p, good_col) in

                    if new_alpha >= beta then (new_alpha, new_col)
                    else cut_alpha new_alpha new_col (j+1)
                  with Game.Column_full -> cut_alpha alpha_p good_col (j+1)
              in cut_alpha neg_infinity col 0
            )
      )

let alphabeta game color level heuristic=
  let n = Game.nbr_token game in
  let value, col = ab n 0 game neg_infinity infinity Max level color heuristic in
  let rec what_col_to_play g cost column =
    let n_in_col = Game.nbr_token_in_col g column in
    if n_in_col <> 6 then (cost, column)
    else if Game.nbr_token_in_col g 3 <> 6 then
      (cost, 3)
    else if Game.nbr_token_in_col g 2 <> 6 then
      (cost, 2)
    else if Game.nbr_token_in_col g 4 <> 6 then
      (cost, 4)
    else what_col_to_play g cost (column+1)
  in what_col_to_play game value col


(* Return [(beta', col)] where [beta'] is the "cost" of this node and
   [col] is a column to play to achieve that cost. *)
let rec node_min game color alpha beta depth heuristic =
  if depth = 0 then heuristic game color Min
  else
    (* Check whether a winning (⇒ extremal) position can be achieved in
       one move. *)
    let col_win = Game.next_win game color in
    if col_win < 7 then (neg_infinity, col_win)
    else node_min_iter game color alpha beta depth heuristic 0 0

and node_min_iter game color alpha beta_cur depth heuristic good_col j =
  if j > 6 then (beta_cur, good_col)
  else if Game.is_winning game j then (infinity, j)
  else if Game.is_draw game then (0., j)
  else (
    try
      Game.move game j color;
      let beta, _ = (node_max game (Game.color_invers color) alpha beta_cur
                       (depth-1) heuristic) in
      Game.remove game j color;
      if beta < beta_cur then (* new min playing [j] *)
        if beta <= alpha then (beta, j)
        else node_min_iter game color alpha beta depth heuristic j (j+1)
      else
        node_min_iter game color alpha beta_cur depth heuristic good_col (j+1)
    with Game.Column_full ->
      node_min_iter game color alpha beta_cur depth heuristic good_col (j+1)
  )

(* Return [(alpha', col)] where [alpha'] is the "cost" of this node
   and [col] is a column to play to achive that cost. *)
and node_max game color alpha beta depth heuristic =
  if depth = 0 then heuristic game color Max
  else
    let col_win = Game.next_win game color in
    if col_win < 7 then (infinity, col_win)
    else node_max_iter game color alpha beta depth heuristic 0 0

and node_max_iter game color alpha_cur beta depth heuristic good_col j =
  if  j > 6 then (alpha_cur, good_col)
  else if Game.is_winning game j then (neg_infinity, j)
  else if Game.is_draw game then (0., j)
  else (
    try
      Game.move game j color;
      let alpha,_ = (node_min game (Game.color_invers color) alpha_cur beta
                       (depth-1) heuristic) in
      Game.remove game j color;
      if alpha > alpha_cur then (* new max playing [j] *)
        if alpha >= beta then (alpha, j)
        else node_max_iter game color alpha beta depth heuristic j (j+1)
      else
        node_max_iter game color alpha_cur beta depth heuristic good_col (j+1)
    with Game.Column_full ->
      node_max_iter game color alpha_cur beta depth heuristic good_col (j+1)
  )

let alphabetabis game color depth heuristic =
  node_max game color neg_infinity infinity depth heuristic
