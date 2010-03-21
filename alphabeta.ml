type mode = Max | Min;;

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

let win_in_2moves game j color =
  try
    Game.move game j color;
    Game.move game j (Game.color_invers color);

    if Game.is_winning game j then
      (
        Game.remove game j (Game.color_invers color);
        Game.remove game j color;
        false
      )
    else
      (
        Game.remove game j (Game.color_invers color);
        Game.remove game j color;
        Game.move game j (Game.color_invers color);
        Game.move game j color;
        if Game.is_winning game j then
          (
            Game.remove game j color;
            Game.remove game j (Game.color_invers color);
            false
          )
        else
          (
            Game.remove game j color;
            Game.remove game j (Game.color_invers color);
            true
          )
      )
  with Game.Column_full -> false

let h game color mode =
  let col_win_max = Game.next_win game color
  and col_win_min = Game.next_win game (Game.color_invers color) in

  if col_win_max < 7 then (infinity, col_win_max)
  else if col_win_min < 7 then (8., col_win_min)
  else
    (
      let tab_value = [|0.; 0.; 0.; 0.; 0.; 0.; 0.|] in
      for j = 0 to 6 do
        let row = Game.nbr_token_in_col game j in
        if row = 6 then tab_value.(j) <- 0.
        else
          (
            (*if win_in_2moves game j color then*)
              let aline_horiz_max = Game.horizontal game color j
              and aline_vert_max = Game.vertical game color j
              and aline_diag_left_max = Game.left_diagonal game color j
              and aline_diag_right_max = Game.right_diagonal game color j in

              if fst aline_horiz_max >= 4 && snd aline_horiz_max >= 2 then
                tab_value.(j) <- tab_value.(j) +. 4.;
              (*if fst aline_vert_max >= 4 && snd aline_vert_max >= 2 then
                tab_value.(j) <- tab_value.(j) +. 4.;*)
              if fst aline_diag_left_max >= 4 && snd aline_diag_left_max >= 2
              then
                tab_value.(j) <- tab_value.(j) +. 4.;
              if fst aline_diag_left_max >= 4 && snd aline_diag_left_max >= 2
              then
                tab_value.(j) <- tab_value.(j) +. 4.;

              if fst aline_horiz_max >= 4 && snd aline_horiz_max >= 1 then
                tab_value.(j) <- tab_value.(j) +. 2.;
              (*if fst aline_vert_max >= 4 && snd aline_vert_max >= 1 then
                tab_value.(j) <- tab_value.(j) +. 2.;*)
              if fst aline_diag_left_max >= 4 && snd aline_diag_left_max >= 1
              then
                tab_value.(j) <- tab_value.(j) +. 2.;
              if fst aline_diag_right_max >= 4 && snd aline_diag_right_max >= 1
              then
                tab_value.(j) <- tab_value.(j) +. 2.;

              let aline_horiz_min =
                Game.horizontal game (Game.color_invers color) j
              and aline_vert_min =
                Game.vertical game (Game.color_invers color) j
              and aline_diag_left_min =
                Game.left_diagonal game (Game.color_invers color) j
              and aline_diag_right_min =
                Game.right_diagonal game (Game.color_invers color) j in

              if fst aline_horiz_min >= 4 && snd aline_horiz_min >= 2 then
                tab_value.(j) <- tab_value.(j) +. 4.;
              (*if fst aline_vert_min >= 4 && snd aline_vert_min >= 2 then
                tab_value.(j) <- tab_value.(j) +. 4.;*)
              if fst aline_diag_left_min >= 4 && snd aline_diag_left_min >= 2
              then
                tab_value.(j) <- tab_value.(j) +. 4.;
              if fst aline_diag_left_min >= 4 && snd aline_diag_left_min >= 2
              then
                tab_value.(j) <- tab_value.(j) +. 4.;

              if fst aline_horiz_min >= 4 && snd aline_horiz_min >= 1 then
                tab_value.(j) <- tab_value.(j) +. 2.;
              (*if fst aline_vert_min >= 4 && snd aline_vert_min >= 1 then
                tab_value.(j) <- tab_value.(j) +. 2.;*)
              if fst aline_diag_left_min >= 4 && snd aline_diag_left_min >= 1
              then
                tab_value.(j) <- tab_value.(j) +. 2.;
              if fst aline_diag_right_min >= 4 && snd aline_diag_right_min >= 1
              then
                tab_value.(j) <- tab_value.(j) +. 2.;

           (* else tab_value.(j) <- tab_value.(j) -. 16.;*)
          )
      done;
      if mode = Max then max_tab tab_value
      else
        (
          let tab = Array.map (fun x -> (-.x)) tab_value in
          min_tab tab
        )
    )

let alphabeta game color alpha beta level heuristic=
  let n = Game.nbr_token game in
  let rec ab nbr_token col g a b mode l colo =
    if Game.is_winning g col then
      if (mode = Min) then (infinity, col)
      else (neg_infinity, col)

    else if Game.is_draw g then (0., col)
    else if nbr_token < 3 then fst_moves game nbr_token
    else if l = 0 then heuristic game color mode
    else
      (
        if mode = Min then
          let col_win = Game.next_win g colo in
          if col_win < 7 then (neg_infinity, col_win)
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
              in cut_beta infinity col 0
            )
        else
          let col_win = Game.next_win g colo in
          if col_win < 7 then (infinity, col_win)
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
              in cut_alpha neg_infinity col 0
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
  node_max game color alpha beta 1. 0 0;;



