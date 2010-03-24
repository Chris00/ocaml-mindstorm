type mode = Max | Min

let rec ab col game alpha beta mode depth heuristic =
  let result = Gamemem.get_game_result game in
  if result = Gamemem.WIN then
    if mode = Min then (infinity, col)
    else (neg_infinity, col)
  else if result = Gamemem.DRAW then (0., col)
  else if depth = 0 then (heuristic game, col)
  else if mode = Min then
    let rec cut_beta beta_p good_col j =
      if j > 6 then (beta_p, good_col)
      else
        (
          if Gamemem.makemove game j then
            let value, _ =
              ab j game alpha (min beta beta_p) Max (depth-1) heuristic in
            if not (Gamemem.undomove game j) then assert false;

            let (new_beta, new_col) =
              if beta_p > value then (value, j)
              else (beta_p, good_col) in

            if alpha >= new_beta then (new_beta, new_col)
            else cut_beta new_beta new_col (j+1)
          else cut_beta beta_p good_col (j+1)
        )
    in cut_beta infinity col 0

  else
    let rec cut_alpha alpha_p good_col j =
      if j > 6 then (alpha_p, good_col)
      else
        (
          if Gamemem.makemove game j then
            let value, _ =
              ab j game (max alpha alpha_p) beta Min (depth-1) heuristic in
            if not (Gamemem.undomove game j) then assert false;

            let (new_alpha, new_col) =
              if alpha_p < value then (value, j)
              else (alpha_p, good_col) in

            if new_alpha >= beta then (new_alpha, new_col)
            else cut_alpha new_alpha new_col (j+1)
          else  cut_alpha alpha_p good_col (j+1)
        )
    in cut_alpha neg_infinity col 0


let alphabeta game level heuristic =
  Printf.printf "lance alphabeta, nbre pieces ds jeu = %i\n%!"
    game.Gamemem.filled;
  if game.Gamemem.filled = 0 || game.Gamemem.filled = 1 then (0., 3)
  else
    let value, col =
      ab 0 game neg_infinity infinity Max level heuristic in
    let rec what_col_to_play g cost c =
      let n = g.Gamemem.stack.(c) in
      if n <> 6 then (cost, c)
      else what_col_to_play g cost (c+1)
    in what_col_to_play game value col
