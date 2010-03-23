type mode = Max | Min

let rec ab col game alpha beta mode depth heuristic =
    if Gamemem.connected game col >= 4 then (infinity, col)
    else if Gamemem.opponent_connected game col >= 4 then (neg_infinity, col)
    else if Gamemem.draw game then (0., col)
    else if depth = 0 then (heuristic game, col)
    else if mode = Min then
      let rec cut_beta beta_p good_col j =
        if j > 6 then (beta_p, good_col)
        else
          (
            if Gamemem.makemove game j then
              let value, _ =
                ab j game alpha (min beta beta_p) Max (depth-1) heuristic in
              Gamemem.undomove game j;

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
              Gamemem.undomove game j;

              let (new_alpha, new_col) =
                if alpha_p < value then (value, j)
                else (alpha_p, good_col) in

              if new_alpha >= beta then (new_alpha, new_col)
              else cut_alpha new_alpha new_col (j+1)
            else  cut_alpha alpha_p good_col (j+1)
          )
      in cut_alpha neg_infinity col 0


let alphabeta game level heuristic=
  ab 0 game neg_infinity infinity Max level heuristic

let bb = Gamemem.make_board();;
Gamemem.initboard bb;
Gamemem.makemove bb 0;;
Gamemem.makemove bb 1;;
Gamemem.makemove bb 0;;
Gamemem.makemove bb 1;;
Gamemem.makemove bb 0;;

let cost, col = alphabeta bb 8 Gamemem.groupeval;;
Printf.printf "cost = %f   colum, = %i\n" cost col;;
