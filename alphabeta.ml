type mode = Max | Min;;


let rec next_win game color j =
  if j > 6 then j
  else
    try
      Game.move game j color;
      if Game.is_winning game j then
        (
          Game.remove game j color;
          j
        )
      else
        (
          Game.remove game j color;
          next_win game color (j+1)
        )
    with Game.Column_full -> (next_win game color (j+1))

let rec alphabeta ?(col=0) game alpha beta mode level color =
  if Game.is_winning game col then
    if (mode = Min) then (1., col)
    else (-1., col)

  else if Game.is_draw game then (0., col)

  else
    (
      if mode = Min then
        let col_win = next_win game color 0 in
        if col_win < 7 then (-1., col_win)
        else
          (
            let rec cut_beta beta_p good_col j =
              if j > 6 then (beta_p, good_col)
              else
                try
                  Game.move game j color;
                  let value = fst (alphabeta ~col:j game alpha
                                     (min beta beta_p) Max (level-1)
                                     (match color with
                                      |0 -> 1
                                      |1 -> 0
                                      |_ -> raise (Failure ""))) in
                  Game.remove game j color;

                  let (new_beta, new_col) =
                    if beta_p > value then (value, j)
                    else (beta_p, good_col) in

                    if alpha >= new_beta then (new_beta, new_col)
                    else cut_beta new_beta new_col (j+1)
                with Game.Column_full -> cut_beta beta_p good_col (j+1)
            in cut_beta 1. col 0
          )
      else
        let col_win = next_win game color 0 in
        if col_win < 7 then (1., col_win)
        else
          (
            let rec cut_alpha alpha_p good_col j =
              if j > 6 then (alpha_p, good_col)
              else
                try
                  Game.move game j color;
                  let value = fst (alphabeta ~col:j game (max alpha alpha_p)
                                     beta Min (level-1)
                                     (match color with
                                      |0 -> 1
                                      |1 -> 0
                                      |_ -> raise (Failure ""))) in
                  Game.remove game j color;

                  let (new_alpha, new_col) =
                    if alpha_p < value then (value, j)
                    else (alpha_p, good_col) in

                  if new_alpha >= beta then (new_alpha, new_col)
                  else cut_alpha new_alpha new_col (j+1)
                with Game.Column_full -> cut_alpha alpha_p good_col (j+1)
            in cut_alpha (-1.) col 0
          )
    )

let print game =
  Printf.printf "\n";
  for j = 0 to 6 do
    for i = 0 to 5 do
      let couleur = match (Game.get game i j) with
        | 2 -> "       "
        | 1 -> " Yellow"
        | 0 -> " Red   "
        | _ -> raise (Failure "") in
      Printf.printf "%s" couleur;
    done;
    Printf.printf"\n"
  done;
  Printf.printf"------------------------------------------\n"

let g = Game.make();;

Game.move g 0 0;;
Game.move g 1 1;;
Game.move g 1 0;;
Game.move g 0 1;;
Game.move g 0 0;;
Game.move g 1 1;;
Game.move g 1 0;;
Game.move g 0 1;;
Game.move g 0 0;;
Game.move g 1 1;;
Game.move g 3 0;;
Game.move g 2 1;;
Game.move g 2 0;;
Game.move g 3 1;;
Game.move g 3 0;;
Game.move g 2 1;;
Game.move g 2 0;;
Game.move g 3 1;;
Game.move g 3 0;;
Game.move g 2 1;;
Game.move g 5 0;;
Game.move g 6 1;;
Game.move g 6 0;;
Game.move g 5 1;;
Game.move g 5 0;;
Game.move g 6 1;;
Game.move g 6 0;;
Game.move g 5 1;;
Game.move g 5 0;;
Game.move g 6 1;
Game.move g 4 0;
Game.move g 4 1;
Game.move g 4 0;
Game.move g 4 1;
Game.move g 4 0;;

print g;;

let a = alphabeta g (-1.) 1. Max 42 0;;
Printf.printf "%f" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;
