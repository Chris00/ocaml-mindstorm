type mode = Max | Min;;


let h current_game mode color =
 (*le joueur a des pieces dans le jeu, on va calculer le nombre de possibilité
qu'il a, a partir du jeu courrant, de faire des alignements. on va ensuite
calculer le nombre d'alignement possible de l'autre joueur
on renverra un nombre proche de 1 si le nombre d'alignement de notre joueur est
plus eleve que l'autre
attention il faut verifier qu'au prochain coup l'autre joueur ne gagne pas.
On favorise aussi les pions sur la colonne du milieu
*)

(*Verifier si on gagne au prochain coup dans ce cas on renvoie la colonne*)
(*verifier si l'adversaire a un alignement de 3 et qu'il peut en faire un de 4,
dans ce cas renvoie la colonne pour que l'alignement ne se fasse pas*)
0.5;;


let rec minmax ?(col=0) current_game mode level color =
  if Game.is_winning current_game col then
    if (mode = Max) then 1.
    else -1.

  else if Game.is_draw current_game then 0.

  else
    (
      if level = 0 then h current_game mode color
      else
        (
          let value = match mode with
            |Max -> ref neg_infinity
            |Min -> ref infinity
          in

          for j = 0 to 6 do
            if Game.get_row current_game j < 5 then
              (
                Game.move current_game j color;
                if mode = Max then value := max !value
                  (minmax ~col:j current_game Min (level-1)
                     (match color with
                      |1 -> 0
                      |0 -> 1
                      |_ -> raise (Failure "")))

                else value := min !value (minmax ~col:j current_game Max (level-1)
                                     (match color with
                                      |1 -> 0
                                      |0 -> 1
                                      |_ -> raise (Failure "")))
              )
          done;
          !value
        )
    )
;;

let rec alphabeta ?(col=0) current_game alpha beta mode level color =
  if Game.is_winning current_game col then
    if (mode = Max) then 1.
    else -1.

  else if Game.is_draw current_game then 0.

  else
    (
      if level = 0 then h current_game mode color
      else
        (
          let value = ref 0. in

          if mode = Min then
            let beta_p = ref infinity and j = ref 0 in
            while (!j < 7) do
              if Game.get_row current_game !j < 5 then
                (
                  Game.move current_game !j color;
                  value:=alphabeta ~col:!j current_game alpha (min beta !beta_p)
                    Max (level-1) (match color with
                                   |0 -> 1
                                   |1 -> 0
                                   |_ -> raise (Failure ""));
                  beta_p := min !beta_p !value;
                  Game.remove current_game !j color;
                  if alpha >= !beta_p then j := 7
                  else j := !j + 1;
                  )
            done;
            !beta_p

          else
            let alpha_p = ref neg_infinity and j = ref 0 in
            while ( !j < 7) do
              if Game.get_row current_game !j < 5 then
                (
                  Game.move current_game !j color;
                  value:= alphabeta ~col:!j current_game (max alpha !alpha_p)
                    beta Min (level-1) (match color with
                                        |0 -> 1
                                        |1 -> 0
                                        |_ -> raise (Failure ""));
                  alpha_p := max !alpha_p !value;
                  Game.remove current_game !j color;
                  if !alpha_p >= beta then (j := 7)
                  else j := !j + 1
                )
            done;
            !alpha_p
        )
    )
;;
(*
let rec alphabeta ?(col=0) current_game alpha beta mode level color =
  if Game.is_winning current_game col then
    if (mode = Max) then 1.
    else -1.

  else if Game.is_draw current_game then 0.

  else
    (
      if level = 0 then h current_game mode color
      else
        (
          let value = ref 0. in

          if mode = Min then
            let rec cut_alpha b colonne iter =
              if iter < 7 then
                (
                  if Game.get_row current_game iter < 5 then
                    (
                      Game.move current_game iter color;
                      value := alphabeta ~col:iter current_game alpha
                        (min beta b) Max (level-1) (match color with
                                                    |1 -> 0
                                                    |0 -> 1
                                                    |_ -> raise (Failure ""));
                      let new_b = min b !value in
                      if alpha >= new_b then new_b
                      else cut_alpha new_b iter (iter+1)
                    )
                  else cut_alpha b colonne (iter+1)
                )
              else (Game.remove current_game colonne color;
                    b)
            in cut_alpha infinity 0 0;

          else
            let rec cut_beta a colonne iter =
              if iter < 7 then
                (
                  if Game.get_row current_game iter < 5 then
                    (
                      Game.move current_game iter color;
                      value := alphabeta ~col:iter current_game (max alpha a)
                        beta Min (level-1) (match color with
                                            |1 -> 0
                                            |0 -> 1
                                            |_ -> raise (Failure ""));
                      let new_a = max a !value in
                      if new_a >= beta then new_a
                      else cut_beta new_a iter (iter+1)
                    )
                  else cut_beta a colonne (iter+1)
                )
              else (Game.remove current_game colonne color;
                    a)
            in cut_beta neg_infinity 0 0
        )
    )
;;*)




let g = Game.make();;
Game.move g 0 0;;
Game.move g 1 1;;
Game.move g 1 0;;
(*Game.move g 0 1;;*)
(*Game.move g 0 0;;*)
(*Game.move g 1 1;;*)
(*Game.move g 1 0;;*)
Game.move g 0 1;;
Game.move g 0 0;;
Game.move g 1 1;;
Game.move g 3 0;;
Game.move g 2 1;;
Game.move g 2 0;;
Game.move g 3 1;;
Game.move g 3 0;;
(*Game.move g 2 1;;*)
(*Game.move g 2 0;;*)
(*Game.move g 3 1;;*)
(*Game.move g 3 0;;*)
Game.move g 2 1;;
Game.move g 5 0;;
Game.move g 6 1;;
Game.move g 6 0;;
Game.move g 5 1;;
Game.move g 5 0;;
(*Game.move g 6 1;;*)
(*Game.move g 6 0;;*)
(*Game.move g 5 1;;*)
(*Game.move g 5 0;;*)
Game.move g 6 1;;
Game.move g 4 0;;
Game.move g 4 1;;
Game.move g 4 0;;
(*Game.move g 4 1;;*)
(*Game.move g 4 0;;*)

let a = minmax g Max 42 1;;
Printf.printf "%i" (int_of_float a);;
Printf.printf "\n";;
let b = alphabeta g neg_infinity infinity Max 42 1;;
Printf.printf "%i" (int_of_float b);;
Printf.printf "\n";;



(*
let rec alphabeta current_game gamer actor1 actor2 alpha beta =
  if Game.number_of_moves current_game <> 0 && Game.is_winning current_game then
    if (gamer) then (1., Game.last_move_col current_game)
    else (-1., Game.last_move_col current_game)

  else if Game.number_of_moves current_game = 42 then
    (0., Game.last_move_col current_game)
  else
    (
      let game = Game.copy current_game in
      if gamer then
        (*faire un remove???*)
        let rec cut_beta a b col i =
          if i < 7 then
            (
              if Game.npieces_col game i < 6 then
                (
                  Game.move game i actor1.pion;
                  let value = fst (alphabeta game (not gamer) actor2 actor1
                                     (max alpha a) b) in
                  let (a_temp, col_temp) =
                    if a > value then (a, col)
                    else (value, i) in
                  if a_temp >= beta then (a_temp, col_temp)
                  else cut_beta a_temp b col_temp (i+1)
                )
              else cut_beta a b col (i+1);
            )
          else (a, col) in
        cut_beta neg_infinity infinity 0 0;

      else let rec cut_alpha a b col i =
        if i < 7 then
          (
            if Game.npieces_col game i < 6 then
              (
                Game.move game i actor1.pion;
                let value = fst (alphabeta game (not gamer) actor2 actor1
                                        a (min beta b)) in
                let (b_temp, col_temp) =
                  if b < value then (b, col) else (value, i) in
                if alpha >= b_temp then (b_temp, col_temp)
                else cut_alpha a b_temp col_temp (i+1)
              )
            else cut_alpha a b col (i+1);
          )
        else (b, col) in
      cut_alpha neg_infinity infinity 0 0
    )
;;
*)




