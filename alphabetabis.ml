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


let rec minmax current_game mode level color =
  if Gamebis.number_of_moves current_game <> 0 &&
    Gamebis.is_winning current_game then
    if (mode = Max) then 1.
    else -1.

  else if Gamebis.number_of_moves current_game = 42 then 0.

  else
    (
      if level = 0 then h current_game mode color
      else
        (
          let value = match mode with
            |Max -> ref neg_infinity
            |Min -> ref infinity
          in

          (*probleme de copy du jeu*)

          for i = 0 to 6 do
            if Gamebis.npieces_col current_game i < 6 then
              (
                let game = Gamebis.copy current_game in
                Gamebis.move game i color;
                if mode = Max then value := max !value
                  (minmax game Min (level-1)
                     (match color with
                      |Gamebis.Yellow -> Gamebis.Red
                      |Gamebis.Red -> Gamebis.Yellow
                      |Gamebis.Empty -> Gamebis.Empty (*narrive pas*)))

                else value := min !value (minmax game Max (level-1)
                                     (match color with
                                      |Gamebis.Yellow -> Gamebis.Red
                                      |Gamebis.Red -> Gamebis.Yellow
                                      |Gamebis.Empty -> Gamebis.Empty))
              )
          done;
          !value
        )
    )
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




