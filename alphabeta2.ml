type player = Human | Computer
type actor = {player : player; pion : slot_content}
type mode = Max | Min;;


let h current_game =
 (*le joueur a des pieces dans le jeu, on va calculer le nombre de possibilité
qu'il a, a partir du jeu courrant, de faire des alignements. on va ensuite
calculer le nombre d'alignement possible de l'autre joueur
on renverra un nombre proche de 1 si le nombre d'alignement de notre joueur est
plus eleve que l'autre
attention il faut verifier qu'au prochain coup l'autre joueur ne gagne pas.
On favorise aussi les pions sur la colonne du milieu
*)
0.5;;

(*peut etre renvoyer une liste des colonnes a jouer*)
let rec minmax current_game mode level color =
  if number_of_moves current_game <> 0 && is_winning current_game then
    if (mode = Max) then 1.
    else -1.

  else if number_of_moves current_game = 42 then 0.

  else
    (
      if level = 0 then h current_game
      else
        (
          let value = match mode with
            |Max -> ref neg_infinity
            |Min -> ref infinity
          in
          (*probleme de copy du jeu*)

          for i = 0 to 6 do
            if npieces_col current_game i < 6 then
              (
                let game = copy current_game in
                move game col color;
                if mode = Max then value := max !value
                  (minmax game Min (level-1) (match color with
                                              |Yellow -> Red
                                              |Red -> Yellow
                                              |Empty -> Empty
                                                 (*narrive pas*)))
                else value := min !value (minmax game Max (level-1)
                                            (match color with
                                             |Yellow -> Red
                                             |Red -> Yellow
                                             |Empty -> Empty))
              )
          done;
            !value
        )
    )

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

let affich g =
  Printf.printf "\n";
  for i = 0 to 6 do
    for j = 0 to 5 do
      let couleur = match (g.tab.(i).(j)) with
        | Empty -> "       "
        | Yellow -> " Yellow"
        | Red -> " Red   " in
      Printf.printf "%s" couleur;
    done;
    Printf.printf"\n"
  done;
  Printf.printf"------------------------------------------\n";;

let aColor pion =
  if pion = Yellow then Printf.printf " Yellow"
  else Printf.printf" Red";;

let g = make();;
affich g;;
let player1 = {player = Human; pion = Yellow};;
let player2 = {player = Human; pion = Red};;


move g 0 player1.pion;
move g 1 player2.pion;
move g 1 player1.pion;
move g 0 player2.pion;
move g 0 player1.pion;
move g 1 player2.pion;
move g 1 player1.pion;
move g 0 player2.pion;
move g 0 player1.pion;
move g 1 player2.pion;
(*move g 1 player1;*)
(*move g 0 player2;*)
move g 3 player1.pion;
move g 2 player2.pion;
move g 2 player1.pion;
move g 3 player2.pion;
move g 3 player1.pion;
move g 2 player2.pion;
move g 2 player1.pion;
move g 3 player2.pion;
move g 3 player1.pion;
move g 2 player2.pion;
(*move g 2 player1;*)
(*move g 3 player2;*)
move g 5 player1.pion;
move g 6 player2.pion;
move g 6 player1.pion;
move g 5 player2.pion;
move g 5 player1.pion;
move g 6 player2.pion;
move g 6 player1.pion;
move g 5 player2.pion;
move g 5 player1.pion;
move g 6 player2.pion;
(*move g 6 player1;*)
(*move g 5 player2;*)
move g 4 player1.pion;
move g 4 player2.pion;
move g 4 player1.pion;
move g 4 player2.pion;
move g 4 player1.pion;;
(* g.tab;; *)

move g 3 player1.pion;;

g.tab;;
affich g;;

minmax g Max 42 Red;;
