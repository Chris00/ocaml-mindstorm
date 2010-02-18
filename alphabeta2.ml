open Game

type player = Human | Computer
type actor = { player : player; pion : int}
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
0.5;;

(*peut etre renvoyer une liste des colonnes a jouer*)
let rec minmax ?(col=0) current_game mode level color =
  if is_winning current_game col then
    if (mode = Max) then 1.
    else -1.

  else if is_draw current_game then 0.

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
            if get_row current_game j < 5 then
              (
                move current_game j color;
                if mode = Max then value := max !value
                  (minmax ~col:j current_game Min (level-1) (match color with
                                              |0 -> 1
                                              |1 -> 0
                                              |_ -> raise (Failure"")))
                else value := min !value (minmax ~col:j current_game Max
                                            (level-1) (match color with
                                                       |0 -> 1
                                                       |1 -> 0
                                                       |_ -> raise (Failure ""))
                                         );
                remove current_game j color
              )
          done;
          !value
        )
    )
;;

let rec alphabeta ?(col=0) current_game alpha beta mode level color =
  if is_winning current_game col then
    if (mode = Max) then 1.
    else -1.

  else if is_draw current_game then 0.

  else
    (
      if level = 0 then h current_game mode color
      else
        (
          let value = ref 0. in

          if mode = Min then
            (
            let beta_p = ref infinity and j = ref 0 in
            while (!j < 7) do
              if get_row current_game !j < 5 then
                (
                  move current_game !j color;
                  value:=alphabeta ~col:!j current_game alpha (min beta !beta_p)
                    Max (level-1) (match color with
                                   |0 -> 1
                                   |1 -> 0
                                   |_ -> raise (Failure ""));
                  beta_p := min !beta_p !value;
                  remove current_game !j color;
                  if alpha >= !beta_p then (j:=7)
                  else j := !j + 1
                )
            done;
            !beta_p
            )

          else
            (
            let alpha_p = ref neg_infinity and j = ref 0 in
            while (!j < 7) do
              if get_row current_game !j < 5 then
                (
                  move current_game !j color;
                  value:= alphabeta ~col:!j current_game (max alpha !alpha_p)
                    beta Min (level-1) (match color with
                                        |0 -> 1
                                        |1 -> 0
                                        |_ -> raise (Failure ""));
                  alpha_p := max !alpha_p !value;
                  remove current_game !j color;
                  if !alpha_p >= beta then (j:=7)
                  else j:= !j + 1
                )
            done;
            !alpha_p
            )
        )
    )
;;

let affich g =
  Printf.printf "\n";
  for j = 0 to 6 do
    for i = 0 to 5 do
      let couleur = match (get g i j) with
        | 2 -> "       "
        | 1 -> " Yellow"
        | 0 -> " Red   " in
      Printf.printf "%s" couleur;
    done;
    Printf.printf"\n"
  done;
  Printf.printf"------------------------------------------\n";;

let aColor pion =
  if pion = 0 then Printf.printf " Yellow"
  else Printf.printf" Red";;

let g = make() and
    player1 = {player = Human; pion = 1}
               and player2 = {player = Human; pion = 0};;

move g 0 player1.pion;
move g 1 player2.pion;
move g 1 player1.pion;
move g 0 player2.pion;
move g 0 player1.pion;
move g 1 player2.pion;
move g 1 player1.pion;
move g 0 player2.pion;
(*move g 0 player1.pion;*)
(*move g 1 player2.pion;*)
move g 3 player1.pion;
move g 2 player2.pion;
move g 2 player1.pion;
move g 3 player2.pion;
move g 3 player1.pion;
move g 2 player2.pion;
move g 2 player1.pion;
move g 3 player2.pion;
(*move g 3 player1.pion;*)
(*move g 2 player2.pion;*)
move g 5 player1.pion;
move g 6 player2.pion;
move g 6 player1.pion;
move g 5 player2.pion;
move g 5 player1.pion;
(*move g 6 player2.pion;*)
(*move g 6 player1.pion;*)
(*move g 5 player2.pion;*)
(*move g 5 player1.pion;*)
move g 6 player2.pion;
move g 4 player1.pion;
move g 4 player2.pion;
move g 4 player1.pion;
(*move g 4 player2.pion;*)
(*move g 4 player1.pion*)
;;
affich g;;
minmax g Max 42 0 ;;
alphabeta g neg_infinity infinity Max 42 0;;
