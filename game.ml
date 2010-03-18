open Bigarray

(*un pion est rouge ou jaune, ou, dans le plateau, aucun pion*)
(* Yellow = 1, Red = 0*)
type color = Yellow | Red

let color_invers color =
  if color = Yellow then Red
  else Yellow

(*expo est le tableau des puissances de 2 jusque expo 21
  nbr_pieces est le tableau qui retourne le nombre de piece selon la colonne*)
let (expo, nbr_pieces) =
  let a = Array.make 22 1 and b = Array.make 128 0 in
  for i=1 to 21 do
    a.(i) <- a.(i-1)*2;
    b.(i) <- int_of_float ((log (float_of_int i))/. (log 2.))
  done;
  for i=22 to 127 do
    b.(i) <- int_of_float ((log (float_of_int i))/. (log 2.))
  done;
  (a,b)

(*tableau utile pour connaitre la colonne*)
let shift_tab = [|21;14;7;0;14;7;0|]

(*tableau qui retourne la colonne selon une colonne et le piont qu'on ajoute*)
let col_add =
    let a = Array2.create int c_layout 64 2 in
    for col = 0 to 63 do
      a.{col, 0} <- col + expo.(nbr_pieces.(col));
      a.{col, 1} <- col + expo.(nbr_pieces.(col)+1)
    done;
    a

(*même principe mais pour retirer un pion*)
let col_remove =
  let a = Array2.create int c_layout 128 2 in
  for col = 2 to 127 do
    a.{col, 0} <- col - expo.(nbr_pieces.(col)-1);
    a.{col, 1} <- col - expo.(nbr_pieces.(col))
  done;
  a

(*une colonne est represente sur 7 bits :
  1------ veut dire que la colonne est rempli, les tiré sont rempacés par
  1 si c'est un pion jaune ou par 0 si c'est un pion rouge
  01----- la colonne a 5 pion
  001----la colonne a 4 pion
  ...
  0000001 la colonne est vide*)
type t =
    {
      mutable cols_left : int;
      mutable cols_right : int
    }

let make() =
  {
    cols_left = 0x204081;
    cols_right = 0x204081
  }

let copy game =
{
  cols_left = game.cols_left;
  cols_right = game.cols_right
}

(*compare game avec other game, retourne 1 si game est plus petit que other_game
  -1 si game est plus grand, ou 0 si les deux parties sont egales,
  selon l'ordre lexicographique*)
let comparate game other_game =
  if game.cols_left < other_game.cols_left then -1
  else if game.cols_left > other_game.cols_left then 1
  else if game.cols_right < other_game.cols_right then -1
  else if game.cols_right > other_game.cols_right then 1
  else 0

(*retourne la colonne j sous forme d'entier*)
let get_col game j =
  if j<4 then (game.cols_left lsr shift_tab.(j)) land 0x7F
  else if j<7 then (game.cols_right lsr shift_tab.(j)) land 0x7F
  else raise
    (invalid_arg "Game.get_col : enter a number between 0 to 6 for the collumn")

(*on veut la ligne de la derniere piece dans la colonne j*)
let get_row game j =
  (nbr_pieces.(get_col game j)-1)

let nbr_token_in_col game j =
  (nbr_pieces.(get_col game j))

(*calcule le nombre de pieces dans une partie*)
let nbr_token game =
  nbr_pieces.(get_col game 0)+nbr_pieces.(get_col game 1)+
    nbr_pieces.(get_col game 2)+nbr_pieces.(get_col game 3)+
    nbr_pieces.(get_col game 4)+nbr_pieces.(get_col game 5)+
    nbr_pieces.(get_col game 6)

(* on conte a partir de 0*)
(* i est la ligne, j est la colonne*)
let get game i j =
  let col = get_col game j in
  if i < 0 or i > 5 or j < 0 or j > 6 or (nbr_pieces.(col)) <= i then 2
  else (col lsr i) land 1

(*retourne la couleur option*)
let get_color game i j =
  let col = get_col game j in
  if i < 0 or i > 5 or j < 0 or j > 6 then
    raise (invalid_arg
             "Game.get : i must be between 0 to 5 and j must be between 0 to 6")
  else if nbr_pieces.(col) <= i then None
  else
    let piece = (col lsr i) land 1 in
    if piece = 1 then Some Yellow
    else Some Red

exception Column_full
exception Column_empty

(*on ajoute un pion color dans le colonne j*)
let move game j color =
  let col = get_col game j in
  if col >= 64 then raise Column_full;

  let new_col =
    if color = Red then col_add.{col,0}
    else col_add.{col,1} in

  if j < 3 then game.cols_left <- game.cols_left + (new_col-col) lsl ((3-j)*7)
  else if j > 3 then
    game.cols_right <- game.cols_right + (new_col-col) lsl ((6-j)*7)
  else
    (
      game.cols_left <- game.cols_left + (new_col - col);
      game.cols_right <- game.cols_right + (new_col - col) lsl 21
    )

let remove game j color =
  let col = get_col game j in
  if col < 2 then raise Column_empty;
  let new_col =
    if color = Red then col_remove.{col, 0}
    else col_remove.{col, 1} in

  if j < 3 then game.cols_left <- game.cols_left + (new_col-col) lsl ((3-j)*7)
  else if j > 3 then
    game.cols_right <- game.cols_right + (new_col-col) lsl ((6-j)*7)
  else
    (
      game.cols_left <- game.cols_left + (new_col - col);
      game.cols_right <- game.cols_right + (new_col - col) lsl 21
    )

let reset game =
  game.cols_left <- 0x204081;
  game.cols_right <- 0x204081

let is_draw game =
  (game.cols_left land 0x8102040 = 0x8102040
      && game.cols_right land 0x8102040 = 0x8102040)

let rec win cur_zero cur_one act pos =
  if cur_zero = 4 or cur_one = 4 then true
  else if ((act - cur_zero) > 3 && (act - cur_one) > 3) then false
  else if pos mod 10 = 0 then win (cur_zero+1) 0 (act+1) (pos/10)
  else if pos mod 10 = 1 then win 0 (cur_one+1) (act+1) (pos/10)
  else win 0 0 (act+1) (pos/10)

(*on regarde si on a gagne dans la colonne j ou lon vient de jouer*)
let is_winning game j =
  let col = get_col game j and
      pos = ref 0 in
  (*cas vertical*)
  if col = 31 || col = 62 || col = 124 || col = 16 || col = 33 || col = 67
  then true
  else
    (
      (*cas horizontal*)
      let i = (nbr_pieces.(col) - 1) in
      for k = 0 to 6 do
        pos := !pos * 10 + get game i k;
      done;
      if win 0 0 0 !pos then true
      else
        (
          (*cas diagonal /*)
          pos := 0;
          for k = 0 to 6 do
            if i - j + k < 0 || i - j + k > 6 then
              pos := !pos * 10 + 2
            else pos := !pos*10 + get game (i+k-j) k;
          done;
          if win 0 0 0 !pos then true
          else
            (
              (*cas diagonal \ *)
              pos := 0;
              for k = 0 to 6 do
                if i + j - k < 0 || i + j - k > 6 then
                  pos := !pos * 10 + 2
                else pos := !pos * 10 + get game (i+j-k) k
              done;
              if win 0 0 0 !pos then true
              else false
            )
        )
    )

(*regarde si le joueur color gagne au prochain tour,
  si oui retourne cette colonne
  sinon retourne 7*)
let next_win game color =
  let rec next j =
    if j > 6 then j
    else
      try
        move game j color;
        if is_winning game j then
          (
            remove game j color;
            j
          )
        else
          (
            remove game j color;
            next (j+1)
          )
      with Column_full -> (next (j+1))
  in next 0

let plus couple1 couple2 =
  (fst couple1+ fst couple2, snd couple1+ snd couple2)

let horizontal game color j =
  let col = get_col game j in
  let i =
    if nbr_pieces.(col) > 0 then nbr_pieces.(col) - 1
    else 0 in
  let rec hor_right j acc n_token =
    if j > 6 then (acc, n_token)
    else if get_color game i j = Some color then
      hor_right (j+1) (acc+1) (n_token+1)
    else if get_color game i j = None then hor_right (j+1) (acc+1) n_token
    else (acc, n_token)
  and hor_left j acc n_token =
    if j < 0 then (acc, n_token)
    else if get_color game i j = Some color then
      hor_left (j-1) (acc+1) (n_token+1)
    else if get_color game i j = None then hor_left (j-1) (acc+1) n_token
    else (acc, n_token)
  in
  plus (hor_right j 0 0) (hor_left (j-1) 0 0)


let vertical game color j =
  let col = get_col game j in
  let n_col = nbr_pieces.(col) in
  let rec vert_up i acc n_token =
    if i > 5 then (acc, n_token)
    else vert_up (i+1) (acc+1) n_token
  and vert_down i acc n_token =
    if i < 0 then (acc, n_token)
    else if get_color game i j = Some color then
      vert_down (i-1) (acc+1) (n_token+1)
    else (acc, n_token)
  in plus (vert_up n_col 0 0) (vert_down (n_col-1) 0 0)

let left_diagonal game color j =
  let col = get_col game j in
  let i = nbr_pieces.(col) - 1 in
  let rec diag k acc n_token =
    if k > 6 || i-j+k > 5 then (acc, n_token)
    else if i - j + k < 0 then diag (k+1) acc n_token
    else if get_color game (i+k-j) k = Some color then
      diag (k+1) (acc+1) (n_token+1)
    else if get_color game (i+k-j) k = None then
      diag (k+1) (acc+1) n_token
    else (acc, n_token)
  in diag 0 0 0

let right_diagonal game color j =
  let col = get_col game j in
  let i = nbr_pieces.(col) - 1 in
  let rec diag k acc n_token =
    if k > 6 || i + j - k < 0 then (acc, n_token)
    else if i + j - k > 5 then diag (k+1) acc n_token
    else if get_color game (i+j-k) k = Some color then
      diag (k+1) (acc+1) (n_token+1)
    else if get_color game (i+j-k) k = None then
      diag (k+1) (acc+1) n_token
    else (acc, n_token)
  in diag 0 0 0

(*nbr_aline = le nombre d'alignement qui est encore possible de faire
  weight le poids total de ces alignement, le poids d'un alignement est le
  nombre de pieces qu'il y a déja dans cet alignement*)
(*let aline game j color=
  let (nbr_aline, weight) = (ref 0, ref 0) in
  if j > 6 then (!nbr_aline, !weight)
  else
    let col = get_col game j in
    let n_col = nbr_pieces.(col)  in
    (*vertical*)
    if n_col <> 0 then
      (
        let rec vertical i acc=
          if i < 0 then acc
          else if get game i j = color then vertical (i-1) (acc+1)
          else acc
        in
        let nbr_pieces_aline = vertical (n_col-1) 0 in
        if nbr_pieces_aline + (6-n_col) >= 4 then
          (
            nbr_aline := !nbr_aline+1;
            weight := !weight + (nbr_pieces_aline)
          )
      );
    (*horizontal*)
    let rec horizontal j acc n_token =
      if j > 6 then (acc, n_token)
      else if get game n_col j = color then horizontal (j+1) (acc+1) (n_token+1)
      else if get game n_col j = 2 then horizontal (j+1) (acc+1) n_token
      else acc
    in
    let nbr_pieces_aline = horizontal 0 0 in
    if fst nbr_pieces_aline >= 4 then
      (
        nbr_aline := !nbr_aline+1;
        weight := !weight + (*qqch*)
      );

    let rec diagonal_droite k acc =
      if i + j - k < 0 || i + j - k > 6 then acc
      else if get game (i+j-k) k = color
        || get game (i+j-k) k = 2 then diagonal_droite (k+1) (acc+1)
      else acc
    in
    let nbr_pieces_aline = diagonal_droite 0 0 in
    if nbr_pieces_aline >= 4 then
      (
        nbr_aline := !nbr_aline+1;
        weight := !weight + (*qqch*)
      );

    let rec diagonal_gauche k acc =
      if i - j + k < 0 || i - j + k > 6 then acc
      else if get game (i+k-j) k = color
        ||get game (i+k-j) k = 2 then diagonal_gauche (k+1) (acc+1)
      else acc
    in
    let nbr_pieces_aline = diagonal_gauche 0 0 in
    if nbr_pieces_aline >=4 then
      (
        nbr_aline := !nbr_aline+1;
        weight := !weight + (*qqch*)
      );*)

(*let jeu = make();;
move jeu 3 Red;;
move jeu 4 Yellow;;
move jeu 4 Red;;
move jeu 3 Yellow;;
Printf.printf "\n";;
let a = right_diagonal jeu Yellow 3;;
Printf.printf "%i" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;*)
