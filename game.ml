(*un pion est rouge ou jaune, ou, dans le plateau, aucun pion*)
(* Yellow = 1, Red = 0*)

(*expo est le tableau des puissances de 2 jusque expo 21*)
let expo =
  let a = Array.make 22 1 in
  for i=1 to 21 do
    a.(i) <- a.(i-1)*2
  done;
  a

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

(*retourne la colonne j sous forme d'entier*)
let get_col game j =
  match j with
  |0 -> (game.cols_left lsr 21) land 0x7F
  |1 -> (game.cols_left lsr 14) land 0x7F
  |2 -> (game.cols_left lsr 7) land 0x7F
  |3 -> (game.cols_left) land 0x7F
  |4 -> (game.cols_right lsr 14) land 0x7F
  |5 -> (game.cols_right lsr 7) land 0x7F
  |6 -> (game.cols_right) land 0x7F
  |a -> raise (Failure (string_of_int a))

let nbr_pieces col =
  if col >= 64 then 6
  else if col >= 32 then 5
  else if col >= 16 then 4
  else if col >= 8 then 3
  else if col >= 4 then 2
  else if col >= 2 then 1
  else 0

(*on veut la ligne de la derniere piece dans la colonne j*)
let get_row game j =
  (nbr_pieces (get_col game j)-1)

(* on conte a partir de 0*)
    (* i est la ligne, j est la colonne*)
let get game i j =
  let col = get_col game j in
  if i < 0 or i > 5 or j < 0 or j > 6 or (nbr_pieces col) <= i then 2
  else (col lsr i) land 1

exception Column_full
exception Column_empty

(*0 -> red, 1 -> yellow*)
(*on ajoute un pion color dans le colonne j*)
let move game j color =
  let col = get_col game j in
  let nbr = nbr_pieces col in
  if col >= 64 then raise Column_full;

  let new_col = match color with
    |0 -> col + expo.(nbr)
    |1 -> col + expo.(nbr+1)
    |_ -> raise (Failure "") in
  if j < 3 then game.cols_left <- game.cols_left + (new_col - col) lsl ((3-j)*7)
  else if j > 3 then
    game.cols_right <- game.cols_right + (new_col - col) lsl ((6-j)*7)
  else
    (
      game.cols_left <- game.cols_left + (new_col - col);
      game.cols_right <- game.cols_right + (new_col - col) lsl 21
    )

let remove game j color =
    let col = get_col game j in
    let nbr = nbr_pieces col in
    if col < 2 then raise Column_empty;
    let new_col = match color with
      |0 -> col - expo.(nbr-1)
      |1 -> col - expo.(nbr)
      |_ -> raise (Failure "") in

    if j < 3 then game.cols_left <- game.cols_left + (new_col - col) lsl ((3-j)*7)
    else if j > 3 then
      game.cols_right <- game.cols_right + (new_col - col) lsl ((6-j)*7)
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

let rec win ?(cur_zero=0) ?(cur_one=0) ?(act=0) pos =
  if cur_zero = 4 or cur_one = 4 then true
  else if ((act - cur_zero) > 3 && (act - cur_one) > 3) then false
  else if pos mod 10 = 0 then win ~cur_zero:(cur_zero+1)
    ~cur_one:0 ~act:(act+1) (pos/10)
  else if pos mod 10 = 1 then win ~cur_zero:0 ~cur_one:(cur_one+1)
    ~act:(act+1) (pos/10)
  else win ~cur_zero:0 ~cur_one:0 ~act:(act+1) (pos/10)

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
      let i = (nbr_pieces col - 1) in
      for k = 0 to 6 do
        pos := !pos * 10 + get game i k;
      done;
      if win !pos then true
      else
        (
          (*cas diagonal /*)
          pos := 0;
          for k = 0 to 6 do
            if i - j + k < 0 || i - j + k > 6 then
              pos := !pos * 10 + 2
            else pos := !pos*10 + get game (i+k-j) k;
          done;
          if win !pos then true
          else
            (
              (*cas diagonal \ *)
              pos := 0;
              for k = 0 to 6 do
                if i + j - k < 0 || i + j - k > 6 then
                  pos := !pos * 10 + 2
                else pos := !pos * 10 + get game (i+j-k) k
              done;
              if win !pos then true
              else false
            )
        )
    )
;;
