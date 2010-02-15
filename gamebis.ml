(*un pion est rouge ou jaune, ou, dans le plateau, aucun pion*)

type slot_content = Red | Yellow | Empty

(*un couple de la piece courrante, représente une piece et le tableau
  représentant les lignes dont la piece fait partie : horizontal, diagonal...*)
type couple_current_piece =
    {
      mutable current_piece : slot_content;
      mutable tab_line_piece : int array
    }

(*un evenement c'est l'endroit ou l'on place le pion et la couleur de celui-ci*)
(*attention raw et line démarre a 0 pour les algos*)
type event =
    {
      col : int;
      line : int;
      piece : slot_content
    }

(*un jeu c'est un tableau représentant l'état du jeu
  un autre tableau représentant le pion et les lignes dont il fait parti
  et une liste des événements placés du plus recents au plus vieux*)
type t =
    {
      mutable tab : slot_content array array;
      mutable tab_line : couple_current_piece array array;
      (*on aurait pu le mettre ac tab mais c pour calculer si on a gagné
        sans l'arbre qu'on n'a pas encore implémenter*)
      (*le tab intérieur et de taille 4, représente les 4 lignes
        : |, -, /, \ *)
      mutable list_event : event list;
      mutable number_of_move : int;
      mutable col_st : int array
    }

let number_of_moves game = game.number_of_move
let last_move game = List.hd game.list_event
let last_move_col game = (List.hd game.list_event).col
let npieces_col game i = game.col_st.(i)
let get_tab game = game.tab
let get_tab_line_piece game i j = game.tab_line.(i).(j).tab_line_piece

let init_matrix nrow ncol f =
  Array.init nrow (fun i -> Array.init ncol (fun j -> f i j))

(*création d'un jeu -> on initialise le tableau a vide et la liste d'événement
  est vide*)
let make () =
  {
    tab = Array.make_matrix 7 6 Empty;
    tab_line = init_matrix 7 6 (fun _ _ -> {current_piece = Empty;
                                         tab_line_piece = Array.make 4 0});
    list_event = [];
    number_of_move = 0;
    col_st = Array.make 7 0
  }
;;

(*récupération de la couleur (s'il y a ) du pion en i,j*)
let get game row col = game.tab.(col).(row)

exception Column_full

(*l'acteur place le pion qui correspond a sa couleur dans la colonne j*)
let move game col pion =
  let i = game.col_st.(col) in (*i est le nombre de piece dans la
                                       colonne [col] *)
  if i = 6 then raise Column_full;
  game.col_st.(col) <- i + 1;
  game.tab.(col).(i) <- pion;
  game.tab_line.(col).(i).current_piece <- pion;
  game.number_of_move <- game.number_of_move + 1;
  (*on remplit le tab_line_piece du pion courant et on modifie
    les tab des pions de meme couleurs qui lui sont alignés*)
  (* pour la ligne verticale *)
  if i <> 0 && game.tab_line.(col).(i-1).current_piece = pion then (
    let down = game.tab_line.(col).(i-1).tab_line_piece.(0) in
    for k = 0 to down do
      game.tab_line.(col).(i-k).tab_line_piece.(0) <- down + 1;
    done
  )
  else game.tab_line.(col).(i).tab_line_piece.(0) <- 1;
  (*on récupère les données pour toutes les cases adjacentes sauf
    la verticale pour qui c'est déjà fait juste au dessus*)
  let l = ref 0
  and r = ref 0
  and up_r = ref 0
  and down_l = ref 0
  and down_r = ref 0
  and up_l = ref 0 in

  if col > 0 then (
    let left = game.tab_line.(col-1) in
    if left.(i).current_piece = pion then l := left.(i).tab_line_piece.(1);
    if i>0 && left.(i-1).current_piece = pion then
      down_l := left.(i-1).tab_line_piece.(2);
    if i<5 && left.(i+1).current_piece = pion then
      up_l := left.(i+1).tab_line_piece.(3);
  );

  if col < 6 then (
    let right = game.tab_line.(col+1) in
    if right.(i).current_piece = pion then r := right.(i).tab_line_piece.(1);
    if i<5 && right.(i+1).current_piece = pion then
      up_r := right.(i+1).tab_line_piece.(2);
    if i>0 && right.(i-1).current_piece = pion then
      down_r := right.(i-1).tab_line_piece.(3);
  );

  (*on modifie les données de tab_line pour la case courante*)
  game.tab_line.(col).(i).tab_line_piece.(1) <- !l + 1 + !r;
  game.tab_line.(col).(i).tab_line_piece.(2) <- !down_l + 1 + !up_r;
  game.tab_line.(col).(i).tab_line_piece.(3) <- !up_l + 1 + !down_r;

  (*on modifie les données des cases adjacentes se trouvant dans une
    meme ligne que le pion courant*)
  (*mise à jour de la ligne horizontale*)
  if !l <> 0 then (
    for k = 1 to !l do
      game.tab_line.(col-k).(i).tab_line_piece.(1) <- !l + 1 + !r
    done;
  );
  if !r <> 0 then (
    for k = 1 to !r do
      game.tab_line.(col+k).(i).tab_line_piece.(1) <- !l  + 1 + !r
    done;
  );

  (*pour la première diagonale*)
  if !down_l <> 0 then (
    for k = 1 to !down_l do
      game.tab_line.(col-k).(i-k).tab_line_piece.(2) <- !down_l + 1 + !up_r
    done;
  );
  if !up_r <> 0 then (
    for k = 1 to !up_r do
      game.tab_line.(col+k).(i+k).tab_line_piece.(2) <-
        !down_l  + 1 + !up_r
    done;
  );

  (*pour la deuxième diagonale*)
  if !up_l <> 0 then (
    for k = 1 to !up_l do
      game.tab_line.(col-k).(i+k).tab_line_piece.(3) <- !up_l + 1 + !down_r
    done;
  );
  if !down_r <> 0 then (
    for k = 1 to !down_r do
      game.tab_line.(col+k).(i-k).tab_line_piece.(3) <-
        !up_l  + 1 + !down_r
    done;
  );

  (*mise à jour de la liste des évènements du jeu*)
  game.list_event <- {col = col; line = i; piece = pion} :: game.list_event;
;;

(*retourne de num coup en arrière dans le jeu*)
let rec remove game num =
  if num <> 0 then (
    let next_event = List.hd game.list_event in
    let j = next_event.col
    and i = next_event.line in
    game.col_st.(j) <- game.col_st.(j) - 1;
    game.tab.(j).(i) <- Empty;
    game.tab_line.(j).(i)
    <- {current_piece = Empty; tab_line_piece = [|0;0;0;0|]};
    game.number_of_move <- game.number_of_move - 1;
    (*plus changer les valeurs des voisins...*)
    game.list_event <- List.tl game.list_event;
    remove game (num-1)
  )
;;

(*creer une copy du jeu courant*)
let copy game =
  let tl = game.tab_line in
  {
    tab = Array.map Array.copy game.tab;

    tab_line = init_matrix 7 6
      (fun i j -> {current_piece = (tl.(i).(j)).current_piece;
                tab_line_piece = Array.copy tl.(i).(j).tab_line_piece } );

    list_event = game.list_event;
    number_of_move = game.number_of_move;
    col_st = Array.copy game.col_st
  }
;;


(*création d'une nouvelle partie*)
let reset game =
  game.tab <- Array.make_matrix 7 6 Empty;
  game.tab_line <- init_matrix 7 6
    (fun _ _ -> {current_piece = Empty; tab_line_piece = [|0;0;0;0|]});
  game.list_event <- [];
  game.number_of_move <- 0;
  game.col_st <- Array.make 7 0
;;

(*retourne vrai qd le pion en (i,j) est ds une ligne de 4pions de meme couleur*)
(*faire avec une methode recursif qui garde win*)
let is_winning game =
  if game.number_of_move <> 0 then
    let last_move = List.hd game.list_event in
    let piece = game.tab_line.(last_move.col).(last_move.line).tab_line_piece in
    let rec winner k = k < 4 && (piece.(k) >= 4 || winner (k+1)) in
    winner 0
  else false
;;
