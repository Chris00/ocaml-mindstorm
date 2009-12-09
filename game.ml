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

(*création d'un jeu -> on initialise le tableau a vide et la liste d'événement
  est vide*)
let make () =
  {
    tab = Array.init 7 (fun i -> (Array.make 6 Empty));
    tab_line = Array.init 7
      (fun i -> Array.init 6
         (fun i -> {current_piece = Empty; tab_line_piece = Array.make 4 0}));
    list_event = [];
    number_of_move = 0;
    col_st = Array.make 7 0
  }
;;

(*récupération de la couleur (s'il y a ) du pion en i,j*)
let get current_game j i =
  current_game.tab.(j).(i)
;;

(*l'acteur place le pion qui correspond a sa couleur dans la colonne j*)
let move current_game j pion =
  let i = current_game.col_st.(j) in (*i est le nombre de piece dans la
                                       colonne j*)

  if i <> 6 then (
    current_game.col_st.(j) <- i + 1;
    current_game.tab.(j).(i) <- pion;
    current_game.tab_line.(j).(i).current_piece <- pion;
    current_game.number_of_move <- current_game.number_of_move + 1;
    (*on remplit le tab_line_piece du pion courant et on modifie
      les tab des pions de meme couleurs qui lui sont alignés*)
    (* pour la ligne verticale *)
    if i <> 0 && current_game.tab_line.(j).(i-1).current_piece = pion then (
      let down = current_game.tab_line.(j).(i-1).tab_line_piece.(0) in
      for k = 0 to down do
        current_game.tab_line.(j).(i-k).tab_line_piece.(0) <- down + 1;
      done
    )
    else current_game.tab_line.(j).(i).tab_line_piece.(0) <- 1;

    (*on récupère les données pour toutes les cases adjacentes sauf
      la verticale pour qui c'est déjà fait juste au dessus*)
    let l = ref 0
    and r = ref 0
    and up_r = ref 0
    and down_l = ref 0
    and down_r = ref 0
    and up_l = ref 0 in

    if j>0 then (
      if current_game.tab_line.(j-1).(i).current_piece = pion then
        l := current_game.tab_line.(j-1).(i).tab_line_piece.(1);

      if i>0 && current_game.tab_line.(j-1).(i-1).current_piece = pion then
        (down_l := current_game.tab_line.(j-1).(i-1).tab_line_piece.(2));

      if i<5 && current_game.tab_line.(j-1).(i+1).current_piece = pion then
        (up_l := current_game.tab_line.(j-1).(i+1).tab_line_piece.(3));
    );

    if j<6 then (
      if current_game.tab_line.(j+1).(i).current_piece = pion then
        r := current_game.tab_line.(j+1).(i).tab_line_piece.(1);

      if i<5 && current_game.tab_line.(j+1).(i+1).current_piece = pion then
        up_r := current_game.tab_line.(j+1).(i+1).tab_line_piece.(2);

      if i>0 && current_game.tab_line.(j+1).(i-1).current_piece = pion then
        down_r := current_game.tab_line.(j+1).(i-1).tab_line_piece.(3);
    );

    (*on modifie les données de tav_line pour la case courante*)
    current_game.tab_line.(j).(i).tab_line_piece.(1) <- !l + 1 + !r;
    current_game.tab_line.(j).(i).tab_line_piece.(2) <- !down_l + 1 + !up_r;
    current_game.tab_line.(j).(i).tab_line_piece.(3) <- !up_l + 1 + !down_r;

    (*on modifie les données des cases adjacentes se trouvant dans une
      meme ligne que le pion courant*)
    (*mise à jour de la ligne horizontale*)
    if !l <> 0 then (
      for k = 1 to !l do
        current_game.tab_line.(j-k).(i).tab_line_piece.(1) <- !l + 1 + !r
      done;
    );

    if !r <> 0 then (
      for k = 1 to !r do
        current_game.tab_line.(j+k).(i).tab_line_piece.(1) <- !l  + 1 + !r
      done;
    );

    (*pour la première diagonale*)
    if !down_l <> 0 then (
      for k = 1 to !down_l do
        current_game.tab_line.(j-k).(i-k).tab_line_piece.(2) <-
          !down_l + 1 + !up_r
      done;
    );

    if !up_r <> 0 then (
      for k = 1 to !up_r do
        current_game.tab_line.(j+k).(i+k).tab_line_piece.(2) <-
          !down_l  + 1 + !up_r
      done;
    );

    (*pour la deuxième diagonale*)
    if !up_l <> 0 then (
      for k = 1 to !up_l do
        current_game.tab_line.(j-k).(i+k).tab_line_piece.(3) <-
          !up_l + 1 + !down_r
      done;
    );

    if !down_r <> 0 then (
      for k = 1 to !down_r do
        current_game.tab_line.(j+k).(i-k).tab_line_piece.(3) <-
          !up_l  + 1 + !down_r
      done;
    );

    (*mise à jour de la liste des évènements du jeu*)
    current_game.list_event <-
      [{col = j; line = i; piece = pion}]@current_game.list_event
  )
;;

(*retourne de num coup en arrière dans le jeu*)
let rec remove current_game num =
  if num <> 0 then (
    let next_event = List.hd current_game.list_event in
    let j = next_event.col
    and i = next_event.line in
    current_game.col_st.(j) <- current_game.col_st.(j) - 1;
    current_game.tab.(j).(i) <- Empty;
    current_game.tab_line.(j).(i)
    <- {current_piece = Empty; tab_line_piece = [|0;0;0;0|]};
    current_game.number_of_move <- current_game.number_of_move - 1;
    (*plus changer les valeurs des voisins...*)
    current_game.list_event <- List.tl current_game.list_event;
    remove current_game (num-1)
  )
;;

(*creer une copy du jeu courant*)
let copy current_game =
  {
    tab = Array.init 7 (fun i -> (Array.copy current_game.tab.(i)));

    tab_line = Array.init 7
      (fun i -> Array.init 6
         (fun j ->
            {current_piece = (current_game.tab_line.(i).(j)).current_piece;
             tab_line_piece = Array.copy
                (current_game.tab_line.(i).(j).tab_line_piece)
            }
         )
      );

    list_event = current_game.list_event;
    number_of_move = current_game.number_of_move;
    col_st = Array.copy current_game.col_st
  }
;;


(*création d'une nouvelle partie*)
let new_part current_game =
  current_game.tab <- Array.init 7 (fun i -> (Array.make 6 Empty));
  current_game.tab_line <- Array.init 7
    (fun i -> Array.init 6
       (fun i -> ({current_piece = Empty; tab_line_piece = [|0;0;0;0|]})));
  current_game.list_event <- [];
  current_game.number_of_move <- 0;
  current_game.col_st <- Array.make 7 0
;;

(*retourne vrai qd le pion en (i,j) est ds une ligne de 4pions de meme couleur*)
(*faire avec une methode recursif qui garde win*)
let isWin current_game =
  if current_game.number_of_move <> 0 then
    let i = (List.hd (current_game.list_event)).line
    and j = (List.hd (current_game.list_event)).col in
    let rec winner won k =
      if (won = false && k < 4) then
        if (current_game.tab_line.(j).(i).tab_line_piece.(k)=4)
        then winner true k
        else winner won (k+1)
      else won
    in winner false 0
  else false
;;
