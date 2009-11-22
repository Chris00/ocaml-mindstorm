type piece = Red | Yellow | Empty
type couple_current_piece =
    {
      mutable current_piece : piece;
      mutable tab_line_piece : int array
    }

(*un evenement c'est l'endroit ou l'on place le pion et la couleur de celui-ci*)
(*attention raw et line démarre a 1 et dans les algos a 0*)
type event =
    {
      raw : int;
      line : int;
      piece : piece
    }

(*un jeu c'est un tableau représentant l'état du jeu
  un autre tableau représentant le pion et les lignes dont il fait parti
  et une liste des événements placés du plus recents au plus vieux*)
type g =
    {
      mutable tab : piece array array;
      mutable tab_line : couple_current_piece array array; (*on aurait pu le mettre ac tab mais c pour calculer si on a gagné sans l'arbre qu'on n'a pas encore implémenter*) (*le tab intérieur et de taille 4, représente les 4 lignes : |, -, /, \ *)
      mutable list_event : event list
    }
 
(*création d'un jeu -> on initialise le tableau a non et la liste d'événement
  est vide*)
let make () = { tab = Array.init 7 (fun i -> (Array.make 6 Empty)); tab_line = Array.init 7 (fun i -> Array.init 6 (fun i -> {current_piece = Empty; tab_line_piece = Array.make 4 0})); list_event = [] };;

(*récupération de la couleur (s'il y a ) du pion en i,j*)
let get_piece current_game j i = current_game.tab.(j).(i);;

(*on place le pion p dans la colonne j*)
let move current_game j p =
  let n = Array.length current_game.tab and i= ref 0 in
  while (!i < n && current_game.tab.(j).(!i) <> Empty) do
    i := !i+1
  done;
  if (!i<>n-1) then
    (
      current_game.tab.(j).(!i) <- p;
      current_game.tab_line.(j).(!i).current_piece <- p;
      (*on remplit le tab_line_piece du pion courant et on modifie les tab des pions de meme couleurs qui lui sont alignés*)
      (* pour la ligne verticale *)
      if (!i != 0 && current_game.tab_line.(j).(!i-1).current_piece = p)
      then
        (
          let down = ref 0 in
          down := current_game.tab_line.(j).(!i-1).tab_line_piece.(0);
          for k = 0 to !down do
            current_game.tab_line.(j).(!i-k).tab_line_piece.(0) <- !down + 1;
          done;
        )
      else current_game.tab_line.(j).(!i).tab_line_piece.(0) <- 1;

      (*on récupère les données pour toutes les cases adjacentes sauf la verticale pour qui c'est déjà fait juste au dessus*)
      let l = ref 0 and r = ref 0 and up_r = ref 0 and down_l = ref 0 and down_r = ref 0 and up_l = ref 0 in

      if j>0 then
        (
          if current_game.tab_line.(j-1).(!i).current_piece = p then
            l := current_game.tab_line.(j-1).(!i).tab_line_piece.(1);
          if(!i>0 && current_game.tab_line.(j-1).(!i-1).current_piece = p) then
            (down_l := current_game.tab_line.(j-1).(!i-1).tab_line_piece.(2));
          if (!i<5 && current_game.tab_line.(j-1).(!i+1).current_piece = p) then
            (up_l := current_game.tab_line.(j-1).(!i+1).tab_line_piece.(3));
        );
      if j<6 then
        (
          if current_game.tab_line.(j+1).(!i).current_piece = p then
            r := current_game.tab_line.(j+1).(!i).tab_line_piece.(1);
          if (!i<5 && current_game.tab_line.(j+1).(!i+1).current_piece = p) then
            up_r := current_game.tab_line.(j+1).(!i+1).tab_line_piece.(2);
          if (!i>0 && current_game.tab_line.(j+1).(!i-1).current_piece = p) then
            down_r := current_game.tab_line.(j+1).(!i-1).tab_line_piece.(3);
        );
      (*on modifie les données de tav_line pour la case courante*)
      current_game.tab_line.(j).(!i).tab_line_piece.(1) <- !l + 1 + !r;
      current_game.tab_line.(j).(!i).tab_line_piece.(2) <- !down_l + 1 + !up_r;
      current_game.tab_line.(j).(!i).tab_line_piece.(3) <- !up_l + 1 + !down_r;

      (*on modifie les données des cases adjacentes se trouvant dans une meme ligne que le pion courant*)
      (*mise à jour de la ligne horizontale*)
      if (!l != 0) then
        (
          for k = 1 to !l do current_game.tab_line.(j-k).(!i).tab_line_piece.(1) <- !l + 1 + !r done;
        );

      if (!r != 0) then
        (
          for k = 1 to !r do current_game.tab_line.(j+k).(!i).tab_line_piece.(1) <- !l  + 1 + !r done;
        );

      (*pour la première diagonale*)
      if (!down_l != 0) then
        (
          for k = 1 to !down_l do current_game.tab_line.(j-k).(!i-k).tab_line_piece.(2) <- !down_l + 1 + !up_r done;
        );

      if (!up_r != 0) then
        (
          for k = 1 to !up_r do current_game.tab_line.(j+k).(!i+k).tab_line_piece.(2) <- !down_l  + 1 + !up_r done;
        );

      (*pour la deuxième diagonale*)
      if (!up_l != 0) then
        (
          for k = 1 to !up_l do current_game.tab_line.(j-k).(!i+k).tab_line_piece.(3) <- !up_l + 1 + !down_r done;
        );

      if (!down_r != 0) then
        (
          for k = 1 to !down_r do current_game.tab_line.(j+k).(!i-k).tab_line_piece.(3) <- !up_l  + 1 + !down_r done;
        );

      (*mise à jour de la liste des évènements du jeu*)
      current_game.list_event <-
        [{raw = j; line = !i; piece = p}]@current_game.list_event
    );
  current_game;
;;

(*retourne de num coup en arrière dans le jeu*)
let rec remove current_game num =
  if (num<>0)then
    (
      let next_event = List.hd current_game.list_event in
      let j = next_event.raw
      and i = next_event.line in
      current_game.tab.(j-1).(i-1) <- Empty;
      current_game.tab_line.(j-1).(i-1) <- {current_piece = Empty; tab_line_piece = [|0;0;0;0|]}; (*plus changer les valeurs des voisins...*)
      current_game.list_event <- List.tl current_game.list_event;
      remove current_game (num-1)
    )
  else current_game
;;

(*création d'une nouvelle partie*)
let new_part current_game =
  current_game.tab <- Array.init 7 (fun i -> (Array.make 6 Empty));
  current_game.tab_line <- Array.init 7 (fun i -> Array.init 6 (fun i -> ({current_piece = Empty; tab_line_piece = [|0;0;0;0|]})));
  current_game.list_event <- [];
  current_game;;

(*retourne vrai qd le pion en (i,j) est ds une ligne de 4pions de meme couleur*)
let isWin current_game j i =
  let win = ref false  and k= ref 0 in
  while (!win = false && !k<4) do
    if current_game.tab_line.(j).(i).tab_line_piece.(!k)=4
    then win := true
    else k := !k+1
  done;
  !win;;


(*TEST*)
let game_test = make();;
move game_test 4 Red;;
move game_test 4 Yellow;;
move game_test 4 Red;;
move game_test 4 Yellow;;
move game_test 4 Red;;
isWin game_test 3 3;;
isWin game_test 4 4;;
move game_test 6 Red;;
move game_test 5 Red;;
move game_test 4 Yellow;;
remove game_test 1;;
new_part game_test;;
