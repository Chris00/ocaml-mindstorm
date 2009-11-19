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
  while (!i < n && current_game.tab.(j-1).(!i) <> Empty) do
    i := !i+1
  done;
  if (!i<>n-1) then
    (
      current_game.tab.(j-1).(!i) <- p;
      current_game.tab_line.(j-1).(!i).current_piece <- p;
      (*on remplit le tab ac les lignes dont p fait parti et on modifie les tab des pions de ces lignes*)
      (* pour la ligne verticale *)
      if (!i != 0 && current_game.tab_line.(j-1).(!i-1).current_piece = p)
      then
        (
          let piece_in_line = current_game.tab_line.(j-1).(!i-1).tab_line_piece.(0) + 1 in
          current_game.tab_line.(j-1).(!i).tab_line_piece.(0) <- piece_in_line;
          for k = 1 to (piece_in_line - 1) do
            current_game.tab_line.(j-1).(!i-k).tab_line_piece.(0) <- piece_in_line;
          done;
        )
      else current_game.tab_line.(j-1).(!i).tab_line_piece.(0) <- 1;

      (*pour la ligne horizontale*)
      let g = ref 0 and d = ref 0 in

      if (j-1 > 0 && current_game.tab_line.(j-2).(!i).current_piece = p) then (g := current_game.tab_line.(j-2).(!i).tab_line_piece.(1));

      if (j-1 < 6 && current_game.tab_line.(j).(!i).current_piece = p) then  d := current_game.tab_line.(j).(!i).tab_line_piece.(1);

      current_game.tab_line.(j-1).(!i).tab_line_piece.(1) <- !g + 1 + !d;

      if (!g != 0) then
        (
          for k = 1 to !g do current_game.tab_line.(j-k-1).(!i).tab_line_piece.(1) <- !g + 1 + !d done;
        );

      if (!d != 0) then
        (
          for k = 1 to !d do current_game.tab_line.(j-1+k).(!i).tab_line_piece.(1) <- !g + 1 + !d done;
        );
      
      (* current_game.tab_line.(j-1).(!i) <- ;
         current_game.tab_line.(j-1).(!i) <- ;*)(*plus modifier les voisins*)
      current_game.list_event <-
        [{raw = j; line = !i+1; piece = p}]@current_game.list_event
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
  current_game.tab_line <- Array.init 7 (fun i -> Array.make 6 ({current_piece = Empty; tab_line_piece = [|0;0;0;0|]}));
  current_game.list_event <- [];
  current_game;;





(*TEST*)
let game_test = make();;
move game_test 4 Red;;
move game_test 4 Red;;
move game_test 4 Red;;
move game_test 6 Red;;
move game_test 5 Red;;
move game_test 4 Yellow;;
remove game_test 1;;
new_part game_test;;
