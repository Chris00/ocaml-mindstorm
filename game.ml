type piece = Red | Yellow

(*un evenement c'est l'endroit ou l'on place le pion et la couleur de celui-ci*)
(*attention raw et line démarre a 1 et dans les algos a 0*)
type event =
    {
      raw : int;
      line : int;
      piece : piece
    }

(*un jeu c'est un tableau représentant l'état du jeu
  et une liste des événements placés du plus recents au plus vieux*)
type g =
    {
      mutable tab : (piece option) array array;
      mutable list_event : event list
    }

(*création d'un jeu -> on initialise le tableau a non et la liste d'événement
  est vide*)
let make () = { tab = Array.init 7 (fun i -> (Array.make 6 None)); list_event = [] };;

(*récupération de la couleur (s'il y a ) du pion en i,j*)
let get_piece current_game j i = current_game.tab.(j).(i);;

(*on place le pion p dans la colonne j*)
let move current_game j p =
  let n = Array.length current_game.tab and i= ref 0 in
  while (!i < n && current_game.tab.(j-1).(!i) <> None) do
    i := !i+1
  done;
  if (!i<>n-1) then
    (
      current_game.tab.(j-1).(!i) <- Some p;
      current_game.list_event <- [{raw = j; line = !i+1; piece = p}]@current_game.list_event
    );
  current_game;
;;

let rec remove current_game num =
  if (num<>0)then
    (
      let next_event = List.hd current_game.list_event in
      let j = next_event.raw
      and i = next_event.line in
      current_game.tab.(j-1).(i-1) <- None;
      current_game.list_event <- List.tl current_game.list_event;
      remove current_game (num-1)
    )
  else current_game
;;


(*TEST*)
let game_test = make();;
move game_test 4 Red;;
move game_test 4 Yellow;;
remove game_test 1;;
