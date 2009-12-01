(*un pion est rouge ou jaune, ou, dans le plateau, aucun pion*)
type piece = Red | Yellow | Empty

(*on peut etre soit un humain, soit un ordinateur*)
type player = Computer | Human

(*un acteur du jeu est un joueur avec une couleur qui lui correspond*)
type actor =
    {
      player : player;
      pion : piece;
      fst_player : bool
    }

(*un couple de la piece courrante, repr�sente une piece et le tableau
  repr�sentant les lignes dont la piece fait partie : horizontal, diagonal...*)
type couple_current_piece =
    {
      mutable current_piece : piece;
      mutable tab_line_piece : int array
    }

(*un evenement c'est l'endroit ou l'on place le pion et la couleur de celui-ci*)
(*attention raw et line d�marre a 0 pour les algos*)
type event =
    {
      raw : int;
      line : int;
      piece : piece
    }

(*un jeu c'est un tableau repr�sentant l'�tat du jeu
  un autre tableau repr�sentant le pion et les lignes dont il fait parti
  et une liste des �v�nements plac�s du plus recents au plus vieux*)
type g =
    {
      mutable tab : piece array array;
      mutable tab_line : couple_current_piece array array;
      (*on aurait pu le mettre ac tab mais c pour calculer si on a gagn�
        sans l'arbre qu'on n'a pas encore impl�menter*)
      (*le tab int�rieur et de taille 4, repr�sente les 4 lignes
        : |, -, /, \ *)
      mutable list_event : event list;
      mutable raw_st : int array;
    }

(*cr�ation d'un jeu -> on initialise le tableau a vide et la liste d'�v�nement
  est vide*)
let make () =
  {
    tab = Array.init 7 (fun i -> (Array.make 6 Empty));
    tab_line = Array.init 7
      (fun i -> Array.init 6
         (fun i -> {current_piece = Empty; tab_line_piece = Array.make 4 0}));
    list_event = [];
    raw_st = Array.make 7 0
  }
;;

(*r�cup�ration de la couleur (s'il y a ) du pion en i,j*)
let get_piece current_game j i =
  current_game.tab.(j).(i)
;;

(*l'acteur place le pion qui correspond a sa couleur dans la colonne j*)
let move current_game j actor =
  let i = current_game.raw_st.(j) in (*i est le nombre de piece dans la
                                       colonne j*)

  if (i <> 6) then
    (
      current_game.raw_st.(j) <- i + 1;
      current_game.tab.(j).(i) <- actor.pion;
      current_game.tab_line.(j).(i).current_piece <- actor.pion;
      (*on remplit le tab_line_piece du pion courant et on modifie
        les tab des pions de meme couleurs qui lui sont align�s*)
      (* pour la ligne verticale *)
      if (i != 0 && current_game.tab_line.(j).(i-1).current_piece = actor.pion)
      then
        (
          let down = current_game.tab_line.(j).(i-1).tab_line_piece.(0) in
          for k = 0 to down do
            current_game.tab_line.(j).(i-k).tab_line_piece.(0) <- down + 1;
          done
        )
      else current_game.tab_line.(j).(i).tab_line_piece.(0) <- 1;

      (*on r�cup�re les donn�es pour toutes les cases adjacentes sauf
        la verticale pour qui c'est d�j� fait juste au dessus*)
      let l = ref 0
      and r = ref 0
      and up_r = ref 0
      and down_l = ref 0
      and down_r = ref 0
      and up_l = ref 0 in

      if j>0
      then
        (
          if current_game.tab_line.(j-1).(i).current_piece = actor.pion
          then
            l := current_game.tab_line.(j-1).(i).tab_line_piece.(1);

          if(i>0 && current_game.tab_line.(j-1).(i-1).current_piece =
              actor.pion)
          then
            (down_l := current_game.tab_line.(j-1).(i-1).tab_line_piece.(2));

          if (i<5 && current_game.tab_line.(j-1).(i+1).current_piece =
              actor.pion)
          then
            (up_l := current_game.tab_line.(j-1).(i+1).tab_line_piece.(3));
        );

      if j<6
      then
        (
          if current_game.tab_line.(j+1).(i).current_piece = actor.pion
          then
            r := current_game.tab_line.(j+1).(i).tab_line_piece.(1);

          if (i<5 && current_game.tab_line.(j+1).(i+1).current_piece =
              actor.pion)
          then
            up_r := current_game.tab_line.(j+1).(i+1).tab_line_piece.(2);

          if (i>0 && current_game.tab_line.(j+1).(i-1).current_piece =
              actor.pion)
          then
            down_r := current_game.tab_line.(j+1).(i-1).tab_line_piece.(3);
        );

      (*on modifie les donn�es de tav_line pour la case courante*)
      current_game.tab_line.(j).(i).tab_line_piece.(1) <- !l + 1 + !r;
      current_game.tab_line.(j).(i).tab_line_piece.(2) <- !down_l + 1 + !up_r;
      current_game.tab_line.(j).(i).tab_line_piece.(3) <- !up_l + 1 + !down_r;

      (*on modifie les donn�es des cases adjacentes se trouvant dans une
        meme ligne que le pion courant*)
      (*mise � jour de la ligne horizontale*)
      if (!l != 0) then
        (
          for k = 1 to !l do
            current_game.tab_line.(j-k).(i).tab_line_piece.(1) <- !l + 1 + !r
          done;
        );

      if (!r != 0) then
        (
          for k = 1 to !r do
            current_game.tab_line.(j+k).(i).tab_line_piece.(1) <- !l  + 1 + !r
          done;
        );

      (*pour la premi�re diagonale*)
      if (!down_l != 0) then
        (
          for k = 1 to !down_l do
            current_game.tab_line.(j-k).(i-k).tab_line_piece.(2) <-
              !down_l + 1 + !up_r
          done;
        );

      if (!up_r != 0) then
        (
          for k = 1 to !up_r do
            current_game.tab_line.(j+k).(i+k).tab_line_piece.(2) <-
              !down_l  + 1 + !up_r
          done;
        );

      (*pour la deuxi�me diagonale*)
      if (!up_l != 0) then
        (
          for k = 1 to !up_l do
            current_game.tab_line.(j-k).(i+k).tab_line_piece.(3) <-
              !up_l + 1 + !down_r
          done;
        );

      if (!down_r != 0) then
        (
          for k = 1 to !down_r do
            current_game.tab_line.(j+k).(i-k).tab_line_piece.(3) <-
              !up_l  + 1 + !down_r
          done;
        );

      (*mise � jour de la liste des �v�nements du jeu*)
      current_game.list_event <-
        [{raw = j; line = i; piece = actor.pion}]@current_game.list_event
    )
;;

(*retourne de num coup en arri�re dans le jeu*)
let rec remove current_game num =
  if (num<>0)then
    (
      let next_event = List.hd current_game.list_event in
      let j = next_event.raw
      and i = next_event.line in
      current_game.raw_st.(j) <- current_game.raw_st.(j) - 1;
      current_game.tab.(j).(i) <- Empty;
      current_game.tab_line.(j).(i)
      <- {current_piece = Empty; tab_line_piece = [|0;0;0;0|]};
      (*plus changer les valeurs des voisins...*)
      current_game.list_event <- List.tl current_game.list_event;
      remove current_game (num-1)
    )
;;

(*creer une copy du jeu courant*)
let copy_game current_game =
  let copy = make() in
  copy.tab <- Array.copy current_game.tab;
  copy.tab_line <- Array.copy current_game.tab_line;
  copy.list_event <- current_game.list_event;
  copy.raw_st <- current_game.raw_st;
  copy
;;

(*cr�ation d'une nouvelle partie*)
let new_part current_game =
  current_game.tab <- Array.init 7 (fun i -> (Array.make 6 Empty));
  current_game.tab_line <- Array.init 7
    (fun i -> Array.init 6
       (fun i -> ({current_piece = Empty; tab_line_piece = [|0;0;0;0|]})));
  current_game.list_event <- [];
  current_game.raw_st <- Array.make 7 0
;;

(*retourne vrai qd le pion en (i,j) est ds une ligne de 4pions de meme couleur*)
(*faire avec une methode recursif qui garde win*)
let isWin current_game j i =
  let win = ref false  and k = ref 0 in
  while (!win = false && !k<4) do
    if current_game.tab_line.(j).(i).tab_line_piece.(!k)=4
    then win := true
    else k := !k+1
  done;
  !win
;;
