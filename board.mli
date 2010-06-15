(** la premiere colonne est celle numerotee O *)

val gameboard : unit -> unit
  (** cree un tableau de jeu representant le puissance 4 *)

val add_piece_to_board : Graphics.color -> int -> unit
  (**[add_piece_to_board color col] ajoute une piece de la couleur [color]
     dans la colonne [col] de l'interface graphique *)

val remove_piece_to_board : int -> unit
  (**[remove_piece_to_board col] retire la derniere piece de la colonne [col]
  de l'interface graphique *)

val play : unit -> int
  (**[play()] attend que le joueur humain clic sur une colonne du jeu et
     retourne cette colonne *)

val delete_the_text : unit -> unit
  (**[delete_the_text()] supprime le text qu'il y avait avant au centre *)

val writing_center : string -> Graphics.color -> unit
  (** [writing_center text color] ecrit le texte [text] sur l'interface
      graphique dans la couleur [color], ce dernier est centré en dessous de la
      grille *)

val write_player_turn : Graphics.color -> unit
  (** [write_player_turn color] ecrit, sur l'interface graphique, le joueur
  qui doit jouer, dans la couleur [color] *)

val red_success : unit -> unit
  (** ecrit en rouge sur l'interface graphique "Le joueur ROUGE gagne!!!" *)

val yellow_success : unit -> unit
  (** ecrit en jaune sur l'interface graphique "Le joueur JAUNE gange!!!" *)

val draw : unit -> unit
  (** ecrit en noir sur l'interface graphique "Match Nul" *)

val close_when_clicked : unit -> unit
  (** ferme le programme quand on clic sur l'interface graphique *)
