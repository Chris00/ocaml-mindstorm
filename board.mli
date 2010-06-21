val gameboard : unit -> unit
  (** cree un tableau de jeu representant le puissance 4 *)

val add_piece_to_board : Graphics.color -> int -> unit
  (**[add_piece_to_board color col] ajoute une piece de la couleur [color]
     dans la colonne [col] de l'interface graphique *)

val remove_piece_to_board : int -> unit
  (**[remove_piece_to_board col] retire la derniere piece de la colonne [col]
  de l'interface graphique *)

val write_player_turn : Graphics.color -> unit
  (** [write_player_turn color] ecrit, sur l'interface graphique, le joueur
  qui doit jouer, dans la couleur [color] *)

val red_success : unit -> unit
  (** ecrit en rouge sur l'interface graphique "Le joueur ROUGE gagne!!!" *)

val yellow_success : unit -> unit
  (** ecrit en jaune sur l'interface graphique "Le joueur JAUNE gange!!!" *)

val draw : unit -> unit
  (** ecrit en noire sur l'interface graphique "Match Nul" *)

val close_when_clicked : unit -> unit
  (** ferme le programme quand on clic sur l'interface graphique *)
