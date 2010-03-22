module Run(C: sig val conn_scan : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) :
sig
  val scan : int -> (int -> unit) -> unit
    (**[scan col_new_piece next] scanne les cases de la grille où l'autre
       joueur peut mettre sa pièce jusqu'à ce qu'il trouve une nouvelle pièce.
       [col_new_piece] est la colonne où le robot à mis sa pièce (utile pr
       mettre à jour le jeu courant).
       [next] est la fct qui va être lancée lorsque le robot a trouvé une
       nouvelle pièce ds le jeu et elle est appliquée sur la colonne où la
       pièce a été ajoutée (utile pr pouvoir ensuite calculer le coup prochain
    *)

  val run : unit -> unit
    (** Starts the event loop associated with the scan. *)

  val stop : int -> unit
    (** Stop all motors associated with the scan. *)


end
