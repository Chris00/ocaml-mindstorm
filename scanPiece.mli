module Run(C: sig val conn_scan : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) :
sig
  val add_piece : int -> unit
    (** [add_piece col] add a piece (typically because the computer
        played there) so that the scan knows where to look. *)

  val scan : (int -> unit) -> unit
    (** [scan k] scanne les cases de la grille où l'autre joueur peut
        mettre sa pièce jusqu'à ce qu'il trouve une nouvelle pièce.
        [k] est la fct qui va être lancée lorsque le robot a trouvé
        une nouvelle pièce ds le jeu et elle est appliquée sur la
        colonne où la pièce a été ajoutée (utile pr pouvoir ensuite
        calculer le coup prochain. *)

  val stop : 'a -> unit
    (** Stop all motors associated with the scan. *)

  val return_init_pos : (unit -> unit) -> unit
    (** [return_init_pos k] puts the scanner in its initial position.
        [k] is the continuation to be executed after. *)
end
