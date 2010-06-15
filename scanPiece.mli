module Run(C: sig val conn_scan : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) :
sig
  val add_piece : int -> unit
    (** [add_piece col] adds a piece (typically because the computer
        played there) so that the scan knows where to look. *)

  val scan : (int -> unit) -> unit
    (** [scan k] scanne les cases de la grille ou l'autre joueur peut
        mettre sa piece jusqu'a ce qu'il trouve une nouvelle piece.
        [k] est la fonction qui va etre lancee lorsque le robot a trouve
        une nouvelle piece dans le jeu et elle est appliquee sur la
        colonne ou la piece a ete ajoute (utile pour pouvoir ensuite
        calculer le coup prochain. *)

  val stop : 'a -> unit
    (** Stop all motors associated with the scan. *)

  val return_init_pos : (unit -> unit) -> unit
    (** [return_init_pos k] puts the scanner in its initial position.
        [k] is the continuation to be executed after. *)
end
