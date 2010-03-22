type mode = Max | Min

val heuristic : Game.t -> Game.color -> mode -> float * int
(** h est une fonction heuristique qui retourne la valeur que l'on peut obtenir
   et la colonne � jouer pour l'obtenir*)

val alphabeta : Game.t -> Game.color -> int ->
  (Game.t -> Game.color -> mode -> float * int) -> float * int
  (** [alphabeta game color depth heuristic] retourne la valeur du jeu game
      que l'on peut obtenir en jouant dans la colonne donn�e.
      Cette fonction ne peut �tre utilis�e que si la partie[game] n'est pas
      finie *)
