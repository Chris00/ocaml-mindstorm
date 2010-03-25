

type level = Weak | Normal | Strong

val move_for : ?level:level -> int list -> (unit -> int option) -> int option
  (** [move_for game] Returns the next column to play if [game] is the
      list of columns (numbered from 0 to 6) played by both players.
      If no next column is possible (say because victory has been
      achieved), returns [None].  *)
