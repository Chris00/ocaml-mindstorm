(*Evaluates the future possible positions, like an usual Alphabeta
algorithm, for the current board*)
val look_ahed : Board.t -> int

(*Returns the next move played by the IA for the current board*)
val move_for : Board.t -> int
