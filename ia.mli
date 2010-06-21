(*Evaluates the future possible positions, like an usual Alphabeta
algorithm, for the current board*)
val look_ahed : Structure.board -> int

(*Returns the next move played by the IA for the current board*)
val move_for : Structure.board -> int
