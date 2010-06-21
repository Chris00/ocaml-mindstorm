(*Basic type of the game.
Square is the current  position, wipesq the possible
group wiped, usablegroups the groups usable, sqused the
matrix of the useless squares, stacks the number of pieces
in each column, groups the 69 possible ways to win the game,
xplace the possible row to play, yplace the possible columns
turn the player who can play the next piece, moves the list
of moves in the game, choices the choices taken by the IA,
mlist the list of mouvements, filled the number of men, intgp
the groups possible for a win, solution the array of possible
solutions, sp current solution checked, problem_solved the
column to play to solve the problem, solused the solution used,
oracle the array saying if the players have a solution with
the heuristic, oracle_guesses the number of guesses of the IA,
lastguess his last guess, node_visited the number
of nodes visited in total, maxtreedepth the max tree depth,
rules the array of the rules, instances the number of time
each rule was used, solvable_groups the array of groups
solvable, white_book the book of openings*)
type board = {
  mutable square : int ref array;
  mutable wipesq : int array;
  mutable usablegroup : bool array;
  mutable sqused : bool array;
  mutable stack : int array;
  mutable groups : int ref array array;
  mutable xplace : int array array;
  mutable yplace : int array array;
  mutable turn : int;
  mutable moves : int array;
  mutable choices : int array;
  mutable mlist : int array;
  mutable filled : int;
  mutable intgp : Utils.intgp;
  mutable solution : Utils.solution array;
  mutable sp : int;
  mutable problem_solved : int;
  mutable solused : int;
  mutable oracle : bool array;
  mutable oracle_guesses : int;
  mutable lastguess : int;
  mutable nodes_visited : int;
  mutable maxtreedepth : int;
  mutable rules : int array;
  mutable instances : int array;
  mutable solvable_groups : Utils.solvable_groups;
  mutable white_book : int array array;
}

(*Creates a copy of the current board into a second board*)
val copy_board : board -> board -> unit

(*Creates a new board*)
val make_board : unit -> board

(*initializes the board*)
val initboard : board -> unit

(*Build the opening book*)
val build_book : board -> unit

(*Create a new game*)
val create_game : unit -> board

(*Show the position in the console*)
val show_square : board -> unit

(*Wipe the groups above sq*)
val wipe_above : board -> int -> unit

(*Wipe the odd groups containing an odd
place above sq*)
val wipe_odd : board -> int -> unit

(*Checks group with number group has been wiped*)
val wiped_group : board -> int -> bool

(*Checks is a threat exists*)
val check_threat : board -> int -> int -> int -> bool

(*Checks who owns the place x y*)
val check_men : board -> int -> int -> int

(*Checks if the group has a threat for the given side*)
val check_even_below : board -> int -> int -> bool

val both_groups : board -> int -> int -> unit

val recurse_groups : board -> int -> int array -> int -> bool

val both_many_groups : board -> int -> int array -> unit

val solve_columns : board -> int -> int array -> unit

(*Checks which places can be claimed*)
val check_claim : board -> int array -> unit

(*Generates all the possible instances of before*)
val generate_all_other_before_instances :
  board -> int -> int array -> 'a -> unit

(*Modifies the array of any possible combo of threats*)
val check_double_threat :
  board -> int -> int -> Utils.threat_combo array -> int ref -> unit

(*Gives the column for a combo of threats, -1 if there isn't any*)
val threat_combo : board -> Utils.threat_combo array -> int

(*Wipe the useless groups*)
val wipe_many_groups : board -> int -> int array -> unit

(*Checks if the group has an threat for the player who*)
val threat_group : board -> int -> int -> bool

(*Handles an even threat if there is an odd one lower*)
val handle_even_above_odd : board -> Utils.threat_combo -> unit

(*Handles an odd threat if there is an even one lower*)
val handle_odd_above_even : board -> Utils.threat_combo -> unit

(*Checks how many of your men are connected if you add a piece
in the column x*)
val connected : board -> int -> int

(*Checks how many men connected by the opponent if he adds
a piece in the column x*)
val opponent_connected : board -> int -> int

(*Returns iwho wins, 0 for a draw, -1 if the game isn't
decided yet*)
val get_game_result : board -> int

(*Adds a piece in the column x*)
val makemove : board -> int -> bool

(*Removes the highest piece in the column x*)
val undomove : board -> int -> bool
