(** Basic type of the game. *)
type t = {
  mutable square : int ref array;
  (** The current position *)
  mutable wipesq : int array;
  (** The possible group wiped *)
  mutable usablegroup : bool array;
  (** The groups usable *)
  mutable sqused : bool array;
  (** The matrix of the useless squares *)
  mutable stack : int array;
  (** The number of pieces in each column *)
  mutable groups : int ref array array;
  (** the 69 possible ways to win the game *)
  mutable xplace : int array array;
  (** the possible row to play *)
  mutable yplace : int array array;
  (** the possible columns *)
  mutable turn : int;
  (** the player who can play the next piece *)
  mutable moves : int array;
  (** the list of moves in the game *)
  mutable choices : int array;
  (** the choices taken by the IA *)
  mutable mlist : int array;
  (** The list of mouvements *)
  mutable filled : int;
  (** the number of men *)
  mutable intgp : Utils.intgp;
  (** the groups possible for a win *)
  mutable solution : Utils.solution array;
  (** the array of possible solutions *)
  mutable sp : int;
  (** current solution checked *)
  mutable problem_solved : int;
  (** the column to play to solve the problem *)
  mutable solused : int;
  (** the solution used *)
  mutable oracle : bool array;
  (** the array saying if the players have a solution with the
      heuristic *)
  mutable oracle_guesses : int;
  (** the number of guesses of the IA *)
  mutable lastguess : int;
  (** his last guess *)
  mutable nodes_visited : int;
  (** the number of nodes visited in total *)
  mutable maxtreedepth : int;
  (** the max tree depth *)
  mutable rules : int array;
  (** the array of the rules *)
  mutable instances : int array;
  (** the number of time each rule was used *)
  mutable solvable_groups : Utils.solvable_groups;
  (** the array of groups solvable *)
  mutable white_book : int array array;
  (** the book of openings *)
}

(*Creates a copy of the current board into a second board*)
val copy : t -> t -> unit

(*Creates a new board (and initializes it) *)
val make : unit -> t

(*Build the opening book*)
val build_book : t -> unit

(*Create a new game*)
val create_game : unit -> t

(*Show the position in the console*)
val show_square : t -> unit

(*Wipe the groups above sq*)
val wipe_above : t -> int -> unit

(*Wipe the odd groups containing an odd
place above sq*)
val wipe_odd : t -> int -> unit

(*Checks group with number group has been wiped*)
val wiped_group : t -> int -> bool

(*Checks is a threat exists*)
val check_threat : t -> int -> int -> int -> bool

(*Checks who owns the place x y*)
val check_men : t -> int -> int -> int

(*Checks if the group has a threat for the given side*)
val check_even_below : t -> int -> int -> bool

val both_groups : t -> int -> int -> unit

val recurse_groups : t -> int -> int array -> int -> bool

val both_many_groups : t -> int -> int array -> unit

val solve_columns : t -> int -> int array -> unit

(*Checks which places can be claimed*)
val check_claim : t -> int array -> unit

(*Generates all the possible instances of before*)
val generate_all_other_before_instances :
  t -> int -> int array -> 'a -> unit

(*Modifies the array of any possible combo of threats*)
val check_double_threat :
  t -> int -> int -> Utils.threat_combo array -> int ref -> unit

(*Gives the column for a combo of threats, -1 if there isn't any*)
val threat_combo : t -> Utils.threat_combo array -> int

(*Wipe the useless groups*)
val wipe_many_groups : t -> int -> int array -> unit

(*Checks if the group has an threat for the player who*)
val threat_group : t -> int -> int -> bool

(*Handles an even threat if there is an odd one lower*)
val handle_even_above_odd : t -> Utils.threat_combo -> unit

(*Handles an odd threat if there is an even one lower*)
val handle_odd_above_even : t -> Utils.threat_combo -> unit

(*Checks how many of your men are connected if you add a piece
in the column x*)
val connected : t -> int -> int

(*Checks how many men connected by the opponent if he adds
a piece in the column x*)
val opponent_connected : t -> int -> int

(*Returns iwho wins, 0 for a draw, -1 if the game isn't
decided yet*)
val get_game_result : t -> int

(*Adds a piece in the column x*)
val makemove : t -> int -> bool

(*Removes the highest piece in the column x*)
val undomove : t -> int -> bool
