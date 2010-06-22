val rulecombo : int array array
(** The possibilities of combinaison between the rules *)

exception Combinaison_error
(** Exception raised when 2 rules are not compatible. *)

val overlap : Board.t -> int -> int -> bool
(** [overlap p1 p2] checks whether the the solutions [p1] and [p2] of the
    current board are overlapping.  *)

val column_wdoe : Board.t -> int -> int -> bool
(** [column_wdoe p1 p2] checks whether the solutions [p1] and [p2] of
    the current board are compatible. *)

val comp_rules : Board.t -> int -> int -> bool
(** [comp_rules p1 p2] checks whether the solutions [p1] and [p2] have
    a common solution for the current board. *)

val build_adjacency_matrix : Board.t -> bool array array
(** Build the Adjacentry Matrix for the current board. *)
