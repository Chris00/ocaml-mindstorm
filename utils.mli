val boardX : int
val boardY : int
val maxsquares : int
val groups : int
val maxsols : int
val maxgroups : int
val alloc_solutions : int
val tiles : int
val maxmen : int
val claimeven_ : int
val baseinverse_ : int
val vertical_ : int
val aftereven_ : int
val lowinverse_ : int
val highinverse_ : int
val baseclaim_ : int
val before_ : int
val specialbefore_ : int
val impossible : int
val elm : int -> int -> int
val elx : int -> int
val ely : int -> int
val int_to_tab : int -> int array
val memset : 'a array -> 'a -> int -> unit
val tab_copy : 'a array -> 'a array -> unit
val sq_a1 : int
val sq_a2 : int
val sq_a3 : int
val sq_a4 : int
val sq_a5 : int
val sq_a6 : int
val sq_b1 : int
val sq_b2 : int
val sq_b3 : int
val sq_b4 : int
val sq_b5 : int
val sq_b6 : int
val sq_c1 : int
val sq_c2 : int
val sq_c3 : int
val sq_c4 : int
val sq_c5 : int
val sq_c6 : int
val sq_d1 : int
val sq_d2 : int
val sq_d3 : int
val sq_d4 : int
val sq_d5 : int
val sq_d6 : int
val sq_e1 : int
val sq_e2 : int
val sq_e3 : int
val sq_e4 : int
val sq_e5 : int
val sq_e6 : int
val sq_f1 : int
val sq_f2 : int
val sq_f3 : int
val sq_f4 : int
val sq_f5 : int
val sq_f6 : int
val sq_g1 : int
val sq_g2 : int
val sq_g3 : int
val sq_g4 : int
val sq_g5 : int
val sq_g6 : int
val blstrsize : int
val goodmove : int
val badmove : int
val switch : int -> int
val and_type : int
val or_type : int
type threat_combo = {
  mutable cross : int;
  mutable even : int;
  mutable odd : int;
  mutable gp1 : int;
  mutable gp2 : int;
}
type solvable_groups = { squar : int array array; sqpnt : int array; }
type solution = {
  mutable valid : bool;
  mutable solname : int;
  solpoint : int array;
  sqinv : int array;
  mutable sqinvnumb : int;
  solgroups : int array;
  mutable solgroupsnumb : int;
}
type intgp = {
  tgroups : bool array;
  mutable j : int;
  mutable k : int;
  mygroups : bool array;
}
(*Creates a random boolean, based on the clock*)
val random_bool : unit -> bool

(*Creates a random int, based on the clock*)
val random_int : int -> int
