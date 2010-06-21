(*Constantes et petites fonctions*)

let boardX = 7
and boardY = 6
let maxsquares = boardX*boardY
and groups = 69
and maxsols = 700
and maxgroups = 50
and alloc_solutions = 621
and tiles = 4
and maxmen = 42
and claimeven_ = 1
and baseinverse_ = 2
and vertical_ = 3
and aftereven_ = 4
and lowinverse_ = 5
and highinverse_ = 6
and baseclaim_ = 7
and before_ = 8
and specialbefore_ = 9
and impossible = -30000
and elm x y = (x+(y lsl 3))
and elx z = z land 7
and ely z = z lsr 3
and int_to_tab z =
   [|z land 0xFF;z lsr 8|]
and memset tab a n =
  for i=0 to n - 1 do
    tab.(i) <- a
  done
and tab_copy tab cp =
  for i=0 to (Array.length cp) - 1 do
    cp.(i) <- tab.(i)
  done

let sq_a1 = elm 0 0
and sq_a2 = elm 0 1
and sq_a3 = elm 0 2
and sq_a4 = elm 0 3
and sq_a5 = elm 0 4
and sq_a6 = elm 0 5
and sq_b1 = elm 1 0
and sq_b2 = elm 1 1
and sq_b3 = elm 1 
2
and sq_b4 = elm 1 3
and sq_b5 = elm 1 4
and sq_b6 = elm 1 5
and sq_c1 = elm 2 0
and sq_c2 = elm 2 1
and sq_c3 = elm 2 2
and sq_c4 = elm 2 3
and sq_c5 = elm 2 4
and sq_c6 = elm 2 5
and sq_d1 = elm 3 0
and sq_d2 = elm 3 1
and sq_d3 = elm 3 2
and sq_d4 = elm 3 3
and sq_d5 = elm 3 4
and sq_d6 = elm 3 5
and sq_e1 = elm 4 0
and sq_e2 = elm 4 1
and sq_e3 = elm 4 2
and sq_e4 = elm 4 3
and sq_e5 = elm 4 4
and sq_e6 = elm 4 5
and sq_f1 = elm 5 0
and sq_f2 = elm 5 1
and sq_f3 = elm 5 2
and sq_f4 = elm 5 3
and sq_f5 = elm 5 4
and sq_f6 = elm 5 5
and sq_g1 = elm 6 0
and sq_g2 = elm 6 1
and sq_g3 = elm 6 2
and sq_g4 = elm 6 3
and sq_g5 = elm 6 4
and sq_g6 = elm 6 5
and blstrsize = 14
and goodmove = 16384
and badmove = -16384
and switch a = a lxor 3
and and_type = 1
and or_type = 2

(*Types de base*)
type threat_combo = {
  mutable cross : int;
  mutable even : int;
  mutable odd : int;
  mutable gp1 : int;
  mutable gp2 : int
}


type solvable_groups = {
  squar : int array array;
  sqpnt : int array
}

type solution = {
  mutable valid : bool;
  mutable solname : int;
  solpoint : int array;
  sqinv : int array;
  mutable sqinvnumb : int;
  solgroups : int array;
  mutable solgroupsnumb : int
}

type intgp = {
  tgroups : bool array;
  mutable j : int;
  mutable k : int;
  mygroups : bool array
}

let random_bool() = int_of_float (Unix.gettimeofday() /. 7.) mod 2 = 0

let random_int n = int_of_float (Unix.gettimeofday() /. 7.) mod n
