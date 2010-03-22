(*Constantes et petites fonctions*)

let boardX = 7
and boardY = 6
and goodmove = infinity
and badmove = neg_infinity
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
and elm x y = (x+(y lsl 3))
and elx z = z land 7
and ely z = z lsr 3
and int_to_tab z =
   [|(z lsr 8) land 0xFF;z land 0xFF|]
and memset tab a n =

  for i=0 to n - 1 do
    tab.(i) <- a
  done
and switch a = a lxor 3

let sq_a1 = elm 0 0
and sq_a2 = elm 0 1
and sq_a3 = elm 0 2
and sq_a4 = elm 0 3
and sq_a5 = elm 0 4
and sq_a6 = elm 0 5
and sq_b1 = elm 1 0
and sq_b2 = elm 1 1
and sq_b3 = elm 1 2
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

type board = {
  square : int ref array;
  mutable wipesq : int array;
  mutable usablegroup : bool array;
  mutable sqused : bool array;
  stack : int array;
  groups : int ref array array;
  xplace : int array array;
  yplace : int array array;
  mutable turn : int;
  moves : int array;
  choices : int array;
  mlist : int array;
  mutable filled : int;
  intgp : intgp;
  solution : solution array;
  mutable sp : int;
  mutable problem_solved : int;
  mutable solused : int;
  oracle : int array;
  oracle_guesses : int;
  lastguess : int;
  bestguess : int;
  nodes_visited : int;
  maxtreedepth : int;
  rules : int array;
  instances : int array;
  wins : int array;
  draws : int;
  lastwin : int;
  solvable_groups : solvable_groups;
  white_book : int array;
  black_book : int array;
  wbposit : int;
  bbposit : int;
  lastob : int;
  autotest : int;
  mutable cpu : int;
  white_lev : int;
  black_lev : int;
} 

let make_board() =
  let solv =
    {
      squar = Array.init 64 (fun i -> Array.init 16 (fun j -> 0));
      sqpnt = Array.make 64 0
    }
  and intg =
    {
      tgroups = Array.make groups true;
      j = 0;
      k = 0;
      mygroups = Array.make groups true
    } 
  and sol =
    {
      valid = true;
      solname = -1;
      solpoint = Array.init 2 (fun i -> 0);
      sqinv = Array.init (2*tiles) (fun i -> 0);
      sqinvnumb = 0;
      solgroups = Array.init groups (fun i -> 0);
      solgroupsnumb = 0
    } in
  let board =
    {
      wins = Array.init 2 (fun i -> 0);
      draws = 0;
      lastguess = 0;
      bestguess = maxmen;
      lastwin = 0;
      white_lev = 0;
      black_lev = 0;
      autotest = 0;
      rules = Array.init 3 (fun i -> 0);
      oracle_guesses = 0;
      instances = Array.init 10 (fun i -> 0);
      turn = 1;
      filled = 0;
      cpu = 1;
      bbposit =0;
      groups = Array.init 69 (fun i -> (Array.init 4 (fun j -> ref 0)));
      xplace = Array.init 69 (fun i -> (Array.init 4 (fun j -> 0)));
      yplace = Array.init 69 (fun i -> (Array.init 4 (fun j -> 0)));
      square = Array.init ((boardX+1)*(boardY+2)) (fun i -> ref 0);
      wipesq = Array.init ((boardX+1)*(boardY+2)) (fun i -> 0);
      usablegroup = Array.init groups (fun i -> true);
      sqused = Array.init ((boardX+1)*(boardY+2)) (fun i -> false);
      stack = Array.init (boardX+1) (fun i -> 0);
      moves = Array.init maxmen (fun i -> 0);
      solvable_groups = solv;
      choices = Array.init maxmen (fun i -> 0);
      mlist = Array.init maxmen (fun i -> 0);
      intgp = intg;
      solution = Array.init alloc_solutions (fun i -> sol);
      sp = -1;
      problem_solved = 0;
      solused = -1;
      oracle = Array.init 2 (fun i -> 0);
      nodes_visited = 0;
      maxtreedepth = 0;
      white_book = Array.init 1 (fun i -> 0);
      black_book = Array.init 1 (fun i -> 0);
      wbposit = 0;
      lastob = 0
    }
  in board


let initboard board =
    let i = ref 0 in
      for y=0 to boardY-1 do
	for x=0 to boardX-4 do
	  for k=0 to 3 do
	    board.groups.(!i).(k) <- board.square.(elm (x+k) y);
	    board.xplace.(!i).(k) <- x+k;
	    board.yplace.(!i).(k) <- y
	  done;
	    i:=!i+1
	done
      done;

      for y=0 to boardY-4 do
	for x=0 to boardX-1 do
	  for k=0 to 3 do
	    board.groups.(!i).(k) <- (board.square.(elm x (y+k)));
	    board.xplace.(!i).(k) <- x;
	    board.yplace.(!i).(k) <- y+k
	  done;
	    i:=!i+1
	done
      done;

      for y=0 to boardY-4 do
	for x=0 to boardX-4 do
	  for k=0 to 3 do
	    board.groups.(!i).(k) <- (board.square.(elm (x+k) (y+k)));
	    board.xplace.(!i).(k) <- x+k;
	    board.yplace.(!i).(k) <- y+k
	  done;
	    i:=!i+1
	done
      done;

      for y=3 to boardY-1 do
	for x=0 to boardX-4 do
	  for k=0 to 3 do
	    board.groups.(!i).(k) <- (board.square.(elm (x+k) (y-k)));
	    board.xplace.(!i).(k) <- x+k;
	    board.yplace.(!i).(k) <- y-k
	  done;
	  i:=!i+1
	done
      done;
	
      for x=0 to 63 do
	board.solvable_groups.sqpnt.(x) <- 0;
	for y=0 to 15 do
	  board.solvable_groups.squar.(x).(y) <- -1
	done
      done;
	    
      for x=0 to boardX-1 do
	for y=0 to boardY-1 do
	  board.square.(elm x y) := elm x y
	done
      done;

	for i=0 to groups-1 do
	  for j=0 to tiles-1 do
	    let p = !(board.groups.(i).(j)) in
	      board.solvable_groups.squar.(p).(board.solvable_groups.sqpnt.(p))<-i;
	      board.solvable_groups.sqpnt.(p)
	      <- board.solvable_groups.sqpnt.(p) + 1
	  done
	done;
      for i=0 to 7 do
	board.square.(elm 7 i) := -1;
	board.square.(elm i 6) := -1
      done;
      board.stack.(7) <- -1;
      for y=0 to boardY-1 do
	for x=0 to boardX-1 do
	  board.square.(elm x y) := 0
	done
      done

let gen_odd_threat board x side =
  let emp = Array.make tiles 0
  and empty = ref 0
  and fill = ref 0
  and px = ref 0
  and py = ref 0 in
    for y=0 to tiles-1 do
      if !(board.groups.(x).(y)) = 0 then
	(
	  px := board.xplace.(x).(y);
	  py := board.yplace.(x).(y);
	  emp.(!empty) <- elm !px !py;
	  empty := !empty+1
	)
      else if !(board.groups.(x).(y)) = side then fill := !fill+1
    done;
    if !empty=1 && !fill=3 && (!py land 1) = 0 && board.stack.(!px) < !py then
      emp.(0)
    else -1

let pentas board x y side =
  if board.stack.(x)>=y || board.stack.(x+2)>=y || board.stack.(x+4)>=y ||
    !(board.square.(elm (x+1) y))<>side || !(board.square.(elm (x+3) y))<>side
      then false else true


let check_pentas board side =
  let rec helper x y bol =
    if y<2 then
      (
	let px = x and py = (y+1)*2 in
	helper x (y+1) (bol || pentas board px py side)
      )
    else bol in
  let rec helper2 x flag =
    if x<3 then
      if helper 0 0 false then true
      else helper2 (x+1) flag
    else flag
  in helper2 0 false


let check_double board group pos side =
  let rec helper x =
    if x<groups then
    let y = gen_odd_threat board x side in
      if y=pos then true
      else helper (x+1)
    else false
  in helper (group+1)


let groupeval board =
  let t1 = board.turn in
  let t2 = switch t1
  and score = ref 0. in
    for x = 0 to groups-1 do
      let p1 = ref 0
      and p2 = ref 0 in
	for i = 0 to 3 do
	  if !(board.groups.(x).(i)) = t1 then p1 := !p1+1
	  else if !(board.groups.(x).(i)) = t2 then p2 := !p2+1
	done;
	if !p1 = 4 then score := !score +. goodmove
	else if !p2 = 4 then score := !score +. badmove
	else if !p1 = 3 && !p2 = 0 then
	  (
	    score := !score +. 1.;
	    let z = gen_odd_threat board x t1 in
	      if z <> -1 then
		let f = check_double board x z t1 in
		  if not f then
		    if t1 = 1 then score := !score +. 200.
		    else score := !score +. 150.
		  else if t1 = 1 then score := !score +. 750.
		  else score := !score +. 500.
	  )
	else if !p2 = 3 && !p1 = 0 then
	  (
	    score := !score -. 1.;
	    let z = gen_odd_threat board x t2 in
	      if z <> -1 then
		let f = check_double board x z t2 in
		  if not f then
		    if t1 = 2 then score := !score -. 200.
		    else score := !score -. 150.
		  else if t1 = 2 then score := !score -. 750.
		  else score := !score -. 500.
	  )
	else if !p1 = 2 && !p2 = 0 then score := !score +. 10.
	else if !p2 = 2 && !p1 = 0 then score := !score -. 10.;
	
	if check_pentas board 1 then
	  if t1 = 1 then score := !score +. 800.
	  else score := !score -. 800.
    done;
    !score
      


let connected board move =
  let rec verti y connect =
    let px = move in
      if y>=0 && !(board.square.(elm px y)) = board.turn then
	verti (y-1) (connect+1)
      else connect in
  let rec hori_left x connect =
    let py = board.stack.(move) in
      if x>=0 && !(board.square.(elm x py)) = board.turn then
	hori_left (x-1) (connect+1)
      else connect in
  let rec hori_right x connect =
    let py = board.stack.(move) in
      if x>=0 && !(board.square.(elm x py)) = board.turn then
	hori_right (x+1) (connect+1) 
      else connect in
  let rec diago_NW_left x y connect =
    if x>=0 && y<boardY && !(board.square.(elm x y)) = board.turn then
      diago_NW_left (x-1) (y+1) (connect+1) 
    else connect in
  let rec diago_NW_right x y connect =
    if x<boardX && y>=0 && !(board.square.(elm x y)) = board.turn then
      diago_NW_right (x+1) (y-1) (connect+1)
    else connect in
  let rec diago_NE_left x y connect =
    if x>=0 && y>=0 && !(board.square.(elm x y)) = board.turn then
      diago_NE_left (x+1) (y+1) (connect+1)
    else connect in
  let rec diago_NE_right x y connect =
    if x<boardX && y<boardY && !(board.square.(elm x y)) = board.turn then
      diago_NE_right (x-1) (y-1) (connect+1)
    else connect in
  let h_l = hori_left (move-1) 1
  and d_nw_l = diago_NW_left (move-1) (board.stack.(move)+1) 1 
  and d_ne_l = diago_NE_left (move-1) (board.stack.(move)-1) 1 in
  let h = hori_right (move+1) h_l
  and d_ne = diago_NW_right (move+1) (board.stack.(move)-1) d_nw_l
  and d_nw = diago_NE_right (move+1) (board.stack.(move)+1) d_ne_l
  and v = verti (board.stack.(move)-1) 1 in
    max (max h v) (max d_ne d_nw)


let opponent_connected board move =
  board.turn <- switch board.turn;
  let connect = connected board move in
    board.turn <- switch board.turn;
    connect

let draw board =
  board.filled = 42

let get_game_result board =
  let answer = ref (-1) and i = ref 0 in
    while !i<groups do
      if !(board.groups.(!i).(0)) <> 0 &&
	!(board.groups.(!i).(0)) = !(board.groups.(!i).(1)) &&
	!(board.groups.(!i).(0)) = !(board.groups.(!i).(2)) &&
	!(board.groups.(!i).(0)) = !(board.groups.(!i).(3)) then
	  (
	    answer := !(board.groups.(!i).(0));
	    i := groups
	  )
      else i:= !i+1
    done;
    if !answer = -1 && board.filled = maxsquares then 0
    else !answer
    

      
let makemove board move =
  if board.stack.(move) >= 6 then false
  else
    (
      board.square.(elm move (board.stack.(move))) := board.turn;
      board.moves.(board.filled) <- move;
      board.mlist.(board.filled) <- elm move (board.stack.(move));
      board.turn <- switch board.turn;
      board.stack.(move) <- board.stack.(move)+1;
      board.filled <- board.filled + 1;
      true
    )

let undomove board move =
  if board.stack.(move)<1 then false
  else
    (
      board.stack.(move) <- board.stack.(move)-1;
      board.square.(elm move (board.stack.(move))) := 0;
      board.moves.(board.filled) <- -1;
      board.mlist.(board.filled) <- -1;
      board.turn <- switch board.turn;
      board.filled <- board.filled - 1;
      true
    )
