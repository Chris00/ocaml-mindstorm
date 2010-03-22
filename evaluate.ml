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
and elm x y = (x+(y lsl 3))
and elx z = z land 7
and ely z = z lsr 3
and int_to_tab z =
   [|(z lsr 8) land 0xFF;z land 0xFF|]
and memset tab a n =

  for i=0 to n - 1 do
    tab.(i) <- a
  done

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

(*Utilitaire*)
let show_square_used board =
  for j=0 to boardY - 1 do
    for i=0 to boardX - 1 do
      if board.sqused.(elm i (boardY-j-1)) then 
	Printf.printf "*"
      else Printf.printf "."
    done;
    Printf.printf "\n"
  done;;


(*Fonctions de check des squares et groups*)
let odd_threat board x =
  let emp = Array.make tiles 0 in
  let rec helper empty fill y a b =
    if y < tiles then
      if !(board.groups.(x).(y)) = 0 then
	let px = board.xplace.(x).(y)
	and py = board.yplace.(x).(y) in
	  emp.(empty) <- elm px py;
	  helper (empty+1) fill (y+1) px py
      else if !(board.groups.(x).(y)) = 1 then
	helper empty (fill+1) (y+1) a b
      else helper empty fill (y+1) a b
    else (empty, fill,a,b) in
  let (empty, fill,px,py) = helper 0 0 0 0 0 in
    if empty = 1 && fill = 3 && (py land 1 = 0) &&
      board.stack.(px) < py then emp.(0)
    else -1

let wipe_above board sq =
  let x = elx sq
  and y = ely sq in
    for z=y to boardY-1 do
      board.wipesq.(elm x z) <- 1
    done

let wipe_odd board sq =
  let x = elx sq in
  let y = (board.stack.(x) land 0x0E) + 2 in
  let ye = ely y in
    for z=y to ye do
      if (z-y) land 1 = 0 then board.wipesq.(elm x z) <- 1
    done

let wiped_group board group =
  let rec helper x value =
    if value=1 then true
    else if x<tiles then
      if board.wipesq.(elm board.xplace.(group).(x)
			 board.yplace.(group).(x)) = 1 then
	helper (x+1) 1
      else helper (x+1) 0
    else false
  in helper 0 0

let check_threat board px i side =
  let rec helper y =
    if y < board.solvable_groups.sqpnt.(elm px i) then
      let j = board.solvable_groups.squar.(elm px i).(y) in
	if board.xplace.(j).(0) = board.xplace.(j).(3) then
	  helper (y+1)
	else
	  (
	  let rec help_intern x a b =
	    if x < tiles then
	      let fx = board.xplace.(j).(x)
	      and fy = board.yplace.(j).(x) in
	      let (p1,p2) = 
		if !(board.square.(elm fx fy)) = side then (a+1,b)
		else if !(board.square.(elm fx fy)) = 0 then (a,b+1)
		else (a,b) in
		help_intern (x+1) p1 p2
	    else a+b = tiles
	  in let bool = help_intern 0 0 0 in
	    if bool then true else helper (y+1)
	  )
    else false
  in helper 0
		  
let check_men board group side =
  let rec helper x a b =
    if x < tiles then
      let fx = board.xplace.(group).(x)
      and fy = board.yplace.(group).(x) in
      let (p1,p2) =
	if !(board.square.(elm fx fy)) = side then (a+1,b)
	else if !(board.square.(elm fx fy)) = 0 then (a,b+1)
	else (a,b) in helper (x+1) p1 p2
    else (a,b) in
  let (p1,p2) = helper 0 0 0 in
    if p1+p2 = 4 then p1 else p2

let check_even_below board square side =
  let px = elx square and py = ely square in
  let rec helper i =
    if i < py then
      if !(board.square.(elm px i)) = 0 && (check_threat board px i side)
      then true
      else helper (i+2)
    else false
  in helper 1

let count_odd_threats board threats =
  let rec helper x oddpnt =
    if x < groups then
      let y = odd_threat board x in
	if y = (-1) then helper (x+1) oddpnt
	else 
	  (
	    threats.(oddpnt) <- y;
	    helper (x+1) (oddpnt + 1)
	  )
    else oddpnt
  in helper 0 0
	  
let both_groups board q1 q2 = 
  let p1 = board.solvable_groups.sqpnt.(q1)
  and p2 = board.solvable_groups.sqpnt.(q2) in
  for x=0 to p1-1 do
    for y=0 to p2-1 do
      let g1=board.solvable_groups.squar.(q1).(x)
      and g2=board.solvable_groups.squar.(q2).(y) in
	if g1=g2 && board.intgp.tgroups.(g1) then
	  (
	    board.solution.(board.sp).solgroups.
	      (board.solution.(board.sp).solgroupsnumb) <- g1;
	    board.solution.(board.sp).solgroupsnumb <-
	      board.solution.(board.sp).solgroupsnumb +1
	  )
    done;
  done

let rec recurse_groups board cols cl gp =
  let p = board.solvable_groups.sqpnt.(cl.(0)) in
  let rec helper i =
    if i<p then
      let g1 = board.solvable_groups.squar.(cl.(0)).(i) in
	if g1<>gp then helper (i+1)
	else if cols=1
	  || recurse_groups board (cols-1) (int_to_tab cl.(1)) g1 then
	    true
	else helper (i+1)
    else false
  in helper 0
       
let both_many_groups board cols cl =
  if cols <> 0 then
    let p = board.solvable_groups.sqpnt.(cl.(0)) in
    let rec helper i =
      if i<p then
	let g1 = board.solvable_groups.squar.(cl.(0)).(i) in
	  if (not board.intgp.tgroups.(g1)) then helper (i+1)
	  else if cols=1 || recurse_groups board (cols-1)
	    (int_to_tab cl.(1)) g1 then
	    (
	    board.solution.(board.sp).solgroups.
	      (board.solution.(board.sp).solgroupsnumb) <- g1;
	    board.solution.(board.sp).solgroupsnumb <-
	      board.solution.(board.sp).solgroupsnumb +1
	    )
	  else helper (i+1)
    in helper 0

let solve_columns board cl cols =
  let rec helper i =
    if (not board.intgp.tgroups.(i)) then helper (i+1)
    else
      (
	let answer = ref true
	and j = ref 0 and k = ref 0 in
	  while (!j<cl && !answer) do
	    answer := false;
	    let px = elx cols.(!j) and py = ely cols.(!j) in
	      while (!k<tiles && not (!answer)) do
		if !(board.groups.(i).(!k)) = 0 && px=board.xplace.(i).(!k)
		  && py <=board.yplace.(i).(!k) then answer := true
		else k := !k+1
	      done;
	      j := !j+1
	  done;
	  if !j = cl && !answer then
	    if board.solution.(board.sp).solgroupsnumb = 0 then
	      (
		board.solution.(board.sp).sqinvnumb <- 2*cl;
		for t=0 to cl-1 do
		  let tx = elx cols.(t) and ty = ely cols.(t) in
		    board.solution.(board.sp).sqinv.(t) <- elm tx (ty-1);
		    board.solution.(board.sp).sqinv.(t+cl) <- elm tx ty;
		done;
		board.solution.(board.sp).solgroups.
		  (board.solution.(board.sp).solgroupsnumb) <- i;
		board.solution.(board.sp).solgroupsnumb <-
		  board.solution.(board.sp).solgroupsnumb +1
	      )
      )
  in helper 0

let check_claim board cl = 
  let px = elx cl.(1) and py = elx cl.(1) in
    if py<boardY && (py land 1 = 1) then
	 (
	   board.solution.(board.sp).solgroupsnumb <- 0;
	   board.solution.(board.sp).solname <- baseinverse_;
	   board.solution.(board.sp).sqinv.(0) <- cl.(0);
	   board.solution.(board.sp).sqinv.(1) <- cl.(1);
	   board.solution.(board.sp).sqinv.(2) <- cl.(2);
	   board.solution.(board.sp).sqinv.(3) <- elm px py;
	   board.solution.(board.sp).sqinvnumb <- 4;
	   board.solution.(board.sp).solpoint.(0) <- cl.(0);
	   board.solution.(board.sp).solpoint.(1) <- elm px py;
	   both_groups board cl.(0) (elm px py);
	   board.instances.(baseclaim_) <-
	     board.instances.(baseclaim_) + 1;
	   if board.solution.(board.sp).solgroupsnumb > 0 then
	       (
		 both_groups board cl.(1) cl.(2);
		 board.sp <- board.sp + 1
	       );
	   board.solution.(board.sp).solgroupsnumb <- 0;
	   board.solution.(board.sp).solname <- baseinverse_;
	   board.solution.(board.sp).sqinv.(0) <- cl.(0);
	   board.solution.(board.sp).sqinv.(1) <- cl.(1);
	   board.solution.(board.sp).sqinv.(2) <- cl.(2);
	   board.solution.(board.sp).sqinv.(3) <- elm px py;
	   board.solution.(board.sp).sqinvnumb <- 4;
	   board.solution.(board.sp).solpoint.(0) <- elm px py;
	   board.solution.(board.sp).solpoint.(1) <- cl.(2);
	   both_groups board (elm px py) cl.(2);
	   board.instances.(baseclaim_) <-
	     board.instances.(baseclaim_) + 1;
	   if board.solution.(board.sp).solgroupsnumb > 0 then
	     (
	       both_groups board cl.(0) cl.(1);
	       board.sp <- board.sp + 1
	     )
	 )

let generate_all_other_before_instances board cols cl j =
  let step = 128 lsr cols
  and gc = Array.init 4 (fun j -> (Array.init 3 (fun i -> 0))) in
    for x=0 to cols - 1 do
      let px = elx cl.(x)
      and py = ely cl.(x) in
	gc.(x).(2) <- cl.(x);
	gc.(x).(1) <- elm px (py-1);
	if board.stack.(px)<py-2 then gc.(x).(0) <- elm px (py-2)
	else gc.(x).(0) <- -1
    done;
    let rec helper cnt =
      if cnt < 128 then
	(
	  let pn = Array.init 4 (fun i -> (cnt lsr 6) land 1)
	  and sl = Array.init 4 (fun l -> (Array.init 2 (fun l -> 0)))
	  and flag = ref true and j = ref 0 and k = ref 0 in
	    for x=0 to cols-1 do
	      sl.(x).(1) <- gc.(x).(1+pn.(x));
	      sl.(x).(0) <- gc.(x).(pn.(x))
	    done;
	    while !j<cols && !flag do
	      if sl.(!j).(0) = -1 then flag := false;
	      j := !j+1
	    done;
	    j := 0;
	    while !j<2 && !flag do
	      while !k<cols && !flag do
		if board.sqused.(sl.(!k).(!j)) then flag := false;
		k := !k+1
	      done;
	      j := !j+1
	    done;
	    if !flag then
	      (
		board.solution.(board.sp).solgroupsnumb <- 0;
		board.solution.(board.sp).solname <- before_;
		board.solution.(board.sp).solpoint.(0) <-
		  elm board.xplace.(!j).(0) board.yplace.(!j).(0);
		board.solution.(board.sp).solpoint.(1) <-
		  elm board.xplace.(!j).(3) board.yplace.(!j).(3);
		board.solution.(board.sp).sqinvnumb <- 2*cols;
		for x=0 to (2*cols)-1 do
 		  board.solution.(board.sp).sqinv.(x)<-
		    sl.(x lsr 1).(x land 1)
		done;
		for x = 0 to cols-1 do
		  let py2 = ely sl.(x).(1) in
		    if py2 land 1 = 1 then
		      both_many_groups board 1 (int_to_tab sl.(x).(1))
		    else both_groups board sl.(x).(0) sl.(x).(1)
		done;
		both_many_groups board cols cl;
		board.instances.(before_) <- board.instances.(before_)+1;
		if board.solution.(board.sp).solgroupsnumb>0 then
		  board.sp <- board.sp + 1
	      );
	    helper (cnt+step)
	)
    in helper 0

let check_double_threat board x y tch pnt =
  let pq = elm x y in
    for j=0 to board.solvable_groups.sqpnt.(pq)-2 do
      for k=(j+1) to board.solvable_groups.sqpnt.(pq)-1 do
	let jg = board.solvable_groups.squar.(pq).(j)
	and kg = board.solvable_groups.squar.(pq).(k) in
	let w1 = check_men board jg 1
	and w2 = check_men board kg 1 
	and g1 = ref 0
	and g2 = ref 0 in
	  if (w1=2 && w2=2) then
	    (
	      for wx=0 to tiles-1 do
		let px = board.xplace.(jg).(wx)
		and py = board.yplace.(jg).(wx) in
		  if !(board.groups.(jg).(wx)) = 0 && (px <> x || py<>y)
		  then g1 := elm px py
	      done;
	      for wx = 0 to tiles-1 do
		let px = board.xplace.(kg).(wx)
		and py = board.yplace.(kg).(wx) in
		  if !(board.groups.(jg).(wx)) = 0 && (px<>x || py<>y) then
		    g2 := elm px py
	      done; 
	      if elx !g1 = elx !g2 &&
		(ely !g1 - ely !g2 = 1 || ely !g1-ely !g2= -1) then
		  (
		  tch.(!pnt).cross <- pq; 
		  if (ely !g1) land 1 = 1 then
		    (
		      tch.(!pnt).even <- !g1;
 		      tch.(!pnt).odd <- !g2
		    )
		  else
		    (
		      tch.(!pnt).odd <- !g1;
 		      tch.(!pnt).even <- !g2
		    );
		  tch.(!pnt).gp1 <- jg;
		  tch.(!pnt).gp2 <- kg;
		  pnt := !pnt+1
		)
	    )
      done;
    done
      
let threat_combo board tc = 
  let z = ref 0 in
  let rec helper y =
    if y<boardY then
      (
	for x=0 to boardX-1 do
	  if board.stack.(x)<y then check_double_threat board x y tc z
	done;
	helper (y+2)
      )
  in helper 2;
    !z

let wipe_many_groups board cols cl =
  if cols <> 0 then
    let p=board.solvable_groups.sqpnt.(cl.(0)) in
      for i=0 to p-1 do
	let g1=board.solvable_groups.squar.(cl.(0)).(i) in
	  if board.usablegroup.(g1) then
	    if cols=1 || (recurse_groups board (cols-1)
			    (int_to_tab cl.(1)) g1)
	    then board.usablegroup.(g1) <- false
      done

let handle_even_above_odd board tc =
  (*Rule one*)
  
  wipe_odd board (tc.cross);
  
  (*Rule two*)

  let cl = Array.make 2 0
  and px = elx (tc.cross)
  and py = (ely (tc.cross)) + 1
  and qx = elx (tc.odd)
  and qy = (ely (tc.odd)) + 1 in
    for y1=qy to boardY-1 do
      for y2=py to boardY-1 do
	cl.(0) <- elm qx y1;
	cl.(1) <- elm px y2;
	wipe_many_groups board 2 cl
      done;
    done;
    
    (*Rule three*)
    
    cl.(0)<-tc.odd;
    cl.(1)<-elm (elx (tc.cross)) ((ely (tc.cross)) + 1);
    wipe_many_groups board 2 cl;    
    
    (*Rule four*)

    if board.stack.(qx)=ely (tc.odd) then
      (
	let sx = px and sy = py + 1 in
	  wipe_above board (elm sx sy)
      );
    (*Rule five*)
    
    if board.stack.(px) land 1 = 0 && board.stack.(qx) < qy - 1 then
      (
	cl.(0) <- elm px (board.stack.(px));
	cl.(1) <- elm qx (board.stack.(qx));
	wipe_many_groups board 2 cl
      );
    
    (*Rule Six*)
    
    let qx = elx (tc.odd) and qy = board.stack.(qx) in
      for y=qy to boardY-1 do
	cl.(0)<-elm qx y;
	cl.(1)<-elm qx (y+1);
	wipe_many_groups board 2 cl
      done

let handle_odd_before_even board tc =

  (*Rule one*)

  wipe_odd board (tc.cross);

  (*Rule two*)

  let cl = Array.make 2 0 
  and px = elx (tc.cross)
  and py = (ely (tc.cross)) + 1
  and qx = elx (tc.even)
  and qy = (ely (tc.even)) + 1 in
    for y1=qy to boardY-1 do
      for y2=py to boardY-1 do
	cl.(0)<-elm qx y1;
	cl.(1)<-elm px y2;
	wipe_many_groups board 2 cl
      done;
    done;

    (*Rule three*)

    if board.stack.(px) land 1 = 0 && board.stack.(qx)<qy-1 then
      (
	cl.(0)<-elm px (board.stack.(px));
	cl.(1)<-elm qx (board.stack.(qx));
	wipe_many_groups board 2 cl;
      );

    (*Rule four*)

    let qx = elx (tc.odd) and qy = board.stack.(qx) in
      for y=qy to boardY-1 do
	cl.(0)<-elm qx y;
	cl.(1)<-elm qx (y+1);
	wipe_many_groups board 2 cl
      done


(*Regles de la these*)
let claimeven board =
  let rec helper y1 =
    if y1<boardY then
      (
	for x1=0 to boardX-1 do
	  if board.sqused.(elm x1 y1) then
	    (
	      board.solution.(board.sp).solgroupsnumb <- 0;
	      let q1=elm x1 y1 and q2=elm x1 (y1-1) in
		if !(board.square.(q1)) = 0 && !(board.square.(q2)) = 0
		  && board.sqused.(q2) then
		    (
		      let sln = board.solvable_groups.sqpnt.(q1)
		      and grp = board.solvable_groups.squar.(q1) in
			for j=0 to sln-1 do
			  if board.intgp.tgroups.(grp.(j)) then
			    (
			      if board.solution.(board.sp).
				solgroupsnumb=0
			      then
				(
				  board.solution.(board.sp).
				    solname <- claimeven_;
				  board.solution.(board.sp).
				    solpoint.(0) <- q1;
				  board.solution.(board.sp).
				    solpoint.(1) <- q2;
				  board.solution.(board.sp).
				    sqinv.(0) <- q1;
				  board.solution.(board.sp).
				    sqinv.(1) <- q2;
				  board.solution.(board.sp).
				    sqinvnumb <- 2;
				  board.instances.(claimeven_)
				  <- board.instances.(claimeven_) + 1
				);
			      board.solution.(board.sp).solgroups.
				(board.solution.(board.sp).solgroupsnumb)
			      <- grp.(j);
			      board.solution.(board.sp).solgroupsnumb <-
				board.solution.(board.sp).solgroupsnumb+1
			    );
			done;
		    );
		if board.solution.(board.sp).solgroupsnumb > 0 then
		  board.sp <- board.sp + 1;
	    );     
	done;
	helper (y1+2)
      )
  in helper 1
       


let baseinverse board =
  let set = Array.make 64 true in
    for y1=0 to boardY-1 do
      for x1=0 to boardX-1 do
	let q1 = elm x1 y1 in
	  if board.sqused.(q1) then
	    if board.stack.(x1) = y1 then
	      (
		memset set true 64;
		let sln = board.solvable_groups.sqpnt.(q1)
		and grp = board.solvable_groups.squar.(q1) in
		  for j=0 to sln-1 do
		    if board.intgp.tgroups.(grp.(j)) then
		      for x=0 to tiles-1 do
			let wx = board.xplace.(grp.(j)).(x)
			and wy = board.yplace.(grp.(j)).(x) in
			  if x1<wx && board.stack.(wx)=wy &&
			    set.(elm wx wy) && board.sqused.(elm wx wy)
			  then (
			    set.(elm wx wy) <- false;
			    board.solution.(board.sp).solgroupsnumb <- 0;
			    board.solution.(board.sp).solname
			    <- baseinverse_;
			    board.solution.(board.sp).solpoint.(0) <- q1;
			    board.solution.(board.sp).solpoint.(1)
			    <- elm wx wy;
			    board.solution.(board.sp).sqinv.(0) <- q1;
			    board.solution.(board.sp).sqinv.(1)
			    <- elm wx wy;
			    board.solution.(board.sp).sqinvnumb <- 2;
			    both_groups board q1 (elm wx wy);
			    board.instances.(baseinverse_)
			    <- board.instances.(baseinverse_) + 1;
			    if board.solution.(board.sp).solgroupsnumb>0
			    then board.sp <- board.sp + 1;
			  )
		      done;
		  done;
	      )
      done;
    done



let vertical board = 
  let set = Array.make 64 true in
    for y1=0 to boardY-1 do
      for x1=0 to boardX-1 do
	let q1=elm x1 y1 in
	  if board.sqused.(q1) then
	    if !(board.square.(q1)) = 0 then
	      (
		memset set true 64;
		let sln=board.solvable_groups.sqpnt.(q1)
		and grp=board.solvable_groups.squar.(q1) in
		  for j=0 to sln-1 do
		    if board.intgp.tgroups.(grp.(j)) &&
		      board.xplace.(grp.(j)).(0)=
		      board.xplace.(grp.(j)).(3) then
			for x=0 to tiles-1 do
			  let wy=board.yplace.(grp.(j)).(x) in
			    if wy>0 && (wy land 1)=0 && y1=wy-1
			      && set.(elm x1 wy) 
			      && board.sqused.(elm x1 wy)
			    then (
			      set.(elm x1 wy)<-false;
			      board.solution.(board.sp).solgroupsnumb
			      <- 0;
			      board.solution.(board.sp).solname
			      <- vertical_;
			      board.solution.(board.sp).solpoint.(0)
			      <- q1;
			      board.solution.(board.sp).solpoint.(1)
			      <- elm x1 wy;
			      board.solution.(board.sp).sqinv.(0)
			      <- q1;
			      board.solution.(board.sp).sqinv.(1)
			      <- elm x1 wy;
			      board.solution.(board.sp).sqinvnumb
			      <- 2;
			      both_groups board q1 (elm x1 wy);
			      board.instances.(vertical_)
			      <- board.instances.(vertical_) + 1;
			      if board.solution.(board.sp).
				solgroupsnumb>0 then
				  board.sp <- board.sp+1
			    )
			done;
		  done
	      )
      done;
    done
      


let aftereven board =
  let cl = Array.make 4 0 in
  let rec helper y1 =
    if y1<boardY then
      (
	for x1=0 to boardX-1 do
	  let q1=elm x1 y1 in
	    if board.sqused.(q1) then
	      (
		let sln = board.solvable_groups.sqpnt.(q1)
		and grp = board.solvable_groups.squar.(q1) in
		  for j=0 to sln-1 do
		    if board.intgp.mygroups.(grp.(j)) &&
		      x1=board.xplace.(grp.(j)).(0) &&
		      x1<board.xplace.(grp.(j)).(3) then
			let after = ref true
			and cols = ref 0 
			and x = ref 0 in
			  memset cl 0xffff 4;
			  while !after && !x<tiles do
			    let px=board.xplace.(grp.(j)).(!x)
			    and py=board.yplace.(grp.(j)).(!x) in
			      if board.sqused.(elm px py) then
				after := false;
			      if !(board.square.(elm px py)) = 0 then
				if py land 1 = 1 && board.stack.(px)
				  <=py-1 then
				    (
				      cl.(!cols)<-elm px py;
				      cols := !cols+1;
				    )
				else after := false;
			      x := !x+1
			  done;
			  if !after && !cols>0 then
			    (
			      board.solution.(board.sp).solgroupsnumb<-0;
			      board.solution.(board.sp).solname
			      <-aftereven_;
			      board.solution.(board.sp).solpoint.(0)<-q1;
			      board.solution.(board.sp).solpoint.(1)
			      <-elm (board.xplace.(grp.(j)).(3))
				       (board.yplace.(grp.(j)).(3));
			      board.instances.(aftereven_)
			      <- board.instances.(aftereven_)+1;
			      board.solution.(board.sp).sqinvnumb
			      <- !cols;
			      for pj=0 to !cols-1 do
				board.solution.(board.sp).sqinv.(pj)
				  <-cl.(pj)
			      done;
			      solve_columns board !cols cl;
			      if board.solution.(board.sp).solgroupsnumb
				> 0 then board.sp <- board.sp+1
			    )
		  done
	      )
	done;
	helper (y1+2)
      )
  in helper 1



let lowinverse board =
  let set = Array.make 64 true in
  let rec helper y1 =
    if y1<boardY then
      (
	for x1=0 to boardX-1 do
	  let q1 = elm x1 y1 in
	    if board.sqused.(q1) then
	      if board.stack.(x1)<y1 then
		(
		  memset set true 64;
		  let sln = board.solvable_groups.sqpnt.(q1)
		  and grp = board.solvable_groups.squar.(q1) in
		    for j=0 to sln-1 do
		      if board.intgp.tgroups.(grp.(j)) &&
			board.xplace.(grp.(j)).(0) <>
			board.xplace.(grp.(j)).(3) then
			  for x=0 to tiles do
			    let wx = board.xplace.(grp.(j)).(x)
			    and wy = board.yplace.(grp.(j)).(x) in
			      if x1<wx && board.stack.(wx)<wy && wy>0 &&
				wy land 1 = 0 && set.(elm wx wy) &&
				board.sqused.(elm wx wy) then
				  (
				    set.(elm wx wy)<-false;
				    board.solution.(board.sp)
				      .solgroupsnumb <- 0;
				    board.solution.(board.sp).solname
				    <- lowinverse_;
				    board.solution.(board.sp)
				      .solpoint.(0) <- q1;
				    board.solution.(board.sp)
				      .solpoint.(1) <- elm wx wy;
				    board.solution.(board.sp).sqinv.(0)
				    <- q1;
				    board.solution.(board.sp).sqinv.(1)
				    <- elm wx wy;
				    board.solution.(board.sp).sqinv.(2)
				    <- elm x1 (y1-1);
				    board.solution.(board.sp).sqinv.(3)
				    <- elm wx (wy-1);
				    board.solution.(board.sp).sqinvnumb
				    <- 4;
				    both_groups board q1 (elm wx wy);
				    both_groups board q1 (elm x1 (y1-1));
				    both_groups board (elm wx wy)
				      (elm wx (wy-1));
				    board.instances.(lowinverse_)
				    <- board.instances.(lowinverse_)+1;
				    if board.solution.(board.sp)
				      .solgroupsnumb>0 then board.sp
					<- board.sp + 1;
				  )
			  done;
		    done
		)
	done;
	helper (y1+2)
      )
  in helper 2


let highinverse board =
  let set = Array.make 64 true in
  let rec helper y1 =
    if y1<boardY then
      (
	for x1=0 to boardX do
	  let q1 = elm x1 y1 in
	    if board.sqused.(q1) then
	      (
		memset set true 64;
		let sln = board.solvable_groups.sqpnt.(q1)
		and grp = board.solvable_groups.squar.(q1) in
		  for j=0 to sln-1 do
		    if board.intgp.tgroups.(grp.(j)) &&
		      board.xplace.(grp.(j)).(0)
		      <> board.yplace.(grp.(j)).(3) then
			for x=0 to tiles-1 do
			  let wx=board.xplace.(grp.(j)).(x)
			  and wy=board.yplace.(grp.(j)).(x) in
			    if x1<wx && board.stack.(wx)<wy
			      && wy>0 && wy land 1 = 0 &&
			      set.(elm wx wy) &&
			      board.sqused.(elm wx wy) then
				(
				  set.(elm wx wy)<-false;
				  board.solution.(board.sp)
				    .solgroupsnumb<-0;
				  board.solution.(board.sp).solname
				  <- highinverse_;
				  board.solution.(board.sp)
				    .solpoint.(0) <- q1;
				  board.solution.(board.sp)
				    .solpoint.(1) <- elm wx wy;
				  board.solution.(board.sp).sqinv.(0)
				  <- q1;
				  board.solution.(board.sp).sqinv.(1)
				  <- elm wx wy;
				  board.solution.(board.sp).sqinv.(2)
				  <- elm x1 (y1-1);
				  board.solution.(board.sp).sqinv.(3)
				  <- elm wx (wy-1);
				  board.solution.(board.sp).sqinv.(4)
				  <- elm x1 (y1+1);
				  board.solution.(board.sp).sqinv.(5)
				  <- elm wx (wy+1);
				  board.solution.(board.sp)
				    .sqinvnumb <- 6;
				  board.instances.(highinverse_)
				  <- board.instances.(highinverse_)+1;
				  
				  (*Upper and middle Squares*)
				  both_groups board (elm x1 (y1+1))
				    (elm wx (wy+1));
				  both_groups board q1 (elm wx wy);
				  
				  (*Vertical groups*)
				  both_groups board (elm x1 y1)
				    (elm x1 (y1+1));
				  both_groups board (elm wx wy)
				    (elm wx (wy+1));
				  
				  (*Lower 1st and Upper 2nd if lower
				    is playable*)
				  if board.stack.(x1)=y1-1 then
				    both_groups board (elm x1 (y1-1))
				      (elm  wx (wy+1));
				  
				  (*Upper 1st and Lower 2nd if lower
				    is playable*)
				  if board.stack.(wx)=wy-1 then
				    both_groups board (elm x1 (y1+1))
				      (elm wx (wy-1));
				  
				  if board.solution.(board.sp)
				    .solgroupsnumb > 0 then
				      board.sp <- board.sp + 1
				)
			done;
		  done
	      )
	done;
	helper (y1+2)
      )
  in helper 2


let baseclaim board =
  let set = Array.make 64 true 
  and cl = Array.make 6 0
  and cols = ref 0 in
    for y1=0 to boardY-1 do
      for x1=0 to boardX-1 do
	if board.sqused.(elm x1 y1) then
	  (
	    let q1 = elm x1 y1 in
	    let sln = board.solvable_groups.sqpnt.(q1)
	    and grp = board.solvable_groups.squar.(q1) in
	      for j=0 to sln-1 do
		if board.intgp.tgroups.(grp.(j)) &&
		  board.xplace.(grp.(j)).(0)<>board.yplace.(grp.(j)).(3)
		then
		  (
		    cols := 0;
		    for x=0 to tiles-1 do
		      let wx = board.xplace.(grp.(j)).(x)
		      and wy = board.yplace.(grp.(j)).(x) in
			if board.sqused.(elm wx wy) then
			  (
			    if board.stack.(wx) = wy then
			      (
				cl.(!cols) <- elm wx wy;
				cols := !cols + 1
			      );
			    if !cols = 3 then
			      (
				cl.(3) <- cl.(1);
				cl.(4) <- cl.(0);
				if set.(cl.(0)) then
				  check_claim board (int_to_tab cl.(0));
				set.(cl.(0)) <- false
			      )
			  )
		    done
		  )
	      done
	  )
      done;
    done


let before board =
  let cl = Array.make tiles 0 
  and cols = ref 0 and befo = ref true in
    for j=0 to groups-1 do
      if board.intgp.mygroups.(j) && board.xplace.(j).(0)
	<> board.xplace.(j).(3) && board.yplace.(j).(0) <> boardY-1
	&& board.yplace.(j).(3) <> boardY-1 then
	  (
	    cols := 0;
	    befo := true;
	    let x = ref 0 in
	    while !x < tiles && !befo do
	      let px = board.xplace.(j).(!x)
	      and py = board.yplace.(j).(!x) in
		if board.sqused.(elm px py) then befo := false
		else if !(board.square.(elm px py)) = 0 then
		  (
		    cl.(!cols) <- elm px (py+1)
		  );
		x:= !x+1
	    done;
	    if !befo && !cols>0 then
	      for i=0 to !cols-1 do
		board.solution.(board.sp).sqinv.(i) <- cl.(i)
	      done;
	    generate_all_other_before_instances board !cols cl j
	  )
    done


let threat_group board group who =
  let p = !(board.groups.(group).(0)) lor !(board.groups.(group).(1)) lor
    !(board.groups.(group).(2)) lor !(board.groups.(group).(3)) in
    (p land who) land 1 <> 1



let check_early_win board =
  !(board.square.(sq_d1)) = 1 && !(board.square.(sq_c3)) = 1 &&
  !(board.square.(sq_e3)) = 1 &&
  (!(board.square.(sq_c2)) = 1 || !(board.square.(sq_e2)) = 1) &&
  !(board.square.(sq_b2)) = 0 && !(board.square.(sq_d2)) = 0 &&
  !(board.square.(sq_f2)) = 0


let anypentas board = 
  let a = ref 0 in
    if !(board.square.(sq_c1)) <> 1 && !(board.square.(sq_e1)) <> 1 then 0
    else 
      (
	if !(board.square.(sq_b3)) = 1 && !(board.square.(sq_d3)) = 1 &&
	  (!(board.square.(sq_b2)) = 1 || !(board.square.(sq_d2)) = 1) &&
	  !(board.square.(sq_a2)) = 0 && !(board.square.(sq_c2)) = 0 &&
	  !(board.square.(sq_e2)) = 0 then a := !a lor 2;
	if !(board.square.(sq_d3)) = 1 && !(board.square.(sq_f3)) = 1 &&
	  (!(board.square.(sq_d2)) = 1 || !(board.square.(sq_f2)) = 1) &&
	  !(board.square.(sq_c2)) = 0 && !(board.square.(sq_e2)) = 0 &&
	  !(board.square.(sq_g2)) = 0 then a:= !a lor 1;
	!a
      )







(*Adjacent Matrix*)

let rulecombo = [|[|1;1;1;1;3;3;1;1;1|];
		     [|1;1;1;1;1;1;1;1;1|];
		     [|1;1;1;1;1;1;1;1;1|];
		     [|1;1;1;4;3;3;1;4;12|];
		     [|3;1;1;3;8;8;3;6;6|];
		     [|3;1;1;3;8;8;3;3;3|];
		     [|1;1;1;1;3;3;1;1;1|];
		     [|1;1;1;4;6;3;1;4;4|];
		     [|1;1;1;12;6;3;1;4;4|]|]

exception Combinaison_error


let overlap board p1 p2 =
  let temp = Array.make ((boardX+1)*(boardY+2)) false
  and bol = ref false in
    for x=0 to board.solution.(p2).sqinvnumb - 1 do
      temp.(board.solution.(p2).sqinv.(x)) <- true
    done;
    for x=0 to board.solution.(p1).sqinvnumb - 1 do
      if temp.(board.solution.(p1).sqinv.(x)) then bol := true ;
    done;
    !bol


let claimeven_below board p1 p2 =
  let name = board.solution.(p1).solname
  and bol = ref true in
  let (q1,q2) = if name <> highinverse_ && name <> lowinverse_ then (p2,p1)
  else (p1,p2) in
    if board.solution.(q2).solname = aftereven_ then
      let solcheck = board.solution.(q2).sqinvnumb / 2 in
	for x=0 to 1 do
	  let q1x = elx board.solution.(q1).sqinv.(x+2)
	  and q1y = ely board.solution.(q1).sqinv.(x+2) in
	    for y=0 to solcheck-1 do
	      let q2x = elx board.solution.(q2).sqinv.(solcheck+y)
	      and q2y = ely board.solution.(q2).sqinv.(solcheck+y) in
		if q1x=q2x && q1y>q2y && (q2y land 1 = 1) then bol:=true
	    done
	done;
    else if board.solution.(q2).solname = before_ &&
      board.solution.(q2).solname = specialbefore_ then
	let solcheck = board.solution.(q2).sqinvnumb/2 in
	  for x=0 to 1 do
	    let q1x = elx board.solution.(q1).sqinv.(x+2)
	    and q1y = ely board.solution.(q1).sqinv.(x+2) in
	      for y=0 to solcheck do
		let q2x = elx board.solution.(q2).sqinv.(1+(y lsl 1))
		and q2y = ely board.solution.(q2).sqinv.(1+(y lsl 1)) in
		  if q1x=q2x && q1y>q2y && (q2y land 1=1) then bol:=true
	      done
	  done;
    else if board.solution.(q2).solname = claimeven_ then
      for x=0 to 1 do
	let q1x = elx board.solution.(q1).sqinv.(x+2)
	and q1y = elx board.solution.(q1).sqinv.(x+2)
	and q2x = elx board.solution.(q2).sqinv.(0)
	and q2y = ely board.solution.(q2).sqinv.(0) in
	  if (q1x=q2x &&q1y>q2y) then bol := true
      done
    else if board.solution.(q2).solname = baseclaim_ then
      for x=0 to 1 do
	let q1x = elx board.solution.(q1).sqinv.(x+2)
	and q1y = elx board.solution.(q1).sqinv.(x+2)
	and q2x = elx board.solution.(q2).sqinv.(0)
	and q2y = ely board.solution.(q2).sqinv.(0) in
	  if (q1x=q2x &&q1y>q2y) then bol := true
      done
    else raise Combinaison_error


let column_wdoe board p1 p2 =
  let joinmtrx = Array.make ((boardX+1)*(boardY+2)) false in
    for x=0 to board.solution.(p1).sqinvnumb - 1 do
      joinmtrx.(board.solution.(p1).sqinv.(x)) <- true
    done;
    for x=0 to board.solution.(p2).sqinvnumb - 1 do
      joinmtrx.(board.solution.(p2).sqinv.(x)) <- true
    done;
    let x = ref 0 and answer = ref true and cnt = ref 0 in
      while !x< boardX && !answer do
	for y = 0 to boardY-1 do
	if joinmtrx.(elm !x y) then cnt := !cnt + 1
	done;
	if !cnt land 1 = 1 then answer := false
      done;
      !answer
    


let comp_rules board p1 p2 =
  let c1 = board.solution.(p1).solname - 1
  and c2 = board.solution.(p2).solname - 1 
  and bol = ref true in
  let way = rulecombo.(c1).(c2) in
    if way land 9 <> 0 then
      (
	board.rules.(0) <- board.rules.(0) + 1;
	if overlap board p1 p2 then bol := false
      )
    else if way land 2 <> 0 then
      (
	board.rules.(1) <- board.rules.(1) + 1;
	if overlap board p1 p2 then bol := false
      )
    else if way land 4 <> 0 then
      (
	board.rules.(2) <- board.rules.(2) + 1;
	if not (column_wdoe board p1 p2) then bol := false
      );
    !bol
	


let build_adjacency_matrix board =
  let matrix = Array.init board.sp
    (fun i -> Array.init board.sp (fun j -> false)) in
    for x=0 to board.sp-1 do
      for y=x to board.sp-1 do
	if comp_rules board x y then matrix.(y).(x) <- true
      done
    done;
    matrix


(*Problem Solver*)

let wside = [|"none";"yellow";"red"|]
and rules_name = [|"CLAIMEVEN";"BASEINVERSE";"VERTICAL";"LOWINVERSE";"HIGHINVERSE";"BASECLAIM";"BEFORE";"SPECIALBEFORE"|]
and tempsolused = ref 0
exception No_problem_found

type problem = {
  mutable group : int;
  mutable solved : bool;
  solutions : int array;
  mutable solnumb : int
}

type problem_list = {
  mutable number : int;
  problem : problem array;
  pointer : int array;
  final : int array
}

type up_solution = {
  mutable howmany : int;
  mutable which : int array;
  mutable hmprobs : int;
  mutable wprobs : int array
}

let make_problem group_ solved_ solnumb_ = 
  {
    group = group_;
    solved = solved_;
    solutions = Array.make 621 0;
    solnumb = solnumb_
  }

    
  
let find_most_difficult_problem pblist board =
  let minsol = ref 32767
  and solpnt = ref (-1) in
  for i=0 to pblist.number-1 do
    if (not pblist.problem.(1).solved) then
      (
	let k = ref 0 in
	  for j=0 to pblist.problem.(i).solnumb do
	    let m = pblist.problem.(i).solutions.(j) in
	      if board.solution.(m).valid then k:=!k+1
	  done;
	  if !k < !minsol then
	    (
	      minsol := !k;
	      solpnt := i;
	    )
      )
  done;
  (!minsol, !solpnt)



let build_problem_list board =
  let j = ref 0
  and k = ref 0
  and pblist = {
    number = 0;
    problem = Array.make groups (make_problem 0 false 0);
    pointer = Array.make groups 0;
    final = Array.make groups 0
  } in
    for i=0 to groups-1 do
      pblist.final.(i) <- -1;
      if board.intgp.tgroups.(i) then
	  (
	    pblist.problem.(!j) <- make_problem i false 0;
	    pblist.pointer.(i) <- !j;
	    pblist.problem.(!j).group <- i;
	    pblist.problem.(!j).solnumb <- 0;
	    pblist.problem.(!j).solved <- false;
	    j := !j+1;
	    pblist.number <- !j
	  )
      else pblist.pointer.(i) <- -1
    done;
    
    for x=0 to board.sp-1 do
      board.solution.(x).valid <- true;
      for y=0 to board.solution.(x).solgroupsnumb-1 do
	k := board.solution.(x).solgroups.(y);
	j := pblist.pointer.(!k);
	pblist.problem.(!j).solutions.(pblist.problem.(!j).solnumb)<-x;
	pblist.problem.(!j).solnumb <- pblist.problem.(!j).solnumb + 1
      done;
    done;
    pblist


let remove_solutions pblist board matrix psol = 
  let temp = Array.make maxsols 0
  and tprobs = Array.make groups 0 
  and probs = ref 0 
  and tsol = ref 0 in
  let update = {
    howmany = 0;
    which = temp;
    wprobs = tprobs;
    hmprobs = !probs }in
    for y=0 to board.solution.(psol).solgroupsnumb-1 do
      let z = board.solution.(psol).solgroups.(y) in
      let j = pblist.pointer.(z) in
	if j = -1 then raise No_problem_found;
	if not pblist.problem.(j).solved then
	  (
	    pblist.problem.(j).solved <- true;
	    tprobs.(!probs) <- j;
	    probs := !probs + 1;
	    pblist.final.(j) <- psol
	  )
    done;
    for x=0 to board.sp-1 do
      let ps = if x > psol then matrix.(x).(psol) else matrix.(psol).(x) in
	if not ps && board.solution.(x).valid then
	  (
	    board.solution.(x).valid <- false;
	    temp.(!tsol) <- x;
	    tsol := !tsol + 1
	  )
    done;
    update.howmany <- !tsol;
    if !tsol > 0 then update.which <- Array.copy temp;
    update.hmprobs <- !probs;
    if !probs > 0 then update.wprobs <- Array.copy tprobs;
    update
   


let restore_solutions update pblist board =
  if update.hmprobs > 0 then
    for x=0 to update.hmprobs-1 do
      pblist.problem.(update.wprobs.(x)).solved <- false
    done;
  if update.howmany > 0 then
    for x=0 to update.howmany-1 do
      board.solution.(update.which.(x)).valid <- true
    done

let rec solve_problem_list pblist board matrix =
  let (sols, mdp) = find_most_difficult_problem pblist board in
    if mdp = -1 then true
    else if sols = 0 then false
    else
      (
	tempsolused := !tempsolused + 1;
	if board.solused < !tempsolused then board.solused <- !tempsolused;
	let answer = ref false and x = ref 0 in
	  while !x<pblist.problem.(mdp).solnumb && (not !answer) do
	    let j = pblist.problem.(mdp).solutions.(!x) in
	      if board.solution.(j).valid then
		(
		  pblist.problem.(mdp).solved <- true;
		  pblist.final.(mdp) <- j;
		  let update = remove_solutions pblist board matrix j in
		    answer := solve_problem_list pblist board matrix;
		    restore_solutions update pblist board;
		    if (not !answer) then pblist.final.(mdp) <- -1;
		    pblist.problem.(mdp).solved <- false
		);
	      x := !x + 1
	  done;
	  tempsolused := !tempsolused - 1;
	  !answer
      )


let problem_solver board matrix =
  board.problem_solved <- 0;
  board.solused <- 0;
  tempsolused := 0;
  let pblist = build_problem_list board in
    solve_problem_list pblist board matrix







(*Vraie evaluation*)

let evaluate_black board =
  board.sp <- 0;
  board.intgp.j <- 0;
  board.intgp.k <- 0;
  board.sqused <- Array.make ((boardX+1) * (boardY+2)) true;
  for i = 0 to groups do
    if threat_group board i 2 then
      (
	board.intgp.tgroups.(i)<-true;
	board.intgp.j <- board.intgp.j + 1
      )
    else board.intgp.tgroups.(i) <- false;
    if threat_group board i 1 then
      (
	board.intgp.mygroups.(i) <- true;
	board.intgp.k <- board.intgp.k + 1
      )
    else board.intgp.mygroups.(i) <- false
  done;

  claimeven board;
  baseinverse board;
  vertical board;
  aftereven board;
  lowinverse board;
  highinverse board;
  baseclaim board;
  before board;

  if board.intgp.j = 0 then true
  else if board.sp = 0 then false
  else
    (
      let matrix = build_adjacency_matrix board in
      let oracle = problem_solver board matrix in
	oracle
    )


let evaluate_white board =
  let threats = Array.make maxgroups 0
  and threat = {
    cross = 0;
    even = 0;
    odd = 0;
    gp1 = 0;
    gp2 = 0
    
  } 
  and anythreat = ref 0 in
  let tc = Array.make groups threat in
  let combo = threat_combo board tc in
  let thnum = count_odd_threats board threats 
  and oracle = ref false in
    if thnum + combo = 0 then oracle := false
    else 
      while !anythreat < thnum + combo && not !oracle do
	board.usablegroup <- Array.make groups true;
	board.sqused <- Array.make ((boardX+1)*(boardY+2)) true;
	board.wipesq <- Array.make ((boardX+1)*(boardY+2)) 0;
	if !anythreat < thnum then
	  (
	    wipe_above board threats.(!anythreat);
	    wipe_odd board threats.(!anythreat);
	    let px = elx threats.(!anythreat) in
	      for py=0 to boardY-1 do
		if !(board.square.(elm px py)) = 0
		then board.sqused.(elm px py) <- false
	       done
	  )
	else
	  (
	    let ccmb = !anythreat in
	    let px = elx tc.(ccmb).cross
	    and x = elx tc.(ccmb).odd in
	      for y=0 to boardY-1 do
		if board.sqused.(elm px y) then
		  board.sqused.(elm px y) <- false;
		if board.sqused.(elm x y) then
		  board.sqused.(elm x y) <- false;
	      done
	  );
	board.sp <- 0;
	board.intgp.j <- 0;
	board.intgp.k <- 0;
	for i=0 to groups-1 do
	  if threat_group board i i && not (wiped_group board i) &&
	    board.usablegroup.(i) then
	      (
		board.intgp.tgroups.(i) <- true;
		board.intgp.j <- board.intgp.j + 1
	      )
	  else board.intgp.tgroups.(i) <- false;
	  if threat_group board i 2 then
	    (
	      board.intgp.mygroups.(i) <- true;
	      board.intgp.k <- board.intgp.k + 1
	    )
	  else board.intgp.mygroups.(i) <- false;
	  for y = 0 to boardY-1 do
	    for x=0 to boardX-1 do
	      claimeven board;
	      baseinverse board;
	      vertical board;
	      aftereven board;
	      lowinverse board;
	      highinverse board;
	      baseclaim board;
	      before board;
	      if board.intgp.j = 0 then oracle := true
	      else if board.sp = 0 then oracle := false
	      else 
		let matrix = build_adjacency_matrix board in
		  oracle := problem_solver board matrix
	    done
	  done
	done;
      done;
    !oracle
      


let evaluation_function board =
  if board.turn = 1 then evaluate_black board
  else evaluate_white board


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


(*IA*)

let blstrsize = 14
let goodmove = 16384
let badmove = -16384
let switch a = a lxor 3
let and_type = 1
let or_type = 1

let get_black_best_move board =
  let black_str = [|4;4;4;4;4;2;2;2;2;6;6;6;6;6|] in
    if board.filled >= blstrsize then -1
    else
      let rec helper x =
	if x<board.filled then
	  if board.moves.(x) <> (black_str.(x) - 1) then false
	  else helper (x+1)
	else true
      in if (helper 0) then (black_str.(board.filled) - 1)
	else -1


let reverse_board board =
  for y=0 to boardY-1 do
    for x=0 to boardX/2 - 1 do
      let z = !(board.square.(elm x y)) in
	board.square.(elm x y) := !(board.square.(elm (boardX-x-1) y));
	board.square.(elm (boardX-x-1) y) := z
    done
  done;
  
  for x=0 to boardX/2 -1 do
    let z = board.stack.(x) in
      board.stack.(x) <- board.stack.(boardX-x-1);
      board.stack.(boardX-x-1) <- z
  done


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


let check_double board group pos side =
  let rec helper x =
    if x<groups then
    let y = gen_odd_threat board x side in
      if y=pos then true
      else helper (x+1)
    else false
  in helper (group+1)
       

let group_eval board =
  let t1 = board.turn in
  let t2 = switch t1
  and score = ref 0 in
    for x = 0 to groups-1 do
      let p1 = ref 0
      and p2 = ref 0 in
	for i = 0 to 3 do
	  if !(board.groups.(x).(i)) = t1 then p1 := !p1+1
	  else if !(board.groups.(x).(i)) = t2 then p2 := !p2+1
	done;
	if !p1 = 4 then score := !score + goodmove
	else if !p2 = 4 then score := !score + badmove
	else if !p1 = 3 && !p2 = 0 then
	  (
	    score := !score + 100;
	    let z = gen_odd_threat board x t1 in
	      if z <> -1 then
		let f = check_double board x z t1 in
		  if not f then
		    if t1 = 1 then score := !score + 200
		    else score := !score + 150
		  else if t1 = 1 then score := !score +750
		  else score := !score + 500
	  )
	else if !p2 = 3 && !p1 = 0 then
	  (
	    score := !score - 100;
	    let z = gen_odd_threat board x t2 in
	      if z <> -1 then
		let f = check_double board x z t2 in
		  if not f then
		    if t1 = 2 then score := !score - 200
		    else score := !score - 150
		  else if t1 = 2 then score := !score - 750
		  else score := !score - 500
	  )
	else if !p1 = 2 && !p2 = 0 then score := !score +10
	else if !p2 = 2 && !p1 = 0 then score := !score - 10;
	
	if check_pentas board 1 then
	  if t1 = 1 then score := !score + 800
	  else score := !score - 800
    done;
    !score

let groupeval board =
  let t1 = board.turn in
  let t2 = switch t1
  and score = ref 0 in
    for x = 0 to groups-1 do
      let p1 = ref 0
      and p2 = ref 0 in
	for i = 0 to 3 do
	  if !(board.groups.(x).(i)) = t1 then p1 := !p1+1
	  else if !(board.groups.(x).(i)) = t2 then p2 := !p2+1
	done;
	if !p1 = 4 then score := !score + goodmove
	else if !p2 = 4 then score := !score + badmove
	else if !p1 = 3 && !p2 = 0 then
	  (
	    score := !score + 1;
	    let z = gen_odd_threat board x t1 in
	      if z <> -1 then
		let f = check_double board x z t1 in
		  if not f then
		    if t1 = 1 then score := !score + 200
		    else score := !score + 150
		  else if t1 = 1 then score := !score +750
		  else score := !score + 500
	  )
	else if !p2 = 3 && !p1 = 0 then
	  (
	    score := !score - 1;
	    let z = gen_odd_threat board x t2 in
	      if z <> -1 then
		let f = check_double board x z t2 in
		  if not f then
		    if t1 = 2 then score := !score - 200
		    else score := !score - 150
		  else if t1 = 2 then score := !score - 750
		  else score := !score - 500
	  )
	else if !p1 = 2 && !p2 = 0 then score := !score +10
	else if !p2 = 2 && !p1 = 0 then score := !score - 10;
	
	if check_pentas board 1 then
	  if t1 = 1 then score := !score + 800
	  else score := !score - 800
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


(* let bb = make_board() in *)
(*   initboard bb; *)
(*   let _ = makemove bb 0 *)
(*   and _ = makemove bb 1 *)
(*   and _ = makemove bb 0 *)
(*   and _ = makemove bb 1 *)
(*   and _ = makemove bb 0 in *)
(*   groupeval bb *)





let fast_try_to_win board =
  let rec win x =
    if x<boardX then
      if board.stack.(x)<boardY && connected board x >= 4 then x
      else win (x+1)
    else -1 in
    win 0


(*Binary Tree*)
let proved = 1
and disproved = -1
and unknown = 0
type info =
    {
      mutable max_tree_depth : int;
      mutable bestmove : int
    }
      
type node =
    {
      squaree : int array;
      stac : int array;
      mutable tur : int;
      mutable evaluated : bool;
      mutable expanded : bool;
      mutable value : int;
      mutable typed : int;
      mutable proof : int;
      mutable disproof : int;
      child : node option array;
      parents : node option array
    }

type bintree =
    {
      mutable parent : bintree option;
      mutable lson : bintree option;
      mutable rson : bintree option;
      mutable node : node
    }

let rootnode =
    ref {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = 0;
      stac = Array.init (boardX+1) (fun i -> 0);
      evaluated = false;
      expanded = false;
      value = 0;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.init boardX (fun i -> None);
      parents = Array.init boardX (fun i -> None)
    }

let her_node_expanded = ref 0
      
let fast_init_bin_tree node =
  let root =
    {
      parent = None;
      lson = None;
      rson = None;
      node = node
    }
  in root

let fast_set_node root node =
  let child =
    {
      parent = Some root;
      lson = None;
      rson = None;
      node = node
    }
  in child
	  

let bin_compare c1 c2 =
  let rec helper x =
    if x<6 then
      if c1.(x) > c2.(x) then -1
      else if c1.(x) < c2.(x) then 1
      else helper (x+1)
    else 0 in
    helper 0



let her_generate_all_children node =
  for x=0 to boardX-1 do
    if node.stac.(x) < boardY then
      (
	node.child.(x) <-
	  Some 
	  {
	    squaree = Array.copy node.squaree;
	    stac = Array.copy node.stac;
	    tur = switch node.tur;
	    child = Array.init boardX (fun i -> None);
	    parents = Array.init boardX (fun i -> None);
	    evaluated = false;
	    expanded = false;
	    value = 0;
	    typed = or_type;
	    proof = 0;
	    disproof = 0;
	  };
	match node.child.(x) with
	  |None -> ()
	  |Some no ->
	     (
	       no.squaree.(elm x no.stac.(x)) <- node.tur;
	       no.stac.(x) <- no.stac.(x) + 1;
	       let symm =
		 {
		   squaree = Array.copy node.squaree;
		   stac = Array.copy node.stac;
		   tur = switch node.tur;
		   child = Array.init boardX (fun i -> None);
		   parents = Array.init boardX (fun i -> None);
		   evaluated = false;
		   expanded = false;
		   value = 0;
		   typed = or_type;
		   proof = 0;
		   disproof = 0;
		 } in
		 for y1=0 to boardY-1 do
		   symm.squaree.(elm boardX y1) <- no.squaree.(elm boardX y1);
		   for x1 = 0 to boardX-1 do
		     symm.squaree.(elm x1 y1)
		     <- no.squaree.(elm (boardX-x1-1) y1)
		   done
		 done;
	     )
      )
    else node.child.(x) <- None
  done
    
    
let rec fast_check_node root node =
  match root with
    |Some tree ->
       (
	 let cmp = bin_compare tree.node.squaree node.squaree in
	   if cmp<0 then
	     match tree.lson with
	       |None -> (tree.lson <- Some (fast_set_node tree node); None)
	       |Some lson -> fast_check_node (Some lson) node;
	   else if cmp>0 then
	     match tree.rson with
	       |None -> (tree.rson <- Some (fast_set_node tree node); None)
	       |Some rson -> fast_check_node (Some rson) node;
	   else Some (tree.node)
       )
    |None -> let _ = fast_init_bin_tree node in (Some node)
	 

let rec explore_tree board side depth =
  let cn = ref 0
  and tmp = if board.turn = side then
    group_eval board
  else (-1) * (group_eval board) in
  let answ = ref 0 in
    if depth = 0 then tmp
    else
      (
	if board.turn <> side then
	  (
	    answ := goodmove-depth;
	    let x = ref 0 in
	      while !x<boardX && !answ > badmove do
		if board.stack.(!x) < boardY then
		  (
		    cn := !cn+1;
		    if connected board !x >=4 then answ := badmove
		    else
		      (
			let _ = makemove board !x
			and tmp = explore_tree board side (depth-1) in
			  answ := min !answ tmp;
			  let _ = undomove board !x in ()
		      )
		  )
	      done;
	      if !cn = 0 then answ := 0
	  )
	else
	  (
	    answ := badmove + depth;
	    let x = ref 0 in
	      while !x<boardX do
		if board.stack.(!x) < boardY then
		  (
		    cn := !cn + 1;
		    if connected board !x >= 4 then answ := goodmove
		    else
		      (
			let _ = makemove board !x
			and tmp = explore_tree board side (depth-1) in
			  answ := max !answ tmp;
			  let _ = undomove board !x in ()
		      )
		  )
	      done;
	      if !cn = 0 then answ := 0
	  );
	!answ
      )


let her_evaluate node =
  node.evaluated <- true;
  let auxboard = make_board() in
    for x=0 to 63 do
      auxboard.square.(x) := node.squaree.(x)
    done;
    auxboard.turn <- node.tur;
    for x=0 to boardX+1 do
      auxboard.stack.(x) <- node.stac.(x);
      if (x<boardX) then auxboard.filled <- auxboard.filled + node.stac.(x)
    done;
    if auxboard.filled = maxmen then
      (
	if !rootnode.tur = 1 then
	  if !rootnode.typed = or_type then node.value <- proved
	  else node.value <- disproved
	else if !rootnode.typed = or_type then node.value <- disproved
	else node.value <- proved;
	-1
      )
    else
      let bestmove = fast_try_to_win auxboard in
	if bestmove <> -1 then
	  if node.typed = or_type then node.value <- proved
	  else node.value <- disproved
	else node.value <- unknown;
	bestmove
      
	
let her_set_pdv_according_to_children node =
  let children = ref 0 in
    for x=0 to boardX-1 do
      if node.stac.(x)<boardY then children := !children + 1
    done;
    if node.typed = and_type then
      (
	node.proof <- !children;
	node.disproof <- 1
      )
    else
      (
	node.proof <- 1;
	node.disproof <- !children
      )


let her_set_proof_and_disproof_numbers node =
  if node.expanded then
    if node.typed = and_type then
      (
	node.proof <- 0;
	node.disproof <- 1000000000;
	for x=0 to boardX-1 do
	  match node.child.(x) with
	    |Some no -> (node.proof <- node.proof + no.proof;
			 node.disproof <- min node.disproof no.disproof)
	    |None -> ()
	done;
	if node.disproof = 0 then node.proof <- 1000000000
      )
    else
      (
	node.proof <- 1000000000;
	node.disproof <- 0;
	for x=0 to boardX-1 do
	  match node.child.(x) with
	    |Some no -> (node.proof <- min node.proof no.proof;
			 node.disproof <- node.disproof + no.disproof)
	    |None -> ()
	done;
	if node.proof = 0 then node.disproof <- 1000000000
      )
  else if node.evaluated then
    if node.value = proved then
      (
	node.proof <- 0;
	node.disproof <- 1000000000;
      )
    else if node.value = disproved then
      (
	node.proof <- 1000000000;
	node.disproof <- 0
      )
    else her_set_pdv_according_to_children node
	


let her_ressources_available maxnodes =
  if her_node_expanded>maxnodes then false
  else true


let her_pn_search root maxnodes info =
  info.max_tree_depth <- 1;
  info.bestmove <- her_evaluate root;
  her_set_proof_and_disproof_numbers root;
  while root.proof <> 0 && root.disproof <> 0 &&
    her_ressources_available maxnodes do
      let her_most_proving_node = her_select_most_proving_node root info in
	her_develop_node her_most_proving_node;
	her_update_ancestrors her_most_proving_node
  done;
  root.value <-
  if root.proof = 0 then proved
  else if root.disproof = 0 then disproved
  else unknown
	


let heuristic_play_best board maxnodenum =
  let rootnode_ =
    {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = 0;
      stac = Array.init (boardX+1) (fun i -> 0);
      evaluated = false;
      expanded = false;
      value = unknown;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.init boardX (fun i -> None);
      parents = Array.init boardX (fun i -> None)
    }
  and symmetric =
    {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = board.turn;
      stac = Array.init (boardX+1)
	(fun x -> board.stack.(boardX-1-x) land 0xff);
      evaluated = false;
      expanded = false;
      value = unknown;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.init boardX (fun i -> None);
      parents = Array.init boardX (fun i -> None)
    } in
    for y=0 to boardY-1 do
      rootnode_.squaree.(elm boardX y) <-
	!(board.square.(elm boardX y)) land 0xff;
      symmetric.squaree.(elm boardX y) <-
	!(board.square.(elm boardX y)) land 0xff;
      for x = 0 to boardX-1 do
	rootnode_.squaree.(elm x y) <-
	  !(board.square.(elm x y)) land 0xff;
	symmetric.squaree.(elm x y) <-
	  !(board.square.(elm (boardX-1-x) y)) land 0xff
      done;
    done;

    rootnode_.stac.(boardX) <- board.stack.(boardX) land 0xff;
    symmetric.stac.(boardX) <- board.stack.(boardX) land 0xff;
    
    rootnode := if bin_compare symmetric.squaree rootnode_.squaree > 0
    then symmetric else rootnode_;
    let issymm = if bin_compare symmetric.squaree rootnode_.squaree > 0
    then true else false in
      her_pn_search rootnode maxnodenum info;
      let mymove =
	if rootnode.value = unknown then -1
	else if rootnode.value = disproved then -2
	else if issymm then boardX-1-info.bestmove
	else info.bestmove in
	mymove
    
