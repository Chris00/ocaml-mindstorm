open Utils

type t = {
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
  mutable intgp : intgp;
  mutable solution : solution array;
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
  mutable solvable_groups : solvable_groups;
  mutable white_book : int array array;
}

let copy b bb =
bb.square <- Array.copy b.square;
bb.wipesq <- Array.copy b.wipesq;
bb.usablegroup <- Array.copy b.usablegroup;
bb.sqused <- Array.copy b.sqused;
bb.stack <- Array.copy b.stack;
bb.groups <- Array.copy b.groups;
bb.xplace <- Array.copy b.xplace;
bb.yplace <- Array.copy b.yplace;
bb.turn <- b.turn;
bb.moves <- Array.copy b.moves;
bb.choices <- Array.copy b.choices;
bb.mlist <- Array.copy b.mlist;
bb.filled <- b.filled;
bb.intgp <- b.intgp;
bb.solution <- Array.copy b.solution;
bb.sp <- b.sp;
bb.problem_solved <- b.problem_solved;
bb.solused <- b.solused;
bb.oracle <- Array.copy b.oracle;
bb.oracle_guesses <- b.oracle_guesses;
bb.lastguess <- b.lastguess;
bb.nodes_visited <- b.nodes_visited;
bb.maxtreedepth <- b.maxtreedepth;
bb.rules <- Array.copy b.rules;
bb.instances <- Array.copy b.instances;
bb.solvable_groups <- b.solvable_groups;
bb.white_book <- Array.copy b.white_book


let make() =
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
    } in
  let board =
    {
      lastguess = 0;
      rules = Array.init 3 (fun i -> 0);
      oracle_guesses = 0;
      instances = Array.init 10 (fun i -> 0);
      turn = 1;
      filled = 0;
      groups = Array.init 69 (fun i -> (Array.init 4 (fun j -> ref 0)));
      xplace = Array.init 69 (fun i -> (Array.init 4 (fun j -> 0)));
      yplace = Array.init 69 (fun i -> (Array.init 4 (fun j -> 0)));
      square = Array.init ((boardX+1)*(boardY+2)) (fun i -> ref 0);
      wipesq = Array.init ((boardX+1)*(boardY+2)) (fun i -> 0);
      usablegroup = Array.init groups (fun i -> true);
      sqused = Array.init ((boardX+1)*(boardY+2)) (fun i -> false);
      stack = Array.init (boardX+1) (fun i -> 0);
      moves = Array.init maxmen (fun i -> -1);
      solvable_groups = solv;
      choices = Array.init maxmen (fun i -> 0);
      mlist = Array.init maxmen (fun i -> -1);
      intgp = intg;
      solution = Array.init alloc_solutions
	(fun i -> {
	   valid = true;
	   solname = -1;
	   solpoint = Array.init 2 (fun i -> 0);
	   sqinv = Array.init (2*tiles) (fun i -> 0);
	   sqinvnumb = 0;
	   solgroups = Array.init groups (fun i -> 0);
	   solgroupsnumb = 0
	 }
	);
      sp = 0;
      problem_solved = 0;
      solused = -1;
      oracle = Array.init 2 (fun i -> false);
      nodes_visited = 0;
      maxtreedepth = 0;
      white_book = Array.init 60499 (fun i -> Array.init 14 (fun j -> 0))
    } in board

let init board =
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
	      board.solvable_groups.squar.(p).(board.solvable_groups.sqpnt.(p))
	      <-i;
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

let build_book board =
  let i = ref 0
  and j = ref 0
  and line = ref ""
  and ic = open_in "white_book" in
  try
    while true do
      let a = input_char ic in
      if a = ' ' then (
        board.white_book.(!i).(!j) <- (int_of_string !line);
        line := "";
        j := !j+1;
        )
      else if a = '\n' then (
        board.white_book.(!i).(!j) <- (int_of_string !line);
        line := "";
        j := 0;
        i := !i+1
        )
      else line := String.concat "" [!line;(String.make 1 a)];
    done   
  with End_of_file -> close_in ic

let create_game() =
  let g = make() in
    init g;
    build_book g;
    g

let show_square board =
  for j=0 to boardY - 1 do
    for i=0 to boardX - 1 do
      if !(board.square.(elm i (boardY-j-1))) = 1 then 
	Printf.printf "O"
      else if !(board.square.(elm i (boardY-j-1))) = 2 then
	Printf.printf "X"
      else if !(board.square.(elm i (boardY-j-1))) = 0 then
	Printf.printf "."
      else Printf.printf "%i" !(board.square.(elm i (boardY-j-1)))
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n%!"

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
		else if !(board.square.(elm fx fy)) = 0
		then (a,b+1)
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
    if p1+p2 = 4 then p1 else (-1)

let check_even_below board square side =
  let px = elx square and py = ely square in
  let rec helper i =
    if i < py then
      if !(board.square.(elm px i)) = 0
	&& (check_threat board px i side)
      then true
      else helper (i+2)
    else false
  in helper 1
	  
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
	      board.solution.(board.sp).solgroupsnumb + 1
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
		  board.solution.(board.sp).solgroupsnumb + 1
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
		  if !(board.groups.(kg).(wx)) = 0 && (px<>x || py<>y) then
		    g2 := elm px py
	      done;
	      if elx !g1 = elx !g2 &&
		(ely !g1 - ely !g2 = 1 || ely !g1 - ely !g2= -1) then
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
		  pnt := !pnt+1;
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

let threat_group board group who =
  let p = !(board.groups.(group).(0)) lor !(board.groups.(group).(1)) lor
    !(board.groups.(group).(2)) lor !(board.groups.(group).(3)) in
    (p land who) land 1 <> 1

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

let handle_odd_above_even board tc =

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
      diago_NE_left (x-1) (y-1) (connect+1)
    else connect in
  let rec diago_NE_right x y connect =
    if x<boardX && y<boardY && !(board.square.(elm x y)) = board.turn then
      diago_NE_right (x+1) (y+1) (connect+1)
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
      if !(board.square.(elm move (board.stack.(move)))) = 0
      then raise (Failure "");
      board.square.(elm move (board.stack.(move))) := 0;
      board.filled <- board.filled - 1;
      board.moves.(board.filled) <- -1;
      board.mlist.(board.filled) <- -1;
      board.turn <- switch board.turn;
      true
    )
