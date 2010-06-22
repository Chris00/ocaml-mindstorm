open Utils
open Board
open Rules
open Adjacency
open Solver

let evaluate_black board =
  board.sp <- 0;
  board.intgp.j <- 0;
  board.intgp.k <- 0;
  board.sqused <- Array.make ((boardX+1) * (boardY+2)) true;
  for i = 0 to groups-1 do
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
	    let ccmb = !anythreat-thnum in
	    let px = elx tc.(ccmb).cross
	    and x = elx tc.(ccmb).odd in
	      for y=0 to boardY-1 do
		    if !(board.square.(elm px y)) = 0 then
		    board.sqused.(elm px y) <- false;
		    if !(board.square.(elm x y)) = 0 then
		    board.sqused.(elm x y) <- false;
	      done;
        if ely tc.(ccmb).even > ely tc.(ccmb).odd then
        handle_even_above_odd board tc.(ccmb)
        else handle_odd_above_even board tc.(ccmb)
	  );
	board.sp <- 0;
	board.intgp.j <- 0;
	board.intgp.k <- 0;
	for i=0 to groups-1 do
	  if threat_group board i 1 && not (wiped_group board i) &&
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
      done;
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
		  oracle := problem_solver board matrix;
      anythreat := !anythreat + 1;
      done;
    !oracle
      


let evaluation_function board =
  if board.turn = 1 then evaluate_black board
  else evaluate_white board

let start_best_move board =
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

let pentas board x y side =
  not (board.stack.(x)>=y || board.stack.(x+2)>=y || board.stack.(x+4)>=y ||
    !(board.square.(elm (x+1) y))<>side || !(board.square.(elm (x+3) y))<>side)
      

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


let fast_try_to_win board =
  let rec helper x =
    if x < boardX then
      if board.stack.(x) < boardY && connected board x >= 4 then x
      else helper (x+1)
    else -1 in helper 0


let avoid_immediate_loss board =
  board.turn <- switch board.turn;
  let a = fast_try_to_win board in
    board.turn <- switch board.turn;
    a


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
