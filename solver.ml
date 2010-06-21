open Utils
open Structure

let rules_name = [|"CLAIMEVEN";"BASEINVERSE";"VERTICAL";"LOWINVERSE";"HIGHINVERSE";"BASECLAIM";"BEFORE";"SPECIALBEFORE"|]
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
      if (not pblist.problem.(i).solved) then
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
    hmprobs = !probs
  } in
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
