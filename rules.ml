open Structure
open Utils

let claimeven board =
  let rec helper y1 =
    if y1<boardY then
    (
	 for x1=0 to boardX-1 do
       if board.sqused.(elm x1 y1) then
	    (
	      board.solution.(board.sp).solgroupsnumb <- 0;
	      let q1 = elm x1 y1 and q2 = elm x1 (y1-1) in
		  if !(board.square.(q1)) = 0 &&
		  !(board.square.(q2)) = 0 && board.sqused.(q2) then
		    (
		     let sln = board.solvable_groups.sqpnt.(q1)
		     and grp = board.solvable_groups.squar.(q1) in
		     for j=0 to sln-1 do
			  if board.intgp.tgroups.(grp.(j)) then
			    (
			      if board.solution.(board.sp).solgroupsnumb=0
			      then
				 (
				  board.solution.(board.sp).solname <- claimeven_;
				  board.solution.(board.sp).solpoint.(0) <- q1;
				  board.solution.(board.sp).solpoint.(1) <- q2;
				  board.solution.(board.sp).sqinv.(0) <- q1;
				  board.solution.(board.sp).sqinv.(1) <- q2;
				  board.solution.(board.sp).sqinvnumb <- 2;
				  board.instances.(claimeven_)<-board.instances.(claimeven_)+1
				 );
			      board.solution.(board.sp).solgroups.
				(board.solution.(board.sp).solgroupsnumb) <- grp.(j);
			      board.solution.(board.sp).solgroupsnumb <-
				board.solution.(board.sp).solgroupsnumb+1;
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
				  board.sp <- board.sp + 1
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
			      if !(board.square.(elm px py)) = 0 
			      then
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
				> 0 then board.sp <- board.sp + 1
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
			  for x=0 to tiles-1 do
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
		    done;
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
