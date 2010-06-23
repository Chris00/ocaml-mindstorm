open Utils
open Evaluate

type status = Proved | Disproved | Unknown
exception Error

type info =
    {
      mutable max_tree_depth : int;
      mutable bestmove : int
    }

(*A node of a graph. *)
type node =
    {
      mutable squaree : int array;
      (* A position of a possible game of connect 4. *)
      mutable stac : int array;
      (* The number of pieces in each column *)
      mutable tur : int;
      (* The player who will play next *)
      mutable evaluated : bool;
      (* A boolean saying if the node has been evaluated *)
      mutable expanded : bool;
      (* A boolean saying if the node has been expanded *)
      mutable value : status;
      (* The "value" of the node *)
      mutable typed : int;
      (* The type of the node. *)
      mutable proof : int;
      (* Number saying if the node only needs a few checks to be
         evaluated as a winning node *)
      mutable disproof : int;
      (* if the node only needs a few checks to be evaluated as a
         losing node *)
      mutable child : node option array;
      (* an array of his seven possible children  *)
      mutable parents : node option array;
      (* an array of his seven possible fathers *)
    }

let show node =
  let square = node.squaree in
  for j=0 to boardY - 1 do
    for i=0 to boardX - 1 do
      if (square.(elm i (boardY-j-1))) = 1 then 
	Printf.printf "O"
      else if (square.(elm i (boardY-j-1))) = 2 then
	Printf.printf "X"
      else if (square.(elm i (boardY-j-1))) = 0 then
	Printf.printf "."
      else Printf.printf "%i" (square.(elm i (boardY-j-1)))
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n%!"

type bintree =
  {
    mutable parent : bintree option;
    mutable lson : bintree option;
    mutable rson : bintree option;
    mutable node : node
  }

let make_node() =
  {
    squaree = Array.make ((boardX+1)*(boardY+2)) 0;
    tur = 0;
    stac = Array.make (boardX+1) 0;
    evaluated = false;
    expanded = false;
    value = Unknown;
    typed = or_type;
    proof = 0;
    disproof = 0;
    child = Array.make boardX None;
    parents = Array.make boardX None;
  }

let rootnode = ref (make_node())
let auxboard = Board.make()
let her_node_not_expanded = ref 0
let her_node_expanded = ref 0
let nodeseq = [|3;2;4;5;1;0;6|]

let copy a =
  { a with
    squaree = Array.copy a.squaree;
    stac = Array.copy a.stac;
    child = Array.copy a.child;
    parents = Array.copy a.parents;
  }

let her_generate_all_children node =
  for x=0 to boardX-1 do
    if node.stac.(x) < boardY then
      (
        let no =
	  {
	    squaree = Array.copy node.squaree;
	    stac = Array.copy node.stac;
	    tur = switch node.tur;
	    child = Array.make boardX None;
	    parents = Array.make boardX None;
	    evaluated = false;
	    expanded = false;
	    value = Unknown;
	    typed = if node.typed = or_type then and_type else or_type;
	    proof = 0;
	    disproof = 0;
	  } in
        no.squaree.(elm x (no.stac.(x))) <- node.tur;
        no.stac.(x) <- no.stac.(x) + 1;
        node.child.(x) <- Some no;
	no.expanded <- false;
	no.evaluated <- false;
	for y = 0 to boardX-1 do
	  no.parents.(y) <- None
	done;
	no.parents.(x) <- Some node;
	for y = 0 to boardX-1 do
	  no.child.(y) <- None
	done;
	her_node_expanded := !her_node_expanded + 1
      )
    else node.child.(x) <- None
  done


let rec explore_tree board side depth = 
  let cn = ref 0
  and tmp = if board.Board.turn = side then
      group_eval board
    else (-1) * (group_eval board) in
  let answ = ref 0 in
  if depth = 0 then tmp
  else
    (
      if board.Board.turn <> side then
	(
	  answ := goodmove-depth;
	  let x = ref 0 in
	  while !x<boardX && !answ <> badmove do
	    if board.Board.stack.(!x) < boardY then
	      (
		cn := !cn+1;
		if Board.connected board !x >=4 then answ := badmove
		else
		  (
		    let _ = Board.makemove board !x
		    and tmp = explore_tree board side (depth-1) in
                    answ := min !answ tmp;
		    if not (Board.undomove board !x) then raise Error;
		  )
	      );
            x := !x + 1
	  done;
	  if !cn = 0 then answ := 0
	)
      else
	(
	  answ := badmove + depth;
	  let x = ref 0 in
	  while !x<boardX do
	    if board.Board.stack.(!x) < boardY then
	      (
		cn := !cn + 1;
		if Board.connected board !x >= 4 then answ := goodmove
		else
		  (
		    let _ = Board.makemove board !x
		    and tmp = explore_tree board side (depth-1) in
		    answ := max !answ tmp;
		    if not (Board.undomove board !x) then raise Error
		  )
	      );
            x := !x + 1
	  done;
	  if !cn = 0 then answ := 0
	);
      !answ
    )


let her_evaluate node =
  node.evaluated <- true;
  for x=0 to 63 do
    auxboard.Board.square.(x) := node.squaree.(x)
  done;
  auxboard.Board.turn <- node.tur;
  auxboard.Board.filled <- 0;
  auxboard.Board.stack <- Array.copy node.stac;
  for x=0 to boardX do
    if (x<boardX) then
      auxboard.Board.filled <- auxboard.Board.filled + node.stac.(x)
  done;
  if auxboard.Board.filled = maxmen then
    (
      if !rootnode.tur = 1 then
	if !rootnode.typed = or_type then node.value <- Proved
	else node.value <- Disproved
      else 
	if node.typed = or_type then node.value <- Disproved
	else node.value <- Proved;
      -1
    )
  else
    (
      let bestmove = fast_try_to_win auxboard in
      if bestmove <> -1 then
        (
          if node.typed = or_type then node.value <- Proved
          else node.value <- Disproved)
      else node.value <- Unknown;
      bestmove
    )

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
  else if node.typed = or_type then
    (
      node.proof <- 1;
      node.disproof <- !children
    )
  else raise Error


let her_set_proof_and_disproof_numbers node =
  if node.expanded then
    if node.typed = and_type then
      (
	node.proof <- 0;
	node.disproof <- max_int;
	for x=0 to boardX-1 do
	  match node.child.(x) with
	  |None -> ()
	  |Some no -> (node.proof <- node.proof + no.proof;
		      node.disproof <- min node.disproof no.disproof)
	done;
	if node.disproof = 0 then node.proof <- max_int
      )
    else if node.typed = or_type then
      (
	node.proof <- max_int;
	node.disproof <- 0;
	for x=0 to boardX-1 do
	  match node.child.(x) with
	  |None -> ()
	  |Some no -> (node.proof <- min node.proof no.proof;
		      node.disproof <- node.disproof + no.disproof)
	done;
	if node.proof = 0 then node.disproof <- max_int
      )
    else raise Error
    else if node.evaluated then
      match node.value with
      | Proved ->
        node.proof <- 0;
        node.disproof <- max_int;
      | Disproved ->
        node.proof <- max_int;
        node.disproof <- 0
      | Unknown ->
        her_set_pdv_according_to_children node
    else raise Error
      


let her_ressources_available maxnodes =
  !her_node_expanded <= maxnodes


let rec her_select_most_proving_node node info depth =
  let flag = ref false
  and i = ref 0 in
  if node.expanded then
    (    
      if node.typed = or_type then
        (
          while !i<boardX && not (!flag) do
            match node.child.(nodeseq.(!i)) with
            |None -> (i:= !i+1)
            |Some no -> if no.proof <= node.proof then flag := true else i := !i+1
          done;
        )
      else if node.typed = and_type then
        (
          while !i<boardX && not (!flag) do
            match node.child.(nodeseq.(!i)) with
            |None -> (i := !i+1)
            |Some no -> if no.disproof <= node.disproof then flag := true
              else i := !i+1;
          done;
        )
      else (Printf.printf "Unknown type\n%!";raise Error);
      if depth = 0 then info.bestmove <- nodeseq.(!i);
      if not (!flag)
      then (her_set_proof_and_disproof_numbers node;her_select_most_proving_node node info depth)
      else match node.child.(nodeseq.(!i)) with
      |None -> raise Error
      |Some n -> her_select_most_proving_node n info (depth+1)
    )
  else node

let her_develop_node node =
  node.expanded <- true;
  her_generate_all_children node;
  for i=0 to boardX-1 do
    match node.child.(i) with
    |None -> ()
    |Some no -> (
      let _ = her_evaluate no in
      her_set_proof_and_disproof_numbers no)
  done


let rec her_update_ancestors node =
  her_set_proof_and_disproof_numbers node;
  for x=0 to boardX-1 do
    match node.parents.(x) with
    |None -> ()
    |Some n -> her_update_ancestors n
  done
    

let her_pn_search root maxnodes info =
  info.max_tree_depth <- 1;
  info.bestmove <- her_evaluate root;
  her_set_proof_and_disproof_numbers root;
  while root.proof <> 0 && root.disproof <> 0 && her_ressources_available maxnodes do
    let her_most_proving_node = her_select_most_proving_node root info 0 in
    her_develop_node her_most_proving_node;
    her_update_ancestors her_most_proving_node;
  done;
  if root.proof = 0 then root.value <- Proved
  else if root.disproof = 0 then root.value <- Disproved
  else root.value <- Unknown
    


let heuristic_play_best board maxnodenum =
  let rootnode =
    { squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = board.Board.turn;
      stac = Array.init (boardX+1) (fun x -> board.Board.stack.(x));
      evaluated = false;
      expanded = false;
      value = Unknown;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.make boardX None;
      parents = Array.make boardX None;
    } in
  let b = Board.copy board in
  for y=0 to boardY-1 do
    rootnode.squaree.(elm boardX y) <- !(b.Board.square.(elm boardX y));
    for x = 0 to boardX-1 do
      rootnode.squaree.(elm x y) <- !(b.Board.square.(elm x y));
    done;
  done;
  let info = {
    bestmove = -1;
    max_tree_depth = 0
  } in
  her_node_expanded := 0;
  her_node_not_expanded := 0;
  her_pn_search rootnode maxnodenum info;
  let mymove =
    match rootnode.value with
    | Unknown -> -1
    | Disproved -> -2
    | Proved -> info.bestmove in
  (* FIXME: not clear why one makes these modifications since [b] will
     be garbage collected. *)
  b.Board.nodes_visited <- !her_node_expanded + !her_node_not_expanded;
  b.Board.maxtreedepth <- info.max_tree_depth;
  mymove
