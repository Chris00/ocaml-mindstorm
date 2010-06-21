open Utils
open Structure
open Heuristic
open Solver
open Evaluate
open Opening_book

let look_ahed board =
   let depth = 4
   and def_depth = ref 0
   and def_nodes = ref 0
   and set = ref false
   and next = ref (-1)
   and sc = ref impossible
   and score = Array.init boardX (fun i-> impossible) in
    for x=0 to boardX-1 do
        if board.stack.(x) < boardY then
        (
            if not (makemove board x) then raise Error;
            let p = heuristic_play_best board 5000 in
            def_nodes := !def_nodes + board.nodes_visited;
            def_depth := max !def_depth board.maxtreedepth;
            if p >=0 then score.(x) <- badmove + board.maxtreedepth
            else if p = -2 then score.(x) <- goodmove - board.maxtreedepth
            else
              (
                score.(x) <- explore_tree board (switch board.turn) (depth-1);
                 if score.(x) > -3500 && score.(x) < 3500 then
                    score.(x) <- score.(x) + random_int 35;
                 let oracle = evaluation_function board in
                    if oracle then
                    (
                      score.(x) <- score.(x) + 1;
                      set := true;
                   )
              );
         if not (undomove board x) then raise Error
        );
      if score.(x) > !sc or (score.(x) = !sc && random_bool())
      then (sc := score.(x);next := x)
    done;
    !next


let move_for board =
  board.choices.(board.filled) <- 1;
  if board.filled = 0 then 3
  else if board.filled = 1 then
    (
      if board.moves.(0) = 1 then 2
      else if board.moves.(0) = 5 then 4
      else 3;
    )
  else if board.filled = maxmen - 1 then
      (
        let rec helper x =
	      if board.stack.(x) < boardY then x
	      else helper (x+1)
          in helper 0;
      )
    else let a = fast_try_to_win board in if a >= 0 then a
	else let b = avoid_immediate_loss board in if b >= 0 then b
	else let c = start_best_move board in if c >= 0 then c
    else let d = use_opening_book board board.turn in if d >=0 then d
   	else let e = heuristic_play_best board 2800 in if e >= 0 then e
   	else look_ahed board
