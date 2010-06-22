open Utils
open Structure

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
    if !cnt land 1 = 1 then answer := false;
    x := !x+1
  done;
  !answer



let comp_rules board p1 p2 =
  let c1 = board.solution.(p1).solname - 1
  and c2 = board.solution.(p2).solname - 1
  and bol = ref true in
  let way = rulecombo.(c1).(c2) in
  if way land 9 <> 0 then (
    board.rules.(0) <- board.rules.(0) + 1;
    if overlap board p1 p2 then bol := false
  )
  else if way land 2 <> 0 then (
    board.rules.(1) <- board.rules.(1) + 1;
    if overlap board p1 p2 then bol := false
  )
  else if way land 4 <> 0 then (
    board.rules.(2) <- board.rules.(2) + 1;
    if not (column_wdoe board p1 p2) then bol := false
  );
  !bol



let build_adjacency_matrix board =
  let matrix = Array.make_matrix board.sp board.sp false in
  for x=0 to board.sp-1 do
    for y=x to board.sp-1 do
      if comp_rules board x y then matrix.(y).(x) <- true
    done
  done;
  matrix
