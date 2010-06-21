open Utils
open Structure
open Heuristic

let expand_block blk pss =
    pss.(elm 0 0) <- (blk.(0+0*2)  lsr 6)land 0x03;
    pss.(elm 1 0) <- (blk.(0+0*2)  lsr 4)land 0x03;
    pss.(elm 2 0) <- (blk.(0+0*2)  lsr 2)land 0x03;
    pss.(elm 3 0) <- (blk.(0+0*2)  )land 0x03;
    pss.(elm 4 0) <- (blk.(1+0*2)  lsr 6)land 0x03;
    pss.(elm 5 0) <- (blk.(1+0*2)  lsr 4)land 0x03;
    pss.(elm 6 0) <- (blk.(1+0*2)  lsr 2)land 0x03;
    pss.(elm 7 0) <- 0xff;

    pss.(elm 0 1) <- (blk.(0+1*2)  lsr 6)land 0x03;
    pss.(elm 1 1) <- (blk.(0+1*2)  lsr 4)land 0x03;
    pss.(elm 2 1) <- (blk.(0+1*2)  lsr 2)land 0x03;
    pss.(elm 3 1) <- (blk.(0+1*2)  )land 0x03;
    pss.(elm 4 1) <- (blk.(1+1*2)  lsr 6)land 0x03;
    pss.(elm 5 1) <- (blk.(1+1*2)  lsr 4)land 0x03;
    pss.(elm 6 1) <- (blk.(1+1*2)  lsr 2)land 0x03;
    pss.(elm 7 1) <- 0xff;

    pss.(elm 0 2) <- (blk.(0+2*2)  lsr 6)land 0x03;
    pss.(elm 1 2) <- (blk.(0+2*2)  lsr 4)land 0x03;
    pss.(elm 2 2) <- (blk.(0+2*2)  lsr 2)land 0x03;
    pss.(elm 3 2) <- (blk.(0+2*2)  )land 0x03;
    pss.(elm 4 2) <- (blk.(1+2*2)  lsr 6)land 0x03;
    pss.(elm 5 2) <- (blk.(1+2*2)  lsr 4)land 0x03;
    pss.(elm 6 2) <- (blk.(1+2*2)  lsr 2)land 0x03;
    pss.(elm 7 2) <- 0xff;

    pss.(elm 0 3) <- (blk.(0+3*2)  lsr 6)land 0x03;
    pss.(elm 1 3) <- (blk.(0+3*2)  lsr 4)land 0x03;
    pss.(elm 2 3) <- (blk.(0+3*2)  lsr 2)land 0x03;
    pss.(elm 3 3) <- (blk.(0+3*2)  )land 0x03;
    pss.(elm 4 3) <- (blk.(1+3*2)  lsr 6)land 0x03;
    pss.(elm 5 3) <- (blk.(1+3*2)  lsr 4)land 0x03;
    pss.(elm 6 3) <- (blk.(1+3*2)  lsr 2)land 0x03;
    pss.(elm 7 3) <- 0xff;

    pss.(elm 0 4) <- (blk.(0+4*2)  lsr 6)land 0x03;
    pss.(elm 1 4) <- (blk.(0+4*2)  lsr 4)land 0x03;
    pss.(elm 2 4) <- (blk.(0+4*2)  lsr 2)land 0x03;
    pss.(elm 3 4) <- (blk.(0+4*2)  )land 0x03;
    pss.(elm 4 4) <- (blk.(1+4*2)  lsr 6)land 0x03;
    pss.(elm 5 4) <- (blk.(1+4*2)  lsr 4)land 0x03;
    pss.(elm 6 4) <- (blk.(1+4*2)  lsr 2)land 0x03;
    pss.(elm 7 4) <- 0xff;

    pss.(elm 0 5) <- (blk.(0+5*2)  lsr 6)land 0x03;
    pss.(elm 1 5) <- (blk.(0+5*2)  lsr 4)land 0x03;
    pss.(elm 2 5) <- (blk.(0+5*2)  lsr 2)land 0x03;
    pss.(elm 3 5) <- (blk.(0+5*2)  )land 0x03;
    pss.(elm 4 5) <- (blk.(1+5*2)  lsr 6)land 0x03;
    pss.(elm 5 5) <- (blk.(1+5*2)  lsr 4)land 0x03;
    pss.(elm 6 5) <- (blk.(1+5*2)  lsr 2)land 0x03;
    pss.(elm 7 5) <- 0xff


let collapse_position mypos blk =
	for y = 0 to boardY-1 do
        blk.(0+y*2) <- ((mypos.(elm 0 y) land 0x03)lsl 6) lor 
                    ((mypos.(elm 1 y) land 0x03)lsl 4) lor 
                    ((mypos.(elm 2 y) land 0x03)lsl 2) lor 
                    ((mypos.(elm 3 y) land 0x03));

        blk.(1+y*2) <- ((mypos.(elm 4 y) land 0x03)lsl 6) lor 
                    ((mypos.(elm 5 y) land 0x03)lsl 4) lor 
                    ((mypos.(elm 6 y) land 0x03)lsl 2) lor  0x03;
	done;
    blk.(12) <- 255;
    blk.(13) <- 255

let mybincmp bb tp len =
  let rec helper i =
   if i = len then 0
   else if bb.(i)<tp.(i) then -1
   else if bb.(i)>tp.(i) then 1
   else helper (i+1) in
   helper 0


let get_lower bb =
  let tp = Array.init 64 (fun i -> !(bb.(i)))
  and tp2 = Array.make 64 0xff in
  for y = 0 to boardY - 1 do
    for x = 0 to boardX - 1 do
      tp2.(elm x y) <- !(bb.(elm (boardX-x-1) y));
    done
  done;
  if mybincmp tp tp2 64 = 1 then tp2 else tp


let check_book board cmparray side =
    if side = 1 then (
        cmparray.(12) <- 1;
        cmparray.(13) <- 0)
    else (
        cmparray.(12) <- 255;
        cmparray.(13) <- 255);
    let rec helper head tail =
      if head <= tail then
       let x = (head + tail) / 2 in
       let res = mybincmp cmparray (board.white_book.(x)) 14 in      
        if res = -1 then helper head (x-1)
       else if res = 1 then helper (x+1) tail
       else true
      else false
    in helper 0 (Array.length board.white_book)


let use_opening_book board side =
  let rec helper x =
    if x < boardX then
      if board.stack.(x) < boardY then
       (
        if not (makemove board x) then raise Error;
        let temp = get_lower (board.square)
        and blk = Array.make 14 255 in
        collapse_position temp blk;
        if check_book board blk side
        then (if not (undomove board x) then raise Error else x)
        else (if not (undomove board x) then raise Error else helper (x+1))
       )
      else helper (x+1) 
    else -1 in helper 0
