(* Inspired by http://www.ce.unipr.it/~gbe/velsrc.html *)

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

type rule_t =
  | CLAIMEVEN | BASEINVERSE | VERTICAL | AFTEREVEN | LOWINVERSE
  | HIGHINVERSE | BASECLAIM | BEFORE | SPECIALBEFORE

module Rule : sig
  type t = rule_t
  val to_int : t -> int
    (** Returns a number in [0 .. n-1] where [n] is the number of
        rules which are numbered in the order of their declaration. *)
  type 'a vec (* abstract to force the use of the functions of Rule *)
  val vec : int -> 'a -> 'a vec
  val incr : int vec -> t -> unit
end =
struct
  type t = rule_t
  let to_int (r:t) = (Obj.magic r : int) (* WARNING : unsafe *)

  type 'a vec = 'a array
  let vec n a = Array.make n a
  let get a r = a.(to_int r)
  let incr a r = let i = to_int r in a.(i) <- a.(i) + 1

  let rules_name = [|"CLAIMEVEN";"BASEINVERSE";"VERTICAL";"LOWINVERSE";
                     "HIGHINVERSE";"BASECLAIM";"BEFORE";"SPECIALBEFORE"|]
end

let elm x y = (x+(y lsl 3))
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


type solvable_groups = int array array

type solution = {
  mutable valid : bool;
  mutable solname : Rule.t;
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
  stack : int array; (* stack.(c) = the number of piece in the column c *)
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
  instances : int Rule.vec;
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
  let sg = board.solvable_groups.(elm px i) in
  let rec helper y =
    if y < Array.length sg then
      let j = sg.(y) in
        if board.xplace.(j).(0) = board.xplace.(j).(3) then
          helper (y+1)
        else (
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
          in
          help_intern 0 0 0 || helper (y+1)
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
      if y = -1 then helper (x+1) oddpnt
      else (
        threats.(oddpnt) <- y;
        helper (x+1) (oddpnt + 1)
      )
    else oddpnt
  in helper 0 0

let both_groups board q1 q2 =
  let sg1 = board.solvable_groups.(q1)
  and sg2 = board.solvable_groups.(q2) in
  let sol = board.solution.(board.sp) in
  for x=0 to Array.length sg1 - 1 do
    for y=0 to Array.length sg2 - 1 do
      let g1 = sg1.(x)
      and g2 = sg2.(y) in
      if g1 = g2 && board.intgp.tgroups.(g1) then (
        sol.solgroups.(sol.solgroupsnumb) <- g1;
        sol.solgroupsnumb <- sol.solgroupsnumb + 1
      )
    done;
  done

let rec recurse_groups board cols cl gp =
  let sg = board.solvable_groups.(cl.(0)) in
  let len_sg = Array.length sg in
  let rec helper i =
    if i < len_sg then
      let g1 = sg.(i) in
      if g1 <> gp then helper (i+1)
      else if cols=1
        || recurse_groups board (cols-1) (Array.sub cl 1 ((Array.length cl)-1)) g1 then
          true
      else helper (i+1)
    else false
  in helper 0

let both_many_groups board cols cl =
  if cols <> 0 then
    let sg = board.solvable_groups.(cl.(0)) in
    let len_sg = Array.length sg in
    let sol = board.solution.(board.sp) in
    let rec helper i =
      if i < len_sg then
        let g1 = sg.(i) in
        if not board.intgp.tgroups.(g1) then helper (i+1)
        else if cols=1 || recurse_groups board (cols-1)
          (Array.sub cl 1 ((Array.length cl)-1)) g1
        then (
          sol.solgroups.(sol.solgroupsnumb) <- g1;
          sol.solgroupsnumb <- sol.solgroupsnumb + 1
        )
        else helper (i+1)
    in helper 0

let solve_columns board cl cols =
  let sol = board.solution.(board.sp) in
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
          if sol.solgroupsnumb = 0 then (
            sol.sqinvnumb <- 2 * cl;
            for t=0 to cl-1 do
              let tx = elx cols.(t) and ty = ely cols.(t) in
              sol.sqinv.(t) <- elm tx (ty-1);
              sol.sqinv.(t+cl) <- elm tx ty;
            done;
            sol.solgroups.(sol.solgroupsnumb) <- i;
            sol.solgroupsnumb <- sol.solgroupsnumb +1
          )
      )
  in helper 0

let check_claim board cl =
  let px = elx cl.(1) and py = elx cl.(1) in
  if py < boardY && (py land 1 = 1) then (
    let sol = board.solution.(board.sp) in
    sol.solgroupsnumb <- 0;
    sol.solname <- BASEINVERSE;
    sol.sqinv.(0) <- cl.(0);
    sol.sqinv.(1) <- cl.(1);
    sol.sqinv.(2) <- cl.(2);
    sol.sqinv.(3) <- elm px py;
    sol.sqinvnumb <- 4;
    sol.solpoint.(0) <- cl.(0);
    sol.solpoint.(1) <- elm px py;
    both_groups board cl.(0) (elm px py);
    Rule.incr board.instances BASECLAIM;
    if sol.solgroupsnumb > 0 then (
      both_groups board cl.(1) cl.(2);
      board.sp <- board.sp + 1
    );
    let sol = board.solution.(board.sp) in
    sol.solgroupsnumb <- 0;
    sol.solname <- BASEINVERSE;
    sol.sqinv.(0) <- cl.(0);
    sol.sqinv.(1) <- cl.(1);
    sol.sqinv.(2) <- cl.(2);
    sol.sqinv.(3) <- elm px py;
    sol.sqinvnumb <- 4;
    sol.solpoint.(0) <- elm px py;
    sol.solpoint.(1) <- cl.(2);
    both_groups board (elm px py) cl.(2);
    Rule.incr board.instances BASECLAIM;
    if sol.solgroupsnumb > 0 then (
      both_groups board cl.(0) cl.(1);
      board.sp <- board.sp + 1
    )
  )

let generate_all_other_before_instances board cols cl j =
  let step = 128 lsr cols
  and gc = Array.make_matrix 4 3  0 in
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
        let pn = Array.make 4 ((cnt lsr 6) land 1)
        and sl = Array.matrix 4 2  0
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
        if !flag then (
          let sol = board.solution.(board.sp) in
          sol.solgroupsnumb <- 0;
          sol.solname <- BEFORE;
          sol.solpoint.(0) <-
            elm board.xplace.(!j).(0) board.yplace.(!j).(0);
          sol.solpoint.(1) <-
            elm board.xplace.(!j).(3) board.yplace.(!j).(3);
          sol.sqinvnumb <- 2 * cols;
          for x=0 to 2 * cols -1 do
            sol.sqinv.(x) <- sl.(x lsr 1).(x land 1)
          done;
          for x = 0 to cols-1 do
            let py2 = ely sl.(x).(1) in
            if py2 land 1 = 1 then
              both_many_groups board 1
                (Array.sub sl.(x) 1 ((Array.length sl.(x))-1))
            else both_groups board sl.(x).(0) sl.(x).(1)
          done;
          both_many_groups board cols cl;
          Rule.incr board.instances BEFORE;
          if sol.solgroupsnumb > 0 then
            board.sp <- board.sp + 1
        );
        helper (cnt+step)
      )
  in helper 0

let check_double_threat board x y tch pnt =
  let pq = elm x y in
  let sg = board.solvable_groups.(pq) in
  for j=0 to Array.length sg - 2 do
    for k = j+1 to Array.length sg - 1 do
      let jg = sg.(j)
      and kg = sg.(k) in
      let w1 = check_men board jg 1
      and w2 = check_men board kg 1
      and g1 = ref 0
      and g2 = ref 0 in
      if w1=2 && w2=2 then (
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
    let g = board.solvable_groups.(cl.(0)) in
    for i = 0 to Array.length g - 1 do
      let gi = g.(i) in
      if board.usablegroup.(gi) then
        if cols = 1 || recurse_groups board (cols-1)
          (Array.sub cl 1 ((Array.length cl)-1)) gi
        then board.usablegroup.(gi) <- false
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
    if y1<boardY then (
      for x1=0 to boardX-1 do
        if board.sqused.(elm x1 y1) then (
          let sol = board.solution.(board.sp) in
          sol.solgroupsnumb <- 0;
          let q1 = elm x1 y1 and q2 = elm x1 (y1-1) in
          if !(board.square.(q1)) = 0 && !(board.square.(q2)) = 0
            && board.sqused.(q2) then (
              let grp = board.solvable_groups.(q1) in
              for j=0 to Array.length grp - 1 do
                if board.intgp.tgroups.(grp.(j)) then (
                  if sol.solgroupsnumb = 0 then (
                    sol.solname <- CLAIMEVEN;
                    sol.solpoint.(0) <- q1;
                    sol.solpoint.(1) <- q2;
                    sol.sqinv.(0) <- q1;
                    sol.sqinv.(1) <- q2;
                    sol.sqinvnumb <- 2;
                    Rule.incr board.instances CLAIMEVEN;
                  );
                  sol.solgroups.(sol.solgroupsnumb) <- grp.(j);
                  sol.solgroupsnumb <- sol.solgroupsnumb + 1
                );
              done;
            );
          if sol.solgroupsnumb > 0 then
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
            let grp = board.solvable_groups.(q1) in
            for j=0 to Array.length grp - 1 do
              if board.intgp.tgroups.(grp.(j)) then
                for x=0 to tiles-1 do
                  let wx = board.xplace.(grp.(j)).(x)
                  and wy = board.yplace.(grp.(j)).(x) in
                  if x1<wx && board.stack.(wx)=wy &&
                    set.(elm wx wy) && board.sqused.(elm wx wy)
                  then (
                    set.(elm wx wy) <- false;
                    let sol = board.solution.(board.sp) in
                    sol.solgroupsnumb <- 0;
                    sol.solname <- BASEINVERSE;
                    sol.solpoint.(0) <- q1;
                    sol.solpoint.(1) <- elm wx wy;
                    sol.sqinv.(0) <- q1;
                    sol.sqinv.(1) <- elm wx wy;
                    sol.sqinvnumb <- 2;
                    both_groups board q1 (elm wx wy);
                    Rule.incr board.instances BASEINVERSE;
                    if sol.solgroupsnumb > 0 then
                      board.sp <- board.sp + 1;
                  )
                done;
            done;
          )
    done;
  done



let vertical board =
  let set = Array.make 64 true in
  for y1 = 0 to boardY - 1 do
    for x1=0 to boardX-1 do
      let q1=elm x1 y1 in
      if board.sqused.(q1) then
        if !(board.square.(q1)) = 0 then
          (
            memset set true 64;
            let grp = board.solvable_groups.(q1) in
            for j=0 to Array.length grp - 1 do
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
                      let sol = board.solution.(board.sp) in
                      sol.solgroupsnumb <- 0;
                      sol.solname <- VERTICAL;
                      sol.solpoint.(0) <- q1;
                      sol.solpoint.(1) <- elm x1 wy;
                      sol.sqinv.(0) <- q1;
                      sol.sqinv.(1) <- elm x1 wy;
                      sol.sqinvnumb <- 2;
                      both_groups board q1 (elm x1 wy);
                      Rule.incr board.instances VERTICAL;
                      if sol.solgroupsnumb > 0 then
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
    if y1<boardY then (
      for x1=0 to boardX-1 do
        let q1=elm x1 y1 in
        if board.sqused.(q1) then
          (
            let grp = board.solvable_groups.(q1) in
            for j = 0 to Array.length grp - 1 do
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
                  if !after && !cols>0 then (
                    let sol = board.solution.(board.sp) in
                    sol.solgroupsnumb <- 0;
                    sol.solname <- AFTEREVEN;
                    sol.solpoint.(0) <- q1;
                    sol.solpoint.(1) <-
                      elm (board.xplace.(grp.(j)).(3))
                      (board.yplace.(grp.(j)).(3));
                    Rule.incr board.instances AFTEREVEN;
                    sol.sqinvnumb <- !cols;
                    for pj=0 to !cols-1 do
                      sol.sqinv.(pj) <-cl.(pj)
                    done;
                    solve_columns board !cols cl;
                    if sol.solgroupsnumb > 0 then
                      board.sp <- board.sp+1
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
                let grp = board.solvable_groups.(q1) in
                for j = 0 to Array.length grp - 1 do
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
                              let sol = board.solution.(board.sp) in
                              sol.solgroupsnumb <- 0;
                              sol.solname <- LOWINVERSE;
                              sol.solpoint.(0) <- q1;
                              sol.solpoint.(1) <- elm wx wy;
                              sol.sqinv.(0) <- q1;
                              sol.sqinv.(1) <- elm wx wy;
                              sol.sqinv.(2) <- elm x1 (y1-1);
                              sol.sqinv.(3) <- elm wx (wy-1);
                              sol.sqinvnumb <- 4;
                              both_groups board q1 (elm wx wy);
                              both_groups board q1 (elm x1 (y1-1));
                              both_groups board (elm wx wy)
                                (elm wx (wy-1));
                              Rule.incr board.instances LOWINVERSE;
                              if sol.solgroupsnumb > 0 then
                                board.sp <- board.sp + 1;
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
        for x1=0 to boardX-1 do
          let q1 = elm x1 y1 in
          if board.sqused.(q1) then
            (
              memset set true 64;
              let grp = board.solvable_groups.(q1) in
              for j = 0 to Array.length grp - 1 do
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
                            let sol = board.solution.(board.sp) in
                            sol.solgroupsnumb<-0;
                            sol.solname <- HIGHINVERSE;
                            sol.solpoint.(0) <- q1;
                            sol.solpoint.(1) <- elm wx wy;
                            sol.sqinv.(0) <- q1;
                            sol.sqinv.(1) <- elm wx wy;
                            sol.sqinv.(2) <- elm x1 (y1-1);
                            sol.sqinv.(3) <- elm wx (wy-1);
                            sol.sqinv.(4) <- elm x1 (y1+1);
                            sol.sqinv.(5) <- elm wx (wy+1);
                            sol.sqinvnumb <- 6;
                            Rule.incr board.instances HIGHINVERSE;
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
                            if sol.solgroupsnumb > 0 then
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
          let grp = board.solvable_groups.(q1) in
          for j = 0 to Array.length grp - 1 do
            if board.intgp.tgroups.(grp.(j)) &&
              board.xplace.(grp.(j)).(0) <> board.yplace.(grp.(j)).(3)
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
                            check_claim board cl;
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




(* Adjacency Matrix *)
module Adjacency =
struct

  let rulecombo = [|[|1; 1; 1; 1; 3; 3; 1; 1; 1|];
                    [|1; 1; 1; 1; 1; 1; 1; 1; 1|];
                    [|1; 1; 1; 1; 1; 1; 1; 1; 1|];
                    [|1; 1; 1; 4; 3; 3; 1; 4;12|];
                    [|3; 1; 1; 3; 8; 8; 3; 6; 6|];
                    [|3; 1; 1; 3; 8; 8; 3; 3; 3|];
                    [|1; 1; 1; 1; 3; 3; 1; 1; 1|];
                    [|1; 1; 1; 4; 6; 3; 1; 4; 4|];
                    [|1; 1; 1;12; 6; 3; 1; 4; 4|]|]

  let overlap board p1 p2 =
    let temp = Array.make ((boardX+1)*(boardY+2)) false in
    for x = 0 to board.solution.(p2).sqinvnumb - 1 do
      temp.(board.solution.(p2).sqinv.(x)) <- true
    done;
    let bol = ref false in
    for x=0 to board.solution.(p1).sqinvnumb - 1 do
      if temp.(board.solution.(p1).sqinv.(x)) then bol := true ;
    done;
    !bol


  let claimeven_below board p1 p2 =
    let name = board.solution.(p1).solname in
    let (q1,q2) = (if name <> HIGHINVERSE && name <> LOWINVERSE then (p2,p1)
                   else (p1,p2)) in
    assert(let name = board.solution.(q1).solname in
           name = HIGHINVERSE || name = LOWINVERSE);
    let bol = ref true in
    match board.solution.(q2).solname with
    | AFTEREVEN ->
        let solcheck = board.solution.(q2).sqinvnumb / 2 in
        for x = 0 to 1 do
          let q1x = elx board.solution.(q1).sqinv.(x+2)
          and q1y = ely board.solution.(q1).sqinv.(x+2) in
          for y=0 to solcheck-1 do
            let q2x = elx board.solution.(q2).sqinv.(solcheck+y)
            and q2y = ely board.solution.(q2).sqinv.(solcheck+y) in
            if q1x=q2x && q1y>q2y && (q2y land 1 = 1) then bol:=true
          done
        done;
        !bol
    | BEFORE | SPECIALBEFORE ->
        let solcheck = board.solution.(q2).sqinvnumb / 2 in
        for x=0 to 1 do
          let q1x = elx board.solution.(q1).sqinv.(x+2)
          and q1y = ely board.solution.(q1).sqinv.(x+2) in
          for y=0 to solcheck - 1 do
            let q2x = elx board.solution.(q2).sqinv.(1+(y lsl 1))
            and q2y = ely board.solution.(q2).sqinv.(1+(y lsl 1)) in
            if q1x=q2x && q1y>q2y && (q2y land 1=1) then bol:=true
          done
        done;
        !bol
    | CLAIMEVEN ->
        for x=0 to 1 do
          let q1x = elx board.solution.(q1).sqinv.(x+2)
          and q1y = elx board.solution.(q1).sqinv.(x+2)
          and q2x = elx board.solution.(q2).sqinv.(0)
          and q2y = ely board.solution.(q2).sqinv.(0) in
          if q1x=q2x && q1y>q2y then bol := true
        done;
        !bol
    | BASECLAIM ->
        for x=0 to 1 do
          let q1x = elx board.solution.(q1).sqinv.(x+2)
          and q1y = elx board.solution.(q1).sqinv.(x+2)
          and q2x = elx board.solution.(q2).sqinv.(3)
          and q2y = ely board.solution.(q2).sqinv.(3) in
          if q1x=q2x && q1y>q2y then bol := true
        done;
        !bol
    | HIGHINVERSE | LOWINVERSE | VERTICAL | BASEINVERSE -> assert false


  let column_wdoe board p1 p2 =
    let joinmtrx = Array.make ((boardX+1)*(boardY+2)) false in
    assert(match board.solution.(p1).solname with
           | SPECIALBEFORE | BEFORE | AFTEREVEN | LOWINVERSE -> true
           | _ -> false);
    assert(match board.solution.(p2).solname with
           | SPECIALBEFORE | BEFORE | AFTEREVEN | LOWINVERSE -> true
           | _ -> false);
    for x=0 to board.solution.(p1).sqinvnumb - 1 do
      joinmtrx.(board.solution.(p1).sqinv.(x)) <- true
    done;
    for x=0 to board.solution.(p2).sqinvnumb - 1 do
      joinmtrx.(board.solution.(p2).sqinv.(x)) <- true
    done;
    let x = ref 0 and answer = ref true in
    while !x < boardX && !answer do
      let cnt = ref 0 in
      for y = 0 to boardY - 1 do
        if joinmtrx.(elm !x y) then incr cnt
      done;
      if !cnt land 1 = 1 then answer := false;
      incr x
    done;
    !answer


  let comp_rules board p1 p2 =
    let c1 = Rule.to_int board.solution.(p1).solname
    and c2 = Rule.to_int board.solution.(p2).solname in
    let way = rulecombo.(c1).(c2) in
    if way land 9 <> 0 then (
      board.rules.(0) <- board.rules.(0) + 1;
      not(overlap board p1 p2)
    )
    else if way land 2 <> 0 then (
      board.rules.(1) <- board.rules.(1) + 1;
      not(claimeven_below board p1 p2)
    )
    else if way land 4 <> 0 then (
      board.rules.(2) <- board.rules.(2) + 1;
      column_wdoe board p1 p2
    )
    else true


  let make_matrix board =
    let matrix = Array.make_matrix board.sp board.sp false in
    for x = 0 to board.sp - 1 do
      for y = x to board.sp - 1 do
        if comp_rules board x y then matrix.(y).(x) <- true
      done
    done;
    matrix
end

(*Problem Solver*)

let wside = [|"none";"yellow";"red"|]
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

let make_problem group solved solnumb =
  {
    group = group;
    solved = solved;
    solutions = Array.make 621 0;
    solnumb = solnumb
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
      let matrix = Adjacency.make_matrix board in
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
              let matrix = Adjacency.make_matrix board in
              oracle := problem_solver board matrix
          done
        done
      done;
    done;
  !oracle



let evaluation_function board =
  if board.turn = 1 then evaluate_black board
  else evaluate_white board


let init_board board =
  let i = ref 0 in
  (* Step one. Horizontal lines. *)
  for y=0 to boardY - 1 do
    for x=0 to boardX - 4 do
      for k = 0 to 3 do
        board.groups.(!i).(k) <- board.square.(elm (x+k) y);
        board.xplace.(!i).(k) <- x+k;
        board.yplace.(!i).(k) <- y
      done;
      incr i
    done
  done;
  (* Step two. Vertical lines *)
  for y=0 to boardY - 4 do
    for x=0 to boardX - 1 do
      for k = 0 to 3 do
        board.groups.(!i).(k) <- board.square.(elm x (y+k));
        board.xplace.(!i).(k) <- x;
        board.yplace.(!i).(k) <- y+k
      done;
      incr i
    done
  done;
  (* Step three. Diagonal (north east) lines *)
  for y=0 to boardY - 4 do
    for x=0 to boardX - 4 do
      for k = 0 to 3 do
        board.groups.(!i).(k) <- board.square.(elm (x+k) (y+k));
        board.xplace.(!i).(k) <- x+k;
        board.yplace.(!i).(k) <- y+k
      done;
      incr i
    done
  done;
  (* Step four. Diagonal (south east) lines *)
  for y=3 to boardY - 1 do
    for x=0 to boardX - 4 do
      for k = 0 to 3 do
        board.groups.(!i).(k) <- (board.square.(elm (x+k) (y-k)));
        board.xplace.(!i).(k) <- x+k;
        board.yplace.(!i).(k) <- y-k
      done;
      incr i
    done
  done;
  assert(!i = groups);

  for x=0 to boardX - 1 do
    for y=0 to boardY - 1 do
      board.square.(elm x y) := elm x y
    done
  done;

  let sqpnt = Array.make 64 0 in
  let solv = board.solvable_groups in
  for i=0 to groups - 1 do
    for j=0 to tiles - 1 do
      let p = !(board.groups.(i).(j)) in
      solv.(p).(sqpnt.(p)) <- i;
      sqpnt.(p) <- sqpnt.(p) + 1
    done
  done;

  (* Resize the solvable groups to their max number of elements: *)
  for p = 0 to 63 do solv.(p) <- Array.sub solv.(p) 0 sqpnt.(p) done;

  (* Here we set all out squares to a default value to detect problems *)
  for i=0 to 7 do
    board.square.(elm 7 i) := -1;
    board.square.(elm i 6) := -1;
  done;
  board.stack.(7) <- -1;
  for y=0 to boardY - 1 do
    for x=0 to boardX - 1 do
      board.square.(elm x y) := 0
    done
  done

(* Return a fully initialized board *)
let make_board() =
  let solv = Array.make_matrix 64 16 0
  and intg =
    {
      tgroups = Array.make groups true;
      j = 0;
      k = 0;
      mygroups = Array.make groups true
    } in
  let board = {
    wins = Array.make 2 0;
    draws = 0;
    lastguess = 0;
    bestguess = maxmen;
    lastwin = 0;
    white_lev = 0;
    black_lev = 0;
    autotest = 0;
    rules = Array.make 3 0;
    oracle_guesses = 0;
    instances = Rule.vec 10 0;
    turn = 1;
    filled = 0;
    cpu = 1;
    bbposit =0;
    groups = Array.make_matrix 69 4 0;
    xplace = Array.make_matrix 69 4 0;
    yplace = Array.make_matrix 69 4 0;
    square = Array.init ((boardX+1)*(boardY+2)) (fun i -> ref 0);
    wipesq = Array.make ((boardX+1)*(boardY+2)) 0;
    usablegroup = Array.make groups true;
    sqused = Array.make ((boardX+1)*(boardY+2)) false;
    stack = Array.make (boardX+1) 0;
    moves = Array.make maxmen 0;
    solvable_groups = solv;
    choices = Array.make maxmen 0;
    mlist = Array.make maxmen 0;
    intgp = intg;
    solution = Array.init alloc_solutions
      (fun i -> {
         valid = true;
         solname = CLAIMEVEN;  (* any will do *)
         solpoint = Array.make 2 0;
         sqinv = Array.make (2*tiles) 0;
         sqinvnumb = 0;
         solgroups = Array.make groups 0;
         solgroupsnumb = 0
       });
    sp = -1;
    problem_solved = 0;
    solused = -1;
    oracle = Array.make 2 0;
    nodes_visited = 0;
    maxtreedepth = 0;
    white_book = Array.make 1 0;
    black_book = Array.make 1 0;
    wbposit = 0;
    lastob = 0
  } in
  init_board board;
  board

(*IA*)

let blstrsize = 14
let goodmove = 16384
let badmove = -16384
let switch a = a lxor 3;;
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
  not(board.stack.(x)>=y || board.stack.(x+2)>=y || board.stack.(x+4)>=y ||
    !(board.square.(elm (x+1) y))<>side || !(board.square.(elm (x+3) y))<>side)


let check_pentas board side =
  let rec helper x y bol =
    if y<2 then (
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
  for x = 0 to groups - 1 do
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
            else score := !score +150
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
  and d_nw = diago_NW_right (move+1) (board.stack.(move)-1) d_nw_l
  and d_ne = diago_NE_right (move+1) (board.stack.(move)+1) d_ne_l
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
and and_type = 1
and or_type = 2
exception Error

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
      mutable direct : int;
      mutable symmetric : int array;
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

type dbtree =
    {
      paren : dbtree option;
      rison : dbtree option;
      leson : dbtree option;
      cpos : int array;
      mutable stacked : int;
      mutable valu : int
    }

let make_node() =
  {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = 0;
      stac = Array.make (boardX+1) 0;
      evaluated = false;
      expanded = false;
      value = 0;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.make boardX None;
      parents = Array.make boardX None;
      direct = 0;
      symmetric = Array.make boardX 0
  }


let rootnode = ref (make_node())
let streeroot = ref
  {
    parent = None;
    lson = None;
    rson = None;
    node = (make_node())
  }
let her_node_not_expanded = ref 0

let her_node_expanded = ref 0

let nodeseq = [|3;2;4;5;1;0;6|]

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
    {
      parent = Some root;
      lson = None;
      rson = None;
      node = node
    }

let bin_compare c1 c2 =
  let rec helper x =
    if x<6 then
      if c1.(x) > c2.(x) then -1
      else if c1.(x) < c2.(x) then 1
      else helper (x+1)
    else 0 in
    helper 0

let copy a =
  {
      squaree = Array.copy a.squaree;
      stac = Array.copy a.stac;
      tur = a.tur;
      child = Array.copy a.child;
      parents = Array.copy a.parents;
      evaluated = a.evaluated;
      expanded = a.expanded;
      value = a.value;
      typed = a.typed;
      proof = a.proof;
      disproof = a.disproof;
      direct = a.direct;
      symmetric = Array.copy a.symmetric
    }


let rec fast_check_node root node =
  match root with
  |Some tree ->
     (
       let cmp = bin_compare tree.node.squaree node.squaree in
       if cmp<0 then
         match tree.lson with
         |None -> (tree.lson <- Some (fast_set_node tree node);None)
         |Some lson -> fast_check_node (Some lson) node;
       else if cmp>0 then
         match tree.rson with
         |None -> (tree.rson <- Some (fast_set_node tree node);None)
         |Some rson -> fast_check_node (Some rson) node;
       else Some (tree.node)
     )
  |None -> let _ = fast_init_bin_tree node in (Some node)



let her_generate_all_children node =
  let backtrace = ref 0 in
  for x=0 to boardX-1 do
    if node.stac.(x) < boardY then
      (
        node.child.(x) <-
          Some
          {
            squaree = Array.copy node.squaree;
            stac = Array.copy node.stac;
            tur = switch node.tur;
            child = Array.make boardX None;
            parents = Array.make boardX None;
            evaluated = false;
            expanded = false;
            value = 0;
            typed = or_type;
            proof = 0;
            disproof = 0;
            direct = 0;
            symmetric = Array.make boardX 0
          };
        backtrace := x;
        match node.child.(x) with
        |None -> ()
        |Some no ->
           (
             no.squaree.(elm x no.stac.(x)) <- node.tur;
             no.stac.(x) <- no.stac.(x) + 1;
             let symm =
               {
                 squaree = Array.copy no.squaree;
                 stac = Array.copy no.stac;
                 tur = switch node.tur;
                 child = Array.make boardX None;
                 parents = Array.make boardX None;
                 evaluated = false;
                 expanded = false;
                 value = 0;
                 typed = or_type;
                 proof = 0;
                 disproof = 0;
                 direct = 0;
                 symmetric = Array.make boardX 0
               } in
             for y1=0 to boardY-1 do
               symm.squaree.(elm boardX y1) <- no.squaree.(elm boardX y1);
               for x1 = 0 to boardX-1 do
                 symm.squaree.(elm x1 y1)
                 <- no.squaree.(elm (boardX-x1-1) y1)
               done
             done;
             for x1=0 to boardX-1 do
               symm.stac.(x1) <- no.stac.(boardX-x1-1)
             done;
             if bin_compare symm.squaree no.squaree > 0 then
               node.child.(x) <- Some symm;
             let dummy = fast_check_node (Some !streeroot) no in
             match dummy with
             |None ->
                (
                  no.parents.(!backtrace) <- Some node;
                  her_node_expanded := !her_node_expanded + 1;
                  if node.typed = and_type then no.typed <- or_type
                  else no.typed <- and_type;
                  node.symmetric.(x) <- !backtrace
                )
             |Some n ->
                (
                  node.child.(x) <- dummy;
                  no.parents.(!backtrace) <- Some node;
                  her_node_not_expanded := !her_node_not_expanded + 1
                )
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
  for x=0 to boardX-1 do
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



let her_select_most_proving_node node info =
  let depth = ref 0
  and nono = ref (copy node) in
  while !nono.expanded do
    let rec helper x typ =
      if x<boardX then
        match node.child.(nodeseq.(x)) with
        |None -> helper (x+1) typ
        |Some no ->
           (
             if no.proof = node.proof && typ = or_type then x
             else if no.disproof = node.disproof && typ = and_type then x
             else helper (x+1) typ
           )
      else raise Error
    in let i = helper 0 (node.typed) in
    if !depth <> 0 then
      (
        info.bestmove <- nodeseq.(i);
        nono := match node.child.(nodeseq.(i)) with
        |None -> raise Error
        |Some n -> n
      );
    depth := !depth+1
  done;
  info.max_tree_depth <- max info.max_tree_depth (!depth+1);
  !nono


let her_develop_node node =
  node.expanded <- true;
  her_generate_all_children node;
  for i=0 to boardX-1 do
    match node.child.(i) with
      |None -> ()
      |Some no -> (
          let _ = her_evaluate no in
          her_set_proof_and_disproof_numbers no
        )
  done


let rec her_update_ancestors node =
  match node with
    |None -> ()
    |Some n -> (
        her_set_proof_and_disproof_numbers n;
        for x=0 to boardX-1 do
          her_update_ancestors n.parents.(x)
        done
      )


let her_pn_search root maxnodes info =
  info.max_tree_depth <- 1;
  info.bestmove <- her_evaluate root;
  her_set_proof_and_disproof_numbers root;
  while root.proof <> 0 && root.disproof <> 0 &&
    her_node_expanded <= maxnodes do
      let her_most_proving_node = her_select_most_proving_node root info in
        her_develop_node her_most_proving_node;
        her_update_ancestors (Some her_most_proving_node);
  done;
  if root.proof = 0 then root.value <- proved
  else if root.disproof = 0 then root.value <- disproved
  else root.value <- unknown


let heuristic_play_best board maxnodenum =
  let rootnode_ =
    {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = 0;
      stac = Array.make (boardX+1) 0;
      evaluated = false;
      expanded = false;
      value = unknown;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.make boardX None;
      parents = Array.make boardX None
        symmetric = Array.make boardX 0 ;
      direct = 0
    }
  and symmetric =
    {
      squaree = Array.make ((boardX+1)*(boardY+2)) 0;
      tur = board.turn;
      stac = Array.init (boardX+1)
        (fun x -> if x < boardX then board.stack.(boardX-1-x) else -1);
      evaluated = false;
      expanded = false;
      value = unknown;
      typed = or_type;
      proof = 0;
      disproof = 0;
      child = Array.make boardX None;
      parents = Array.make boardX None;
      symmetric = Array.make boardX 0;
      direct = 0
    } in

  her_develop_node rootnode_;
  her_develop_node symmetric;
  for y=0 to boardY-1 do
    rootnode_.squaree.(elm boardX y) <-
      !(board.square.(elm boardX y));
    symmetric.squaree.(elm boardX y) <-
      !(board.square.(elm boardX y));
    for x = 0 to boardX-1 do
      rootnode_.squaree.(elm x y) <-
        !(board.square.(elm x y));
      symmetric.squaree.(elm x y) <-
        !(board.square.(elm (boardX-1-x) y))
    done
  done;
  rootnode_.stac.(boardX) <- board.stack.(boardX);
  symmetric.stac.(boardX) <- board.stack.(boardX);
  rootnode := if bin_compare symmetric.squaree rootnode_.squaree > 0
  then symmetric else rootnode_;
  let issymm = if bin_compare symmetric.squaree rootnode_.squaree > 0
  then true else false in
  let info = {
    bestmove = -1;
    max_tree_depth = 0
  } in
  her_pn_search !rootnode maxnodenum info;
  let mymove =
    if !rootnode.value = unknown then -1
    else if !rootnode.value = disproved then -2
    else if issymm then boardX-1 - info.bestmove
    else info.bestmove in
  mymove
