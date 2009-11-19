(*Algorithme min max*)
open Tree

let rec min_max tree =
  if (not (isLeaf tree)) then
    match tree with
    |Some t ->
       let son = t.subtree in
       let n = Array.length son in
       for i=0 to n-1 do
         min_max son.(i)
       done;

       if (t.height mod 2 = 0) then t.data <- max son
       else t.data <- min son
  else ()
;;

(*Algorithme Alpha Beta*)
