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

       if (t.height mod 2 = 0) then t.data <- max_sons son
       else t.data <- min_sons son
;;

(*Algorithme Alpha Beta*)
let rec alpha_beta tree alpha beta =
  if (isLeaf tree) then get_data tree
  else
    match tree with
    |Some t ->
       let sons = t.subtree in
       if (t.height mod 2 = 0) then (*noeud max*)

         let rec cut_beta alpha beta value i =
           if i < (Array.length sons) then
             let temp = max value (alpha_beta sons.(i) alpha beta) in
             if temp > beta then temp
             else cut_beta (max alpha temp) beta temp (i+1)
           else value in
         cut_beta alpha beta (Some neg_infinity) 0

       else
         let rec cut_alpha alpha beta value i =
           if i < (Array.length sons) then
             let temp = min value (alpha_beta sons.(i) alpha beta) in
             if alpha > temp then temp
             else cut_alpha alpha (min beta temp) temp (i+1)
           else value in
         cut_alpha alpha beta (Some infinity) 0 (*neud min*) 
;;

(*
let rec alpha_beta tree alpha beta =
  let value = ref 0. in
  if (isLeaf tree) then
    (
      match (tree.data) with
      |Some d -> d
      |None -> !value
    )
  else
    (
      match tree with
      |Some t ->
         (
           let sons = t.subtree in
           let n = Array.length sons in
           if (t.height mod 2 = 0) then
             (
               value := neg_infinity;
               for i = 0 to n-1 do
                 value := max !value (alpha_beta (sons.(i))
                                        (max alpha !value) beta);
                 if (!value > beta) then !value
               done
             )
           else
             (
               value := infinity;
               for i = 0 to n-1 do
                 value := min !value (alpha_beta (sons.(i))
                                        alpha (min beta !value));
                 if (alpha > !value) then !value
               done
             );
           value;
         );
    )
;;
*)
