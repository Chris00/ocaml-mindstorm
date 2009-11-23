(*Un arbre c'est une donnée, des sous-arbres et une hauteur d'arbre*)
type tree =
    {
      mutable data : float option;
      mutable height : int;
      subtree : (tree option) array
    }

(*Création d'un noeud vide*)
let make_empty () =
  {
    data = None;
    height = 0;
    subtree = Array.make 7 None
  }
;;

(*Création d'une feuille*)
let make_leaf data =
  {
    data = data;
    height = 0;
    subtree = Array.make 7 None
  }
;;

let get_data tree =
  match tree with
  |Some t -> t.data
  |None -> None
;;

(*dit si un noeud est une feuille*)

let isLeaf node =
  match node with
  |Some node ->
     let tab = node.subtree in
     let rec leaf pos bool=
       if (pos < Array.length tab) && (bool) then
         match tab.(pos) with
         |Some i -> leaf (pos+1) false
         |None -> leaf (pos+1) true
       else bool in
     leaf 0 true
  |None -> true
;;

(*calcul le max des fils d'un noeud*)
let max_sons tab_node =
  let rec max pos max_current =
    if (pos >= Array.length tab_node) then Some max_current
    else match tab_node.(pos) with
    |Some root ->
       (
         match root.data with
         |Some max_possible ->
            if (max_current < max_possible) then max (pos+1) max_possible
            else max (pos+1) max_current
         |None -> max (pos+1) max_current;
       )
    |None -> max (pos+1) max_current
  in max 0 neg_infinity
;;

(*calcul le min des fils d'un noeud*)
let min_sons tab_node =
  let rec min pos min_current =
    if (pos >= Array.length tab_node) then Some min_current
    else match tab_node.(pos) with
    |Some root ->
       (
         match root.data with
         |Some min_possible ->
            if (min_current > min_possible) then min (pos+1) min_possible
            else min (pos+1) min_current
         |None -> min (pos+1) min_current;
       )
    |None -> min (pos+1) min_current
  in min 0 infinity
;;


(*
(*dans le cas ou le joueur 1 commence*)
let node_min node =
  node.height mod 2 = 0
;;
*)

(*TEST*)
let a = make_leaf (Some 5.);;
make_empty ();;
let t = {data = Some 5.;
         height = a.height+1;
         subtree = a.subtree}
;;
(*
let a = {data = Some 1.; height=1; subtree=[|None; None|]};;
let b = {data = Some 2.; height = 1; subtree = [|None; None|]};;
let c = {data = Some 3.; height = 1; subtree = [|None; None|]};;
let e = {data = Some 4.; height = 0; subtree = [| Some a; Some b; Some c|]};;
isLeaf e;;
isLeaf a;;
max a.subtree;;
max e.subtree;;
min a.subtree;;
min e.subtree;;
e.subtree.(0);;
*)
