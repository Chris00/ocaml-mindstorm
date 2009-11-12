type tree =
    {
      data : int option;
      height : int;
      subtree : (tree option) array
    }

let isLeaf node =
  let tab = node.subtree in
  let rec helper pos bool=
    if (pos < Array.length tab) && (bool) then
      match tab.(pos) with
      |Some i -> helper (pos+1) false
      |None -> helper (pos+1) true
    else bool in
  helper 0 true;;

(*dans le cas ou le joueur 1 commence*)
let node_min node =
  node.height mod 2 = 0;;
