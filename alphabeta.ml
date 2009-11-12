open Tree

(*alpha<beta*)
let rec alphabeta p alpha beta =
  let value = 0 in
  if isLeaf p then value = p.data
  else if node_min p
  then
    (
      value = infinity;
      for i=0 to (Array.length p.subtree) do
        value = min(value, alphabeta p.subtree.(i) alpha beta);
        if alpha > value then value;
        beta = min(beta, value);
      done;
    )
  else
    (
      value=neg_infinity;
      for i=0 to (Array.length p.subtree) do
        value = max(value, alphabeta p.subtree.(i) alpha beta);
        if value > beta then value;
        alpha = max(alpha, value)
      done;
    );
  value;;
