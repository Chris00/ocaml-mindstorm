let bb = Evaluate.make_board() in
  for i=2 to 3 do
    Evaluate.makemove bb 1;
    Evaluate.makemove bb 2;
  done;
  Printf.printf "%i" (Evaluate.heuristic_play_best bb (ref 100000));
