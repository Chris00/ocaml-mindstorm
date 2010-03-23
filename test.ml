let bb = Evaluate.make_board() in
for i=2 to 5 do
Evaluate.makemove bb i;
Evaluate.makemove bb i;
done;

Printf.printf "%i%!" (Evaluate.heuristic_play_best bb (ref 1000000))  
