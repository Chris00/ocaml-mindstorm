let () =
for j=0 to 6 do
let g = Board.create_game() in
  ignore(Board.makemove g j);
  Printf.printf "J = %d\n%!" j;
  let i = ref 2 in
  while Board.get_game_result g = -1 do   
    let x = Ia.move_for g in
    Printf.printf "Move %d : %d\n%!" !i x;
   ignore(Board.makemove g x);
    i := !i + 1;
  done;
  Printf.printf "Gagné par J%d\n\n\n%!" (Board.get_game_result g)
done
