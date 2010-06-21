let () =
for j=0 to 6 do
let g = Structure.create_game() in
  Structure.initboard g;
  ignore(Structure.makemove g j);
  Printf.printf "J = %d\n%!" j;
  let i = ref 2 in
  while Structure.get_game_result g = -1 do   
    let x = Ia.move_for g in
    Printf.printf "Move %d : %d\n%!" !i x;
   ignore(Structure.makemove g x);
    i := !i + 1;
  done;
  Printf.printf "Gagné par J%d\n\n\n%!" (Structure.get_game_result g)
done
