let g = Structure.create_game() in
    Structure.initboard g;
    let i = ref 1 in
    while Structure.get_game_result g = -1 do   
        let x = Ia.move_for g in
        Printf.printf "Move %d : %d\n%!" !i (x+1);
        Structure.makemove g x;
        i := !i + 1;
    done;
