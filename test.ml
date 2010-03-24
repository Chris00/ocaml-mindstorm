let () =
  let j1 = ref 0 and j2 = ref 0 and nul = ref 0 in
  for i=1 to 300 do
    Printf.printf "%s%i%!" "Partie num : " i;
    let bb = Gamemem.make_board() in
    Gamemem.initboard bb;
    Random.init(i*i*i);
    let c = Random.int(6) in
    Gamemem.makemove bb c;
    let d = Random.int(6) in
    Gamemem.makemove bb d;
    let e = Random.int(6) in
    Gamemem.makemove bb e;
    let f = Random.int(6) in
    Gamemem.makemove bb f;
    while Gamemem.get_game_result bb = Gamemem.UNDECIDED do
      let _, b = Alphabetamem.alphabeta bb 7 Gamemem.groupeval in
      Gamemem.makemove bb b
    done;
    if Gamemem.get_game_result bb <> Gamemem.WIN then
      (
        nul := !nul+1;
        Printf.printf "%s%!" " est nulle\n"
      )
    else if bb.Gamemem.turn = 1 then
      (
        j2 := !j2+1;
        Printf.printf "%s%!" " est gagne par J2\n"
      )
    else
      (
        j1 := !j1+1;
        Printf.printf "%s%!" " est gagnee par J1\n"
      )
  done;
  Printf.printf "%i  %i  %i" !j1 !j2 !nul
