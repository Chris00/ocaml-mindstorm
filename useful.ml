let print game =
  Printf.printf "\n";
  for i = 5 downto 0 do
    for j = 0 to 6 do
      let couleur = match (Game.get_color game i j) with
        | None -> "       "
        | Some Game.Yellow -> " Yellow"
        | Some Game.Red -> " Red   " in
      Printf.printf "%s" couleur;
    done;
    Printf.printf"\n"
  done;
  Printf.printf"--------------------------------------------------\n"


