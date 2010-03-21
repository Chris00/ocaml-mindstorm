let plus couple1 couple2 =
  (fst couple1+ fst couple2, snd couple1+ snd couple2)

let max_tab tab =
  let n = Array.length tab in
  let rec max max_current i_max i =
    if i > n-1 then (max_current, i_max)
    else if tab.(i) > max_current then max tab.(i) i (i+1)
    else max max_current i_max (i+1)
  in max 0. 0 0

let min_tab tab =
  let n = Array.length tab in
  let rec min min_current i_min i =
    if i > n-1 then (min_current, i_min)
    else if tab.(i) < min_current then min tab.(i) i (i+1)
    else min min_current i_min (i+1)
  in min 0. 0 0

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


