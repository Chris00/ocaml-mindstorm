open Printf

let test game =
  printf "Move for [%s]: %!" (String.concat "; " (List.map string_of_int game));
  match move_for game with
  | None -> printf "Victory or past victory\n"
  | Some c -> printf "%i\n" c

let () =
  test [4;4;4;4;4;2];
  test [4;4;3;3;2;3;1;1]

(* Local Variables: *)
(* compile-command: "make test_velena.com" *)
(* End: *)
