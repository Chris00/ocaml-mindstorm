open Printf;;
(*
  let print game =
  printf "\n";
  for i = 5 downto 0 do
  for j = 0 to 6 do
      let couleur = match (Game.get_color game i j) with
        | None -> " "
        | Some Game.Yellow -> "Y"
        | Some Game.Red -> "R" in
      printf "%s" couleur;
    done;
    printf "\n"
  done

let jeu = Game.make();;
Game.move jeu 3 Game.Red;;
Game.move jeu 3 Game.Yellow;;
Game.move jeu 3 Game.Red;;
Game.move jeu 2 Game.Yellow;;
Game.move jeu 4 Game.Red;;
Game.move jeu 4 Game.Yellow;;
Game.move jeu 3 Game.Red;;
Game.move jeu 2 Game.Yellow;;
Game.move jeu 2 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 2 Game.Red;;
Game.move jeu 1 Game.Yellow;;
Game.move jeu 1 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 1 Game.Yellow;;
Game.move jeu 1 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 5 Game.Yellow;;
Game.move jeu 5 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 1 Game.Red;;
Game.move jeu 2 Game.Yellow;;
Game.move jeu 1 Game.Red;;
Game.move jeu 3 Game.Yellow;;
*)
(*print jeu;;

let a = alphabeta jeu Game.Red 8 h;;
Printf.printf "%f" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;*)


(*let a = alphabeta jeu Game.Red (neg_infinity) infinity 2 h;;
Printf.printf "%f" (fst a);;
Printf.printf "\n";;
Printf.printf "%i" (snd a);;*)

(*let jeu = Game.make();;*)
(*Game.move jeu 0 Game.Red;
Game.move jeu 1 Game.Yellow;
Game.move jeu 1 Game.Red;
Game.move jeu 0 Game.Yellow;
Game.move jeu 0 Game.Red;
Game.move jeu 1 Game.Yellow;
Game.move jeu 1 Game.Red;
Game.move jeu 0 Game.Yellow;
(*Game.move jeu 0 Game.Yellow;*)
(*Game.move jeu 1 Game.Red;*)
Game.move jeu 3 Game.Red;
Game.move jeu 2 Game.Yellow;
Game.move jeu 2 Game.Red;
(*Game.move jeu 3 Game.Yellow;*)
(*Game.move jeu 0 Game.Red;*)
Game.move jeu 2 Game.Yellow;
(*Game.move jeu 2 Game.Red;*)
(*Game.move jeu 3 Game.Yellow;*)
(*Game.move jeu 3 Game.Red;*)
(*Game.move jeu 2 Game.Yellow;*)
Game.move jeu 5 Game.Red;
Game.move jeu 6 Game.Yellow;
Game.move jeu 6 Game.Red;
Game.move jeu 5 Game.Yellow;
Game.move jeu 5 Game.Red;
Game.move jeu 6 Game.Yellow;
(*Game.move jeu 6 Game.Red;*)
(*Game.move jeu 5 Game.Yellow;*)
(*Game.move jeu 5 Game.Red;*)
(*Game.move jeu 6 Game.Yellow;*)
Game.move jeu 4 Game.Red;
Game.move jeu 4 Game.Yellow;
Game.move jeu 4 Game.Red;;
(*Game.move jeu 4 Game.Red;;*)
(*Game.move jeu 4 Game.Red;;*)
(*Game.move jeu 3 Game.Yellow;;*)
(*Game.move jeu 4 Game.Red;;*)
*)


(*Game.move jeu 2 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 2 Game.Yellow;;
Game.move jeu 2 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 0 Game.Yellow;;
Game.move jeu 2 Game.Red;;*)
(*Game.move jeu 0 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 2 Game.Yellow;;
Game.move jeu 0 Game.Red;;
Game.move jeu 0 Game.Yellow;;*)
(*
  let () =
  print jeu;
  printf "%!";
  let cost, col =
  Alphabeta.alphabeta jeu Game.Yellow 9 Alphabeta.heuristic in
*)

