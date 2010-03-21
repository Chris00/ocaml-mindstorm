open Alphabeta

let conn1, conn2, fst_computer =
  if Array.length Sys.argv < 4 then (
    Printf.printf "%s <bluetooth addr><bluetooth addr><if_computer>\n"
      Sys.argv.(0);
    exit 1;
  );
  Mindstorm.connect_bluetooth Sys.argv.(1),
  Mindstorm.connect_bluetooth Sys.argv.(2),
  bool_of_string Sys.argv.(3)

module P = Pincer.Run(struct let conn2 = conn2 end)
module S = ScanPiece.Run(struct let conn = conn1 end)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec step game color col =
  (*on verifie que le jeu n'est pas gagné ou match nul*)
  if not (Game.is_winning game col) && not (Game.is_draw game) then
    (*on cherche la colonne a jouer*)
    let _,col_to_play = alphabeta game color neg_infinity infinity 8 h
    in
    Game.move game col_to_play color;
    (*la pince va mettre la piece dans la colonne a jouer
      et on va scanner pour voir si le joueur a joue*)
    if not (Game.is_winning game col_to_play) || not (Game.is_draw game) then
      P.put_piece col_to_play (S.scan col_to_play (fun c -> step game color c))
    else P.put_piece col_to_play (S.stop ())

let () =
  let game = Game.make() in
  if fst_computer then step game Game.Yellow 0
  else S.scan (-1) (fun c -> step game Game.Yellow c)


