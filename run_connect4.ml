open Alphabeta

(*module Conn =
struct
  let r = Robot.make()

  let conn_pincer, conn_scan, fst_computer =
    if Array.length Sys.argv < 4 then (
      Printf.printf "%s <bluetooth addr><bluetooth addr><if_computer>\n"
        Sys.argv.(0);
      exit 1;
    );
    Mindstorm.connect_bluetooth Sys.argv.(1),
    Mindstorm.connect_bluetooth Sys.argv.(2),
    bool_of_string Sys.argv.(3)

end*)

let bt_pincer = ref "00:16:53:0C:84:49"
and bt_scan = ref "00:16:53:0A:F3:3C"
and if_computer = ref true

let spec = Arg.align ["--pince", Arg.Set_string bt_pincer,
                      "<bt_address>set the bluetooth address of the brick
which uses the pincer";
                      "--scan", Arg.Set_string bt_scan,
                      "<bt_address>set the bluetooth address of the brick
which uses the scan";
                      "--computer_first", Arg.Set if_computer,
                      " set first player"]
let () = Arg.parse spec (fun _ -> raise (Arg.Bad "no anonymous arg"))
  "run_connect4 <option>"

module Conn =
struct
  let r = Robot.make()
  let conn_pincer, conn_scan, fst_computer =
    Mindstorm.connect_bluetooth !bt_pincer,
    Mindstorm.connect_bluetooth !bt_scan,
    !if_computer
end

module P = Pincer.Run(Conn)
module S = ScanPiece.Run(Conn)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec step game color col =
  if col <> -1 then Game.move game col (Game.color_invers color);
  (*on verifie que le jeu n'est pas gagné ou match nul*)
  if not (Game.is_winning game col) && not (Game.is_draw game) then
    (
      (*on cherche la colonne a jouer*)
      Printf.printf "calcul de la colonne a jouer\n%!";
      let _, col_to_play = alphabeta game color 8 h in
      Printf.printf "%i\n%!" col_to_play;
      Game.move game col_to_play color;
      (*la pince va mettre la piece dans la colonne a jouer
        et on va scanner pour voir si le joueur a joue*)
      if not(Game.is_winning game col_to_play) && not(Game.is_draw game) then
        P.put_piece col_to_play
          (fun () -> S.scan col_to_play (fun c -> step game color c))
      else
        (
          Printf.printf"c fini, on stoppe après avoir ajouter la piece\n%!";
          P.put_piece col_to_play P.stop)
    )
let () =
  let game = Game.make() in
  if Conn.fst_computer then step game Game.Yellow 0
  else S.scan (-1) (fun c -> step game Game.Yellow c);
  Robot.run Conn.r

