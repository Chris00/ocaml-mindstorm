open Alphabeta
open Board


let print game =
  Printf.printf "\n%!";
  for i = 5 downto 0 do
    for j = 0 to 6 do
      let couleur = match (Game.get_color game i j) with
        | None -> "       "
        | Some Game.Yellow -> "Yellow"
        | Some Game.Red -> "Red   " in
      Printf.printf "%s" couleur;
    done;
    Printf.printf "\n%!"
  done

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

let spec = Arg.align [
  "--pince", Arg.Set_string bt_pincer,
  "<bt_address>set the bluetooth address of the brick which uses the pincer";
  "--scan", Arg.Set_string bt_scan,
  "<bt_address>set the bluetooth address of the brick which uses the scan";
  "--human_first", Arg.Clear if_computer,
  " set the humas as first player"]
let () =
  Arg.parse spec (fun _ -> raise (Arg.Bad "no anonymous arg"))
    "run_connect4 <option>"

module Conn =
struct
  let r = Robot.make()
  let conn_pincer = Mindstorm.connect_bluetooth !bt_pincer
  and conn_scan = Mindstorm.connect_bluetooth !bt_scan
  and fst_computer = !if_computer
end

module P = Pincer.Run(Conn)
module S = ScanPiece2.Run(Conn)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec step game color col =
  if col <> -1 then
    (
        Game.move game col (Game.color_invers color);
        Board.add_piece_to_board (match (Game.color_invers color) with
                                |Game.Yellow -> Graphics.yellow
                                |Game.Red -> Graphics.red) col
    );
  (*on verifie que le jeu n'est pas gagné ou match nul*)
  if (col = -1) ||
    (not (Game.is_winning game col) && not (Game.is_draw game)) then
      (
        (*on cherche la colonne a jouer*)
        let _, col_to_play = alphabeta game color 8 heuristic in
        Printf.printf "%i\n%!" col_to_play;
        Game.move game col_to_play color;
        Board.add_piece_to_board (match color with
                                  |Game.Yellow -> Graphics.yellow
                                  |Game.Red -> Graphics.red) col_to_play;
        (*la pince va mettre la piece dans la colonne a jouer
          et on va scanner pour voir si le joueur a joue*)
        if not(Game.is_winning game col_to_play) && not(Game.is_draw game)
        then
          P.put_piece col_to_play
            (fun () -> S.scan col_to_play (fun c -> step game color c))
        else
          (
            Printf.printf"c fini, on stoppe après avoir ajouter la piece\n%!";
            P.put_piece col_to_play
              (fun () -> S.return_init_pos Board.close_when_clicked);
            Printf.printf"LE ROBOT GAGNE\n%!"
          )
      )
  else S.return_init_pos S.stop
let () =
  Board.gameboard ();
  (* Board.close_when_clicked(); *)
  let game = Game.make() in
  if Conn.fst_computer then step game Game.Yellow (-1)
  else S.scan (-1) (fun c -> step game Game.Yellow c);
  Robot.run Conn.r

