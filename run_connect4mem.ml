open Printf
open Board

let bt_pincer = ref "00:16:53:0C:84:49"
and bt_scan = ref "00:16:53:0A:F3:3C"
and if_computer = ref true

let spec = Arg.align [
  "--pince", Arg.Set_string bt_pincer,
  "<bt_address> set the bluetooth address of the brick which uses the pincer";
  "--scan", Arg.Set_string bt_scan,
  "<bt_address> set the bluetooth address of the brick which uses the scan";
  "--human_first", Arg.Clear if_computer,
  " set human as first player"]
let () =
  Arg.parse spec (fun _ -> raise (Arg.Bad "no anonymous arg"))
    "run_connect4 <options>"

module Conn =
struct
  let r = Robot.make()
  let conn_pincer = Mindstorm.connect_bluetooth !bt_pincer
  and conn_scan = Mindstorm.connect_bluetooth !bt_scan
  and fst_computer = !if_computer
end

module P = Pincer.Run(Conn)
module S = ScanPiece.Run(Conn)

let move_for game gamemem =
  let if_velana_fails () =
    let _, c = Alphabetamem.alphabeta gamemem 9 Gamemem.groupeval
    in Some c in
  match Velena.move_for (List.rev game) if_velana_fails with
  | Some c -> c
  | None -> assert false

let is_won_or_draw game gamemem =
  let if_velana_fails () =
    if Gamemem.get_game_result gamemem = Gamemem.WIN
      || Gamemem.draw gamemem then None
    else Some(-1) (* value does not matter *) in
  Velena.move_for (List.rev game) if_velana_fails = None

let move game col = ignore(Gamemem.makemove game col)

(*si fst_player est vrai, ca veut dire que c'est a l'ordi de commencer,
  on lance donc alphabeta puis la pince et enfin le scan*)
let rec computer_play game gamemem =
  (* On cherche la colonne a jouer *)
  printf "COMPUTER\n%!";
  Board.write_player_turn Graphics.yellow;
  printf "  Joue colonne : %!";
  let col_to_play = move_for !game gamemem in
  printf "%i\n%!" col_to_play;
  game := col_to_play :: !game;
  move gamemem col_to_play;
  (*la pince va mettre la piece dans la colonne a jouer
    et on va scanner pour voir si le joueur a joue*)
  if is_won_or_draw !game gamemem then (
    printf "  Le robot à gagné (mettre le dernier pion)\n%!";
    (* if Gamemem.draw game then Board.draw() *)
    (* else Board.yellow_success(); *)
    P.put_piece col_to_play
      (fun () ->
         Board.add_piece_to_board Graphics.yellow col_to_play;
         (* if Gamemem.draw game then Board.draw() *)
         (* else Board.yellow_success(); *)
         S.return_init_pos Board.close_when_clicked)
  )
  else (
    printf "  Ajoute pièce en col %i\n%!" col_to_play;
    P.put_piece col_to_play begin fun () ->
      printf "  Pièce ajoutée !\n";
      Board.add_piece_to_board Graphics.yellow col_to_play;
      S.add_piece col_to_play;
      human_play game gamemem
    end
  )

and human_play game gamemem =
  Printf.printf "HUMAN\n%!";
  Board.write_player_turn Graphics.red;
  printf "  scan\n%!";
  S.scan begin fun col ->
    printf "  Trouvé pièce en col %i\n%!" col;
    game := col :: !game;
    move gamemem col;
    Board.add_piece_to_board Graphics.red col;
    printf "  test si fini\n%!";
    (* On verifie que le jeu n'est pas gagné ou match nul *)
    if is_won_or_draw !game gamemem then (
      (* if Gamemem.draw game then Board.draw() *)
      (* else Board.red_success(); *)
      printf "  L'HUMAIN A GAGNE\n%!";
      S.return_init_pos Board.close_when_clicked
    )
    else (
      computer_play game gamemem
    )
  end

let () =
  Board.gameboard ();
  let game = ref [] in
  let gamemem = Gamemem.make_board() in
  Gamemem.initboard gamemem;
  (if Conn.fst_computer then computer_play else human_play) game gamemem;
  Robot.run Conn.r

