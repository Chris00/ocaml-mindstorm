open Graphics
open Printf
open Game


(*rem:les colonnes du jeu sont numérotés de 0 à 6 et les lignes de 0 à 5*)
let w = 1000 and h = 720

(*methode qui colorie les cercles dans la couleur color*)
let color_circle color x_center y_center =
  let r_circle = 7*(min (w/9) (h/9))/16 in
  set_color color;
  fill_circle x_center y_center r_circle;
  set_color black;
  draw_circle x_center y_center r_circle;;

(*methode qui trace un disque de la couleur color, en position (raw, line)
  raw va de 0 à 6 et line de 0 à 5*)
let color_circle2 color col line =
  color_circle color (w/6 + col*w/9) (2*h/9 + h/18 + line*h/9);;



let rec gameboard current_game =
  open_graph(sprintf " %ix%i" 400 200);
  set_window_title("choose players");

  (*bouton nombre de joueur*)
  set_font "12x24kana";
  let (n_x1, n_y1) = (text_size "Demo")
  and (n_x2, n_y2) = (text_size "1 player")
  and (n_x3, n_y3) = (text_size "2 players") in

  let x1 = (400-n_x1-n_x2-n_x3)/4 and y1 = (200-n_y1)/2 in
  moveto x1 y1;
  draw_string "Demo";
  draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
  set_line_width 3;
  draw_rect (x1-8) (y1-7) (n_x1+16) (n_y1+14);

  set_line_width 1;
  let x2 = 2*x1+n_x1 and y2 = (200-n_y2)/2 in
  moveto x2 y2;
  draw_string "1 player";
  draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
  set_line_width 3;
  draw_rect (x2-8) (y2-7) (n_x2+16) (n_y2+14);

  set_line_width 1;
  let x3 = x2+x1+n_x2 and y3 = (200-n_y3)/2 in
  moveto x3 y3;
  draw_string "2 players";
  draw_rect (x3-4) (y3-3) (n_x3+8) (n_y3+6);
  set_line_width 3;
  draw_rect (x3-8) (y3-7) (n_x3+16) (n_y3+14);
  synchronize();

  (*Méthode qui crée les deux joueurs*)
  let rec choose_player () =
    (*attente du clic sur un des boutons*)
    let clic = wait_next_event [Button_down; Button_up] in
    if button_down() then
      (
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x1-8)) && (pos_x<(x1+n_x1+8))
             && (pos_y>(y1)) && (pos_y<(y1+n_y1+7)) )
        then
          (
            set_color white;
            draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
            choose_player();
          )

        else if ( (pos_x>(x2-8)) && (pos_x<(x2+n_x2+8))
                  && (pos_y>(y2)) && (pos_y<(y2+n_y2+7)) )
        then
          (
            set_color white;
            draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
            choose_player();
          )

        else if ( (pos_x>(x3-8)) && (pos_x<(x3+n_x3+8))
                  && (pos_y>(y3)) && (pos_y<(y3+n_y3+7)) )
        then
          (
            set_color white;
            draw_rect (x3-4) (y3-3) (n_x3+8) (n_y3+6);
            choose_player();
          )

        else choose_player ();
      )
    else
      (
        (*on a appuye sur Demo*)
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x1-8)) && (pos_x<(x1+n_x1+8))
             && (pos_y>(y1)) && (pos_y<(y1+n_y1+7)) )
        then
          (
            set_color black;
            set_line_width 1;
            draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
            (Computer, Computer)
          )

        (*On a appuyé sur 1 joueur*)
        else if ( (pos_x>(x2-8)) && (pos_x<(x2+n_x2+8))
                  && (pos_y>(y2)) && (pos_y<(y2+n_y2+7)) )
        then
          (
            set_color black;
            set_line_width 1;
            draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
            (Human, Computer);
          )

        else if ( (pos_x>(x3-8)) && (pos_x<(x3+n_x3+8))
                  && (pos_y>(y3)) && (pos_y<(y3+n_y3+7)) )
        then
          (
            set_color black;
            set_line_width 1;
            draw_rect (x3-4) (y3-3) (n_x3+8) (n_y3+6);
            (Human, Human);
            (*renvoie au choix de la couleur*)
          )
        else choose_player ();
      ) in
  let players = choose_player () in
  close_graph();

  (*fenetre pour le choix des couleurs*)
  open_graph(sprintf " %ix%i" 400 200);
  set_window_title("choose color");

  set_font "12x24kana";
  let (n_x1, n_y1) = (text_size "Yellow")
  and (n_x2, n_y2) = (text_size "Red") in

  let x1 = (400-n_x1-n_x2)/3 and y1 = (200-n_y1)/2 in
  moveto x1 y1;
  draw_string "Yellow";
  draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
  set_line_width 3;
  draw_rect (x1-8) (y1-7) (n_x1+16) (n_y1+14);

  set_line_width 1;
  let x2 = 2*x1+n_x1 and y2 = (200-n_y2)/2 in
  moveto x2 y2;
  draw_string "Red";
  draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
  set_line_width 3;
  draw_rect (x2-8) (y2-7) (n_x2+16) (n_y2+14);
  synchronize();

  let rec choose_color() =
    (*attente du clic sur un des boutons*)
    let clic = wait_next_event [Button_down; Button_up] in
    if button_down() then
      (
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x1-8)) && (pos_x<(x1+n_x1+8))
             && (pos_y>(y1)) && (pos_y<(y1+n_y1+7)) )
        then
          (
            set_color white;
            draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
            choose_color()
          )

        else if ( (pos_x>(x2-8)) && (pos_x<(x2+n_x2+8))
                  && (pos_y>(y2)) && (pos_y<(y2+n_y2+7)) )
        then
          (
            set_color white;
            draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
            choose_color()
          )
        else choose_color()
      )
    else
      let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
      (*on a appuyé sur jaune*)
      if ( (pos_x>(x1-8)) && (pos_x<(x1+n_x1+8))
           && (pos_y>(y1)) && (pos_y<(y1+n_y1+7)) )
      then
        (
          set_color black;
          set_line_width 1;
          draw_rect (x1-4) (y1-3) (n_x1+8) (n_y1+6);
          (Yellow, Red)
        )

      (*On a appuyé sur 1 joueur*)
      else if ( (pos_x>(x2-8)) && (pos_x<(x2+n_x2+8))
                && (pos_y>(y2)) && (pos_y<(y2+n_y2+7)) )
      then
        (
          set_color black;
          set_line_width 1;
          draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
          (Red, Yellow)
        )
      else choose_color()
  in
  let color = choose_color() in
  close_graph();

  let play1 = {player = fst players; pion = fst color; fst_player = true}
  and play2 = {player = snd players; pion = snd color; fst_player = false} in

  open_graph(sprintf " %ix%i" w h );
  set_window_title("Connect Four");

  (*creation du bouton nouvelle partie*)
  set_font "12x24kana";
  let n_x = fst (text_size "New Part")
  and n_y = snd (text_size "New Part") in
  let x = (w/2 - n_x/2) and y = (h/9) in
  moveto x y;
  draw_string "New Part";
  draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
  set_line_width 3;
  draw_rect (x-8) (y-7) (n_x+16) (n_y+14);

  (*creation du quadrillage*)
  set_color blue;
  let x_rect = w/9 and y_rect = 2*h/9 in
  let w_rect = 7*w/9 and h_rect = 6*h/9 in
  fill_rect x_rect y_rect w_rect h_rect;
  set_color black;
  set_line_width 3;
  draw_rect x_rect y_rect w_rect h_rect;

  set_line_width  2;
  let worh = min (w/9) (h/9) in
  let r_circle = 7*worh/16 in
  synchronize();

  (*permet de représenter le plateau de jeu en court de partie*)
  for i=0 to 6 do
    for j=0 to 5 do
      set_color (match current_game.tab.(i).(j) with
                 | Empty -> white
                 | Red -> red
                 | Yellow -> yellow);

      let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
      fill_circle x_circle y_circle r_circle;
      set_color black;
      draw_circle x_circle y_circle r_circle;
    done;
  done;
  synchronize();

  (*permet de jouer au puissance4 en passant par l'interface graphique*)
  let rec part player1 player2 =
    let clic = wait_next_event [Button_down; Button_up] in
    if button_down() then
      (
        (*si on clique sur le bouton nouvelle partie*)
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
             && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
        then
          (
            set_color white;
            draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
            part player1 player2
          )
            (*si on clique dans une colonne du jeu*)
        else if ((pos_x>(w/9)) && (pos_x<(8*w/9)) &&
                   (pos_y>(2*h/9)) && (pos_y<(8*h/9))) then
          (
            let col = pos_x/(w/9)-1 in
            (*si la colonne n'est pas pleine, on peut encore y jouer*)
            if (current_game.tab.(col).(5) = Empty) then
              (
                move current_game col player1;
                let action = List.hd current_game.list_event in
                if (player1.pion = Red) then
                  color_circle2 red (action.col) (action.line)
                else color_circle2 yellow (action.col) (action.line);
                set_color black;
                moveto 10 10;

                let win = isWin current_game in
                if win = true then
                  (
                    open_graph(sprintf " %ix%i" 400 200);
                    set_window_title("and the winner is...");

                    let winner =
                      (if action.piece = Red then "Le joueur ROUGE gagne!!!"
                       else "Le joueur JAUNE gagne!!!") in

                    let (n_xw, n_yw) = (text_size winner)
                    and (n_xok, n_yok) = (text_size "ok") in

                    let x1 = (400 - n_xw)/2
                    and ytemp = (200 - n_yw - n_xok)/3 in
                    let y1 = 2*ytemp + n_yok in
                    moveto x1 y1;
                    draw_string winner;

                    let x2 = (400 - n_xok)/2 and y2 = ytemp in
                    moveto x2 y2;
                    draw_string "OK";

                    set_color black;
                    set_line_width 1;
                    draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
                    set_line_width 3;
                    draw_rect (x2-8) (y2-7) (n_x2+16) (n_y2+14);

                    let rec push_ok () =
                      let st = wait_next_event [Button_down; Button_up] in
                      if button_down() then
                        (
                          let pos_x = st.mouse_x and pos_y = st.mouse_y in
                          if ( (pos_x> (x2-8)) && (pos_x<(x2+n_xok+8))
                               && (pos_y>(y2-7)) && (pos_y<(y2+n_yok+7)) )
                          then
                            (
                              set_color white;
                              draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
                              push_ok();
                            )
                          else push_ok ()
                        )
                      else
                        (
                          let pos_x = st.mouse_x and pos_y = st.mouse_y in
                          if ( (pos_x> (x2-8)) && (pos_x<(x2+n_xok+8))
                               && (pos_y>(y2-7)) && (pos_y<(y2+n_yok+7)) )
                          then
                            (
                              set_color black;
                              set_line_width 1;
                              draw_rect (x2-4) (y2-3) (n_x2+8) (n_y2+6);
                              close_graph();
                              new_part current_game;
                              gameboard current_game;
                            )
                          else push_ok();
                        )
                    in push_ok()
                  )
                else part player2 player1;
              )
            else part player1 player2;
          )
      )
        (*il recommence une nouvelle partie car le bouton New Part a été
          pressé*)
    else
      (
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
             && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
        then
          (
            close_graph();
            new_part current_game;
            gameboard current_game;
          )
        else part player1 player2;
      );
    part player1 player2 in
  part play1 play2
;;


(*let st = wait_next_event [Button_down] in ();;*)
let () =
  gameboard(Game.make())
