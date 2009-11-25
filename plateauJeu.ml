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

(*methode qui trace un disque dans la couleur color, en position (raw, line) raw va de 0 à 6 et line de 0 à 5*)
let color_circle2 color raw line =
  color_circle color (w/6 + raw*w/9) (2*h/9 + h/18 + line*h/9);;

let gameboard current_game =
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
  let x_rect = w/9 and y_rect = 2*h/9
                    and w_rect = 7*w/9 and h_rect = 6*h/9 in
  fill_rect x_rect y_rect w_rect h_rect;
  set_color black;
  set_line_width 3;
  draw_rect x_rect y_rect w_rect h_rect;

  set_line_width  2;
  let worh = min (w/9) (h/9) in
  let r_circle = 7*worh/16 in

  if (List.length current_game.list_event = 0) then
    for i=0 to 6 do
      for j=0 to 5 do
        set_color white;
        let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
        fill_circle x_circle y_circle r_circle;
        set_color black;
        draw_circle x_circle y_circle r_circle;
      done;
    done
      (*permet de représenter le plateau de jeu en court de partie*)
  else
    for i=0 to 6 do
      for j=0 to 5 do
        if (current_game.tab.(i).(j) = Empty) then set_color white
        else if (current_game.tab.(i).(j) = Red) then set_color red
        else set_color yellow;

        let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
        fill_circle x_circle y_circle r_circle;
        set_color black;
        draw_circle x_circle y_circle r_circle;
      done;
    done;

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
        else
          (
            if ((pos_x>(w/9)) && (pos_x<(8*w/9)) &&
                  (pos_y>(2*h/9)) && (pos_y<(8*h/9)))
            then
              (
                let raw = pos_x/(w/9)-1 in
                (*si la colonne n'est pas pleine, on peut encore y jouer*)
                if (current_game.tab.(raw).(5) = Empty) then
                  (
                    move current_game raw player1;
                    let action = List.hd current_game.list_event in
                    if (player1 = Red) then
                      color_circle2 red (action.raw) (action.line)
                    else color_circle2 yellow (action.raw) (action.line);
                    set_color black;
                    moveto 10 10;

                    let win = isWin current_game (action.raw) (action.line) in
                    if win = true then
                      (
                        let winner =
                          (if action.piece = Red then "Le joueur ROUGE gagne!!!" else "Le joueur JAUNE gagne!!!")in
                        draw_string winner;
                        let wait = ref true in 
                        while !wait do
                          let st = wait_next_event [Button_down] in 
                          (
                            let pos_x = st.mouse_x and pos_y = st.mouse_y in
                            if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
                                 && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
                            then
                              (
                                wait := false;
                                set_color white;
                                fill_rect 0 0 400 50;
                                set_color black;
                                set_line_width 1;
                                draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
                                set_line_width 2;
                                for i=0 to 6 do
                                  for j=0 to 5 do
                                    set_color white;
                                    let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
                                    fill_circle x_circle y_circle r_circle;
                                    set_color black;
                                    draw_circle x_circle y_circle r_circle;
                                  done;
                                done;
                                new_part current_game;
                                part Red Yellow;
                              )
                          );
                        done;
                      );
                    part player2 player1
                  )
                    (*si la colonne est pleine, le meme joueur rejoue*)
                else part player1 player2;
              )
          )
      )
        (*il recommence une nouvelle partie car le bouton New Part a été pressé*)
    else
      (
        let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
        if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
             && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
        then
          (
            set_color white;
            fill_rect 0 0 400 50;
            set_color black;
            set_line_width 1;
            draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
            set_line_width 2;
            for i=0 to 6 do
              for j=0 to 5 do
                set_color white;
                let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
                fill_circle x_circle y_circle r_circle;
                set_color black;
                draw_circle x_circle y_circle r_circle;
              done;
            done;
            new_part current_game;
            part Red Yellow;
          );
        part player1 player2;
      );
    part player1 player2 in
  part Red Yellow;
  
;;





(*let st = wait_next_event [Button_down] in ();;*)
let game_test = make() in
gameboard game_test;;
