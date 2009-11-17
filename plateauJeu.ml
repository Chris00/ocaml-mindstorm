open Graphics
open Printf


let gameboard w h =
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

  for i=0 to 6 do
    for j=0 to 5 do
      set_color white;
      let x_circle = w/6+i*w/9 and y_circle = 5*h/18+j*h/9 in
      fill_circle x_circle y_circle r_circle;
      set_color black;
      draw_circle x_circle y_circle r_circle;
    done;
  done;



  (*utilisation bouton New Part*)
  let clic = wait_next_event [Button_down; Button_up] in
  if button_down() then
    (
      let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
      if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
           && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
      then
        (
          set_color white;
          draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
        )
    )
  else
    (
      let pos_x = clic.mouse_x and pos_y = clic.mouse_y in
      if ( (pos_x>(x-8)) && (pos_x<(x+n_x+6))
           && (pos_y>(y-7)) && (pos_y<(y+n_y+7)) )
      then
        (
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
        )
    )
;;
let onBoutonNP pos_x pos_y =
  ((pos_x>(x-8)) && (pos_x<(x+n_x+6))
   && (pos_y>(y-7)) && (pos_y<(y+n_y+7)));;

(*methode qui colorie les cercles dans la couleur color*)
let color_circle color x_center y_center w h=
  let r_circle = 7*(min (w/9) (h/9))/16 in
  set_color color;
  fill_circle x_center y_center r_circle;
  set_color black;
  draw_circle x_center y_center r_circle;;

(*methode qui trace un disque dans la couleur color, en position (raw, line)*)
let color_circle2 color raw line w h=
  color_circle color (w/6 + raw*w/9) (5*h/18 + line*h/9);;



(*let st = wait_next_event [Button_down] in ();;*)
(*gameboard 1000 720;;*)
