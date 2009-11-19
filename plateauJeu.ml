open Graphics
open Printf

let plateau w h  =
  open_graph(sprintf " %ix%i" w h );
  set_window_title("Puissance 4");

  (*creation du bouton nouvelle partie*)
  set_font "12x24kana";
  let n_x = fst (text_size "Nouvelle Partie")
  and n_y = snd (text_size "Nouvelle Partie") in
  let x = (w/2 - n_x/2) and y = (h/9) in
  moveto x y;
  draw_string "Nouvelle Partie";
  draw_rect (x-4) (y-3) (n_x+8) (n_y+6);
  set_line_width 3;
  draw_rect (x-8) (y-7) (n_x+16) (n_y+14);

  (*creation du quadrillage*)
  set_color blue;
  let w1 = w - w mod 9 and h1 = h - h mod 9 in
  let x_rect = w1/9 and y_rect = 2*h1/9
                    and w_rect = 7*w1/9 and h_rect = 6*h1/9 in
  fill_rect x_rect y_rect w_rect h_rect;
  set_color black;
  set_line_width 3;
  draw_rect x_rect y_rect w_rect h_rect;

  set_line_width  2;
  let w1orh1 = min (w1/9) (h1/9) in
  let r_circle = 7*w1orh1/16 in

  for i=0 to 6 do
    for j=0 to 5 do
      set_color white;
      let x_circle = w1/6+i*w1/9 and y_circle = 5*h1/18+j*h1/9 in
      fill_circle x_circle y_circle r_circle;
      set_color black;
      draw_circle x_circle y_circle r_circle;
    done;
  done;


  (*methode qui colorie les cercles dans la couleur color*)
  let color_circle color x_center y_center =
    set_color color;
    fill_circle x_center y_center r_circle;
    set_color black;
    draw_circle x_center y_center r_circle in

  (*r�cup�ration position souris*)
  let rec jeu jou tab =
  (*clic sur nouvelle partie*)
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
            jeu jou tab
          )
        else if ( (pos_x>(w1/9)) && (pos_x<(8*w1/9))
                  && (pos_y>(2*h1/9)) && (pos_y<(8*h1/9)) )
        then let col = pos_x/(w1/9) in
        if tab.(col-1) < 6 then
          (
            tab.(col-1) <- tab.(col-1) + 1;
            let ligne = tab.(col-1) + 2 in
            if jou
            then color_circle yellow ((col*w1/9) + w1/18) (ligne*h1/9 - h1/18)
            else color_circle red ((col*w1/9) + w1/18) (ligne*h1/9 - h1/18);
            jeu (not jou) tab
          )
        else jeu jou tab
        else jeu jou tab
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
                let x_circle = w1/6+i*w1/9 and y_circle = 5*h1/18+j*h1/9 in
                fill_circle x_circle y_circle r_circle;
                set_color black;
                draw_circle x_circle y_circle r_circle;
              done;
            done;
            jeu true [|0;0;0;0;0;0;0|](*colorier la case/pion clicl�e*)
          )
        else jeu jou tab
      )
  in jeu true [|0;0;0;0;0;0;0|];;

(*let st = wait_next_event [Button_down] in ();;*)
plateau 1000 720;;
