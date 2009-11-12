open Graphics
open Printf

class plateauJeu (w:int)(h:int) =
  object
  method gameboard =
    open_graph(sprintf " %ix%i" w h);
    set_window_title("Connect 4");
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

  method color_circle color x_center y_center =
    let r_circle = 7*(min (w/9) (h/9))/16 in
    set_color color;
    fill_circle x_center y_center r_circle;
    set_color black;
    draw_circle x_center y_center r_circle

end
(*let st = wait_next_event [Button_down] in ();;*)
