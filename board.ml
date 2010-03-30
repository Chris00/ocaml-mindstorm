open Graphics
open Printf


let number_piece = Array.make 7 0
  (*nombre representant le nombre de pion dans chaque colonne*)
let get_line col =
  number_piece.(col)
let add_piece col =
  number_piece.(col) <- number_piece.(col) + 1

let w = 1000 and h = 720

(*methode qui colorie les cercles dans la couleur color*)
let color_circle color x_center y_center =
  let r_circle = 7*(min (w/9) (h/9))/16 in
  set_color color;
  fill_circle x_center y_center r_circle;
  set_color black;
  draw_circle x_center y_center r_circle

(*methode qui trace un disque de la couleur color, en position (raw, col)
  raw va de 0 a 5 et col de 0 a 6*)
let add_piece_to_board color col =
  let line = get_line col in
  color_circle color (w/6 + col*w/9) (2*h/9 + h/18 + line*h/9);
  add_piece col


let gameboard () =
  open_graph(sprintf " %ix%i" w h);
  set_window_title("Connect Four");

  (*creation du quadrillage*)
  set_color blue;
  let x_rect = w/9 and y_rect = 2*h/9 in
  let w_rect = 7*w/9 and h_rect = 6*h/9 in
  fill_rect x_rect y_rect w_rect h_rect;
  set_color black;
  set_line_width 3;
  draw_rect x_rect y_rect w_rect h_rect;
  synchronize();

  set_line_width 2;
  let worh = min (w/9) (h/9) in
  let r_circle = 7*worh/16 in

  (*dessine les ronds blancs repr�ntant le tableau vide*)
  for j=0 to 6 do
    for i = 0 to 5 do
      set_color white;
      let x_circle = w/6+j*w/9 and y_circle = 5*h/18+i*h/9 in
      fill_circle x_circle y_circle r_circle;
      set_color black;
      draw_circle x_circle y_circle r_circle;
    done;
  done;
  synchronize()

let text_red = "C'est au tour du joueur rouge"
and text_yellow = "C'est au tour du joueur jaune" in
let (x_text_yellow, y_text_yellow) = text_size text_yellow

let delete_the_left_text() =
  let x_rect = w/9 - 1
  and y_rect = (h/9) - (y_text_yellow)/2 - 1
  and w_rect = (x_text_yellow) + 2
  and h_rect = (y_text_yellow) + 2 in
  set_color white;
  fill_rect x_rect y_rect w_rect h_rect;

let write_player_turn color =
  delete_the_left_text();
  set_font "12x24kana";
  if color = red then
    (
      set_color red;
      moveto (x_rect+1) (y_rect+1);
      draw_string text_red
    )
  else
    (
      set_color yellow;
      moveto (x_rect+1) (y_rect+1);
      draw_string text_yellow
    )

let red_success () =
  delete_the_left_text();
  set_font "12x24kana";
  set_color red;
  let winner = "Le joueur ROUGE gagne!!!" in
  let (n_xw, n_yw) = text_size winner in
  let xw = (w - n_xw)/2
  and yw = (h/9)-(n_yw/2)in
  moveto xw yw;
  draw_string winner

let yellow_success () =
  delete_the_left_text();
  set_font "12x24kana";
  set_color yellow;
  let winner = "Le joueur JAUNE gagne!!!" in
  let (n_xw, n_yw) = text_size winner in
  let xw = (w - n_xw)/2
  and yw = (h/9)-(n_yw/2)in
  moveto xw yw;
  draw_string winner;

let draw () =
  delete_the_left_text();
  set_font "12x24kana";
  set_color black;
  let winner = "Match Nul" in
  let (n_xw, n_yw) = text_size winner in
  let xw = (w - n_xw)/2
  and yw = (h/9)-(n_yw/2)in
  moveto xw yw;
  draw_string winner

let close_when_clicked () =
  ignore (wait_next_event [Button_down])
