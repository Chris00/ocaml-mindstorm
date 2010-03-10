open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let switch_port = `S3
let color_port = `S4
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c
let current_line = ref 0
let current_col = ref 0
let init_game = 5234426 (*representation du jeu par un entier*)
(*num du jeu: de 0 a 6 de gauche à droit du cote du joueur*)
let expo_10 = [| 1; 10; 100; 1000; 10000; 100000; 1000000|]

let shift_h = 204
let shift_up_v = 132
let shift_up_r = 189
let shift_up_l = 189
let rot_v l =
  shift_up_v * l

let rot_r l c =
  (shift_up_r*l)+(c*shift_h)

let rot_l l c =
  (shift_up_l*l)-(c*shift_h)

(*tableau des angles courants des moteurs pour avoir le capteur de couleur en
face des trous pour chaque ligne du jeu
la première valeur est pour le moteur vert, la seconde pr l; la troisième pr r*)
let tab_scan =
  [|
    [|
      (0, 0, 0); (0, rot_l 0 1, rot_r 0 1); (0, rot_l 0 2, rot_r 0 2);
      (0, rot_l 0 3, rot_r 0 3); (0, rot_l 0 4, rot_r 0 4);
      (0, rot_l 0 5, rot_r 0 5); (0, rot_l 0 6, rot_r 0 6);
    |];
    [|
      (rot_v 1, rot_l 1 0, rot_r 1 0); (rot_v 1, rot_l 1 1, rot_r 1 1);
      (rot_v 1, rot_l 1 2, rot_r 1 2); (rot_v 1, rot_l 1 3, rot_r 1 3);
      (rot_v 1, rot_l 1 4, rot_r 1 4); (rot_v 1, rot_l 1 5, rot_r 1 5);
      (rot_v 1, rot_l 1 6, rot_r 1 6)
    |];
    [|
      (rot_v 2, rot_l 2 0, rot_r 2 0); (rot_v 2, rot_l 2 1, rot_r 2 1);
      (rot_v 2, rot_l 2 2, rot_r 2 2); (rot_v 2, rot_l 2 3, rot_r 2 3);
      (rot_v 2, rot_l 2 4, rot_r 2 4); (rot_v 2, rot_l 2 5, rot_r 2 5);
      (rot_v 2, rot_l 2 6, rot_r 2 6)
    |];
    [|
      (rot_v 3, rot_l 3 0, rot_r 3 0); (rot_v 3, rot_l 3 1, rot_r 3 1);
      (rot_v 3, rot_l 3 2, rot_r 3 2); (rot_v 3, rot_l 3 3, rot_r 3 3);
      (rot_v 3, rot_l 3 4, rot_r 3 4); (rot_v 3, rot_l 3 5 ,rot_r 3 5);
      (rot_v 3, rot_l 3 6, rot_r 3 6)
    |];
    [|
      (rot_v 4, rot_l 4 0, rot_r 4 0); (rot_v 4, rot_l 4 1, rot_r 4 1);
      (rot_v 4, rot_l 4 2, rot_r 4 2); (rot_v 4, rot_l 4 3, rot_r 4 3);
      (rot_v 4, rot_l 4 4, rot_r 4 4); (rot_v 4, rot_l 4 5 ,rot_r 4 5);
      (rot_v 4, rot_l 4 6, rot_r 4 6)
    |];
    [|
      (rot_v 5, rot_l 5 0, rot_r 5 0); (rot_v 5, rot_l 5 1, rot_r 5 1);
      (rot_v 5, rot_l 5 2, rot_r 5 2); (rot_v 5, rot_l 5 3, rot_r 5 3);
      (rot_v 5, rot_l 5 4, rot_r 5 4); (rot_v 5, rot_l 5 5 ,rot_r 5 5);
      (rot_v 5, rot_l 5 6, rot_r 5 6)
    |]
  |]

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct


  let fst (a, b, c) = a

  let scd (a, b, c) = b

  let lst (a, b, c) = c

  let last (a, b, c, d) = d

  let first (a, b, c, d) = a

  let r = Robot.make()

  let touch = Robot.touch C.conn switch_port r

  (*nous retourne l'angle courant du moteur droit*)
  let meas_right =
    Robot.meas r (fun _ -> last (Motor.get C.conn motor_captor_r))

  (*nous retourne l'angle courant du moteur gauche*)
  let meas_left =
    Robot.meas r (fun _ ->  last (Motor.get C.conn motor_captor_l))

 (*nous retourne l'angle courant du moteur vertical*)
  let meas_vert =
    Robot.meas r (fun _ -> last (Motor.get C.conn motor_captor_vert))


  let stop _ =
    Motor.set C.conn Motor.all (Motor.speed 0)

  let stop_motor_l _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0)


  (*methode retournant le nbre de pions ds la col [col] du jeu [game]*)
  let piece_in_col col game =
    (game/expo_10.(col)) mod 10


  (*methode ajoutant une piece au jeu [game] en colonne [col]*)
  let add_piece col game =
    game + expo_10.(col)

  (* retourne la couleur devant le capteur*)
  let scan_lum f _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    Mindstorm.Sensor.set C.conn color_port `Color_full `Pct_full_scale;
    Unix.sleep 1;
    let data  = Mindstorm.Sensor.get C.conn color_port in
    let color_of_data data = match Sensor.color_of_data data with
      | `Black  -> "black " | `Blue -> "blue  " | `Green -> "green "
      | `Yellow -> "yellow" | `Red  -> "red   " | `White -> "white " in
    let color = color_of_data data in
    Printf.printf "%s" color;
    Printf.printf "\n%!";
    Mindstorm.Sensor.set C.conn color_port `No_sensor `Raw;

    Unix.sleep 1;
    (f ())

  (*méthode pour descendre le capteur; vitesse neg pour descendre*)
  let go_down _ =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_vert (Motor.speed (-10));
    Motor.set C.conn motor_captor_l (Motor.speed (-13));
    Motor.set C.conn motor_captor_r (Motor.speed (-13))

  (*méthode pour monter le capteur; vitesse pos pour monter*)
  let go_up _ =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_vert (Motor.speed 10);
    Motor.set C.conn motor_captor_l (Motor.speed 20);
    Motor.set C.conn motor_captor_r (Motor.speed 20)


  let go_right _ =
    Motor.set C.conn motor_captor_l (Motor.speed (13));
    Motor.set C.conn motor_captor_r (Motor.speed (-12))


  let go_left _ =
    Motor.set C.conn motor_captor_l (Motor.speed (-13));
    Motor.set C.conn motor_captor_r (Motor.speed 12)

  (*attendre d'avoir fait le déplacement vers la droite*)
  let wait_trans_right_r deg_new_pos_r f _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= deg_new_pos_r)
     ( scan_lum f)

  let wait_trans_right_l deg_new_pos f =
    Robot.event meas_left (function
                            |None -> false
                            |Some d -> d >= scd(deg_new_pos))
      (wait_trans_right_r (lst(deg_new_pos)) f)


  (*attendre d'avoir fait le déplacement vers la gauche*)
  let wait_trans_left_l deg_new_pos_l f _ =
    Motor.set C.conn motor_captor_r (Motor.speed 0);
    Robot.event meas_left (function
                            |None -> false
                            |Some d -> d <= deg_new_pos_l)
      (scan_lum f)

  let wait_trans_left_r deg_new_pos f =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d >= lst(deg_new_pos))
      (wait_trans_left_l (scd(deg_new_pos)) f)


  (*déplacement horizontal pr etre ds la bonne colonne*)
  let trans_hor diff_col deg_new_pos f _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    if (diff_col <> 0) then
      (
        if (diff_col > 0) then
          (
            go_left ();
            wait_trans_left_r deg_new_pos f
          )
        else
          (
            go_right ();
            wait_trans_right_l deg_new_pos f
          )
      )
    else scan_lum f ()




  (*condition d'arret de l et r qd le mobile descend*)
  let wait_r_l_down_end angle_down diff_col deg_new_pos f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn motor_captor_r st;
    Motor.set C.conn motor_captor_l st;

    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= angle_down+18)
      (trans_hor diff_col deg_new_pos f)



  (*les moteurs l et r font presque tout à grande vitesse puis décélère sur
    les 30 derniers degrés*)
  let wait_r_l_down angle_down diff_col deg_new_pos f _ =
    Motor.set C.conn motor_captor_vert (Motor.speed 0);
    Robot.event meas_right (function
                        |None -> false
                        |Some d -> d <= angle_down+18 + 20)
     (wait_r_l_down_end angle_down diff_col deg_new_pos f)


(*
  let wait_vert_down_end  angle_down angle_up diff_col deg_new_pos f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 40} in
    Motor.set C.conn motor_captor_vert st;
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_up+10)
      (wait_r_l_down angle_down diff_col deg_new_pos f)
*)

  let wait_vert_down angle_down angle_up diff_col deg_new_pos f =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_up+13)
      (wait_r_l_down angle_down diff_col deg_new_pos f)







  (*condition d'arret de vert qd le mobile monte*)
  let wait_vert_up_end angle_up diff_col deg_new_pos f _ =
    let st = {speed = 0; motor_on = true; brake = true;
                   regulation = `Idle; turn_ratio = 100;
                   run_state = `Ramp_down; tach_limit = 40} in
    Motor.set C.conn motor_captor_vert st;
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_up)
      (trans_hor diff_col deg_new_pos f)

  (* le moteur vert monte vite d'abord et décélère pdt les 30 derniers degrés *)
  let wait_vert_up angle_up diff_col deg_new_pos f _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Motor.set C.conn motor_captor_r (Motor.speed 0);
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_up-30)
      (wait_vert_up_end angle_up diff_col deg_new_pos f)



  (*attente du travail des moteurs l et r*)
  let wait_r_l_up angle_down angle_up diff_col deg_new_pos f =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d >= angle_down)
      (wait_vert_up angle_up diff_col deg_new_pos f)


  let scan_case new_pos_line new_pos_col f _ =
     if new_pos_line = 6 then
      f()
     else
       (
         let diff_line = new_pos_line - !current_line and
             diff_col = new_pos_col - !current_col and
             deg_new_line = tab_scan.(new_pos_line).(!current_col) and
             deg_new_pos = tab_scan.(new_pos_line).(new_pos_col) in

         current_line := new_pos_line;
         current_col := new_pos_col;

         (*on le déplace d'abord verticalement puis horizontalement*)
         if (diff_line <> 0) then
           (
             let angle_up, _, angle_down = deg_new_line in
             if diff_line > 0 then
               (
                 go_up ();
                 wait_r_l_up angle_down angle_up diff_col deg_new_pos f
               )
             else
               (
                 go_down ();
                 wait_vert_down angle_down angle_up diff_col deg_new_pos f
               )
           )
         else
           trans_hor diff_col deg_new_pos f ()
       )
  let scan_game current_game =
    scan_case (piece_in_col 0 current_game) 0
      (scan_case (piece_in_col 1 current_game) 1
         (scan_case (piece_in_col 2 current_game) 2
            (scan_case (piece_in_col 3 current_game) 3
               (scan_case (piece_in_col 4 current_game) 4
                  (scan_case (piece_in_col 5 current_game) 5
                     (scan_case (piece_in_col 6 current_game) 6 (stop ))))))) ()


  let run () =
     scan_game init_game;
    (* scan_case 0 0 (stop )(); *)
    Robot.run r

end

let () =
  let (bt)=
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1)) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
