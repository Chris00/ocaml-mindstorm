open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let switch_port = `S3
let color_port = `S1
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c
let init_game = 0 (*representation du jeu par un entier*)
(*num du jeu: de 0 a 6 de gauche à droit du cote du joueur*)
let expo_10 = [| 1000000; 100000; 10000; 1000; 100; 10; 1|]

(*tableau des angles courants des moteurs pour avoir le capteur de couleur en
face des trous pour chaque ligne du jeu
la première valeur est pour le moteur vert, la seconde pr l; la troisième pr r*)
let tab_scan =
  [|
    [|
      (0, 0, 0); (0, -206, 206); (0, -415, 415); (0, -622, 622);
      (0, -830, 830); (0, -1038, 1038); (0, -1246, 1246);
    |];
    [|
      (131, 193, 193); (131, -15, 401); (131, -223, 609); (131, -431, 817);
      (131, -639, 1025); (131, -847, 1233); (131, -1055, 1441)
    |];
    [|
      (262, 386, 386); (262, 178, 594); (262, -30, 802); (262, -238, 1010);
      (262, -446, 1218); (262, -654, 1426); (262, -862, 1634)
    |];
    [|
      (393, 579, 579); (393, 371, 787); (393, 163, 995); (393, -45, 1203);
      (393, -253, 1411); (393, -461, 1619); (393, -669, 1827)
    |];
    [|
      (524, 772, 772); (524, 564, 980); (524, 356, 1188); (524, 148, 1396);
      (524, -60, 1604); (524, -268, 1812); (524, -476, 2020)
    |];
    [|
      (655, 965, 965); (655, 757, 1173); (655, 549, 1381); (655, 341, 1589);
      (655, 133, 1797); (655, -75, 2005); (655, -283, 2213)
    |]
  |]

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct


  let fst (a, b, c) = a

  let scd (a, b, c) = b

  let lst (a, b, c) = c

  let last (a, b, c, d) = d
 
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
  let scan_lum _ =
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
    Mindstorm.Sensor.set C.conn color_port `No_sensor `Raw

  (*méthode pour descendre le capteur; vitesse neg pour descendre*)
  let go_down _ =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_vert (Motor.speed (-8));
    Motor.set C.conn motor_captor_l (Motor.speed (-11));
    Motor.set C.conn motor_captor_r (Motor.speed (-11))

  (*méthode pour monter le capteur; vitesse pos pour monter*)
  let go_up _ =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_vert (Motor.speed 6);
    Motor.set C.conn motor_captor_l (Motor.speed 9);
    Motor.set C.conn motor_captor_r (Motor.speed 9)

  (*méthode pr déplacer le capteur horizontalement
    dir = 1 vers le distributeur de pieces*)
  let go_horizontal dir =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_l (Motor.speed ((-13) * dir) );
    Motor.set C.conn motor_captor_r (Motor.speed (13 * dir) )


  (*attendre d'avoir fait le déplacement vers la droite*)
  let wait_trans_right_r deg_new_pos =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= lst(deg_new_pos))
      scan_lum

  let wait_trans_right_l deg_new_pos =
    Robot.event meas_left (function
                            |None -> false
                            |Some d -> d >= scd(deg_new_pos))
      scan_lum


  (*attendre d'avoir fait le déplacement vers la gauche*)
  let wait_trans_left_r deg_new_pos =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d >= lst(deg_new_pos))
      scan_lum

  let wait_trans_left_l deg_new_pos =
    Robot.event meas_left (function
                            |None -> false
                            |Some d -> d <= scd(deg_new_pos))
      scan_lum

  (*déplacement horizontal pr etre ds la bonne colonne*)
  let trans_hor diff_col deg_new_pos _ =
    Motor.set C.conn motor_captor_vert (Motor.speed 0);
    if (diff_col <> 0) then
      (
        if (diff_col > 0) then
          (
            go_horizontal 1;
            wait_trans_left_r deg_new_pos;
            wait_trans_left_l deg_new_pos
          )
        else
          (
            go_horizontal (-1);
            wait_trans_right_r deg_new_pos;
            wait_trans_right_l deg_new_pos
          )
      )
    else scan_lum ()


  (*condition d'arret qd le mobile descend*)
  let wait_r_l_down angle_down diff_col deg_new_pos _ =
    Motor.set C.conn motor_captor_vert (Motor.speed 0);
    Robot.event meas_right (function
                        |None -> false
                        |Some d -> d <= angle_down)
      (trans_hor diff_col deg_new_pos)


  let wait_vert_down angle_down angle_up diff_col deg_new_pos =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_up)
      (wait_r_l_down angle_down diff_col deg_new_pos)





  (*condition d'arret qd le mobile monte*)
  let wait_vert_up angle_up diff_col deg_new_pos _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Motor.set C.conn motor_captor_r (Motor.speed 0) ;
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_up)
      (trans_hor diff_col deg_new_pos)

  (*on coupe les moteurs l et r et on attend que le moteur vert ait fini*)

  (*attente du travail des moteurs l et r*)
  let wait_r_l_up angle_down angle_up diff_col deg_new_pos =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d >= angle_down)
      (wait_vert_up angle_up diff_col deg_new_pos)


  let scan_case old_pos_line old_pos_col new_pos_line new_pos_col =
    let diff_line = new_pos_line - old_pos_line and
        diff_col = new_pos_col - old_pos_col and
        deg_new_line = tab_scan.(new_pos_line).(old_pos_col) and
        deg_new_pos = tab_scan.(new_pos_line).(new_pos_col) in

    (*on le déplace d'abord verticalement puis horizontalement*)
    if (diff_line <> 0) then
      (
        let angle_up, _, angle_down = deg_new_line in
        if diff_line > 0 then
          (
            go_up ();
            (*c'est d'abord les moteurs l et r qui ont fini leur boulot
              puis le vert*)
            wait_r_l_up angle_down angle_up diff_col deg_new_pos
          )
        else
          (
            go_down ();
            (*c'est d'abord le moteur vert qui a fini son boulot puis l et r*)
            wait_vert_down angle_down angle_up diff_col deg_new_pos
          )
      )
    else
      trans_hor diff_col deg_new_pos ()


  let run () =
    (* reset_pos C.conn motor_captor_vert; *)
    (* reset_pos C.conn motor_captor_l; *)
    (* reset_pos C.conn motor_captor_r; *)
    scan_case 0 3 0 0;
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
