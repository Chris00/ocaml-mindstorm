open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let color_port = `S4
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c
let current_line = ref 0
let current_col = ref 0
let next_line = ref 0
let next_col = ref (-1)
let light = ref true (*mettre à faux lorsqu'on veut juste remettre le capteur
 à droite*)
let current_game = ref 1635421 (*representation du jeu par un entier*)
(*num du jeu: de 0 a 6 de gauche à droit du cote du joueur*)
let expo_10 = [| 1; 10; 100; 1000; 10000; 100000; 1000000|]

let usleep sec =
  ignore(Unix.select [] [] [] sec)

let adjust_r = 27
let adjust_v = 20


let shift_h = 198
let shift_up_v = 132
let shift_up_r = 189
let shift_up_l = 189

(*angle de rotation de moteur_captor_vert pr la ligne [l]*)
let rot_v l =
  shift_up_v * l

(*angle de rotation de moteur_captor_r pr la ligne [l], colonne [c]*)
let rot_r l c =
  (shift_up_r*l)+(c*shift_h)

(*angle de rotation de moteur_captor_l pr la ligne [l], colonne [c]*)
let rot_l l c =
  (shift_up_l*l)-(c*shift_h)



module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct


  let scd (a, b, c) = b

  let lst (a, b, c) = c

  let last (a, b, c, d) = d

  let r = Robot.make()

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
    current_game := game + expo_10.(col)









  (* retourne la couleur devant le capteur*)
  let rec scan_light f _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    if !light then
      (
        Mindstorm.Sensor.set C.conn color_port `Color_full `Pct_full_scale;
        usleep 0.25;
        let data  = Mindstorm.Sensor.get C.conn color_port in
        let color_of_data data = match Sensor.color_of_data data with
          | `Black  -> -1 | `Blue -> 2 | `Green -> -1
          | `Yellow -> 1 | `Red  -> 0 | `White -> -1 in

        let color = color_of_data data in
        printf "%i\n%!" color;
        Mindstorm.Sensor.set C.conn color_port `No_sensor `Raw;
        usleep 0.25;

        if (color = (-1)) then
          (
            printf "Continuer\n%!";
            f () (*rappelle la fct scan_game qui appelera scan_case sur la
                   prochaine case*)
          )
        else if (color = 2) then
          (
            printf "Ajustement\n%!";
            adjustment !current_line !current_col f
          )
        else
          (
            add_piece !current_col !current_game;
            (*retourner la colonne au prog de jésus et sab*)
            printf "%i\n%!" !current_game;
            light := false; (*pr ne pas rescanner*)
            next_col := -1; (*pr que scan_game renvoie le capteur à droite du
                              jeu*)
            f ()
          )
      )
    else stop ()



  (*ajuste la position du capteur couleur*)
  and adj_l_l angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (-5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l)
      (* (scan_light f) *) (*ça doit etre ça mais prob!!!*)
      stop

  and adj_l_r angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d >= angle_l)
      (* scan_light f) *)
      stop

  and adj_l angle_l f _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Motor.set C.conn motor_captor_r (Motor.speed 0);
    match (Robot.read meas_left) with
     |Some m ->
        (
          if m <= angle_l then
            adj_l_r angle_l f
          else adj_l_l angle_l f
        )
     |None -> assert false

  and adj_r_r angle_r angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (5));
    Motor.set C.conn motor_captor_r (Motor.speed (-5));
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d <= angle_r)
      (adj_l angle_l f)

  and adj_r_l angle_r angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (-5));
    Motor.set C.conn motor_captor_r (Motor.speed (5));
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d >= angle_r)
      (adj_l angle_l f)

  and adj_r angle_r angle_l f _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    match (Robot.read meas_right) with
     |Some m  ->
        (
          if m < angle_r then
            adj_r_l angle_r angle_l f
          else adj_r_r angle_r angle_l f
        )
     |None -> assert false


  and adj_vert_down angle_v angle_r angle_l f =
    Motor.set C.conn motor_captor_vert (Motor.speed (-5));
    Motor.set C.conn motor_captor_l (Motor.speed (-7));
    Motor.set C.conn motor_captor_r (Motor.speed (-7));
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_v)
      (adj_r angle_r angle_l f)



  and adj_vert_up angle_v angle_r angle_l f =
    Motor.set C.conn motor_captor_vert (Motor.speed 5);
    Motor.set C.conn motor_captor_l (Motor.speed 7);
    Motor.set C.conn motor_captor_r (Motor.speed 7);
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v)
      (adj_r angle_r angle_l f)


  (*ajustement de la position du capteur*)
  (*on ajuste d'abord le moteur vert à petite vitesse puis le r puis le l*)
  and adjustment line col f =
    let angle_v = rot_v line and
        angle_r = rot_r line col and
        angle_l = rot_l line col in

    match (Robot.read meas_vert) with
     |Some m  ->
        (
          if m <= angle_v then
               adj_vert_up angle_v angle_r angle_l f
          else adj_vert_down angle_v angle_r angle_l f
        )
     |None -> assert false









  (*méthode pour descendre le capteur; vitesse neg pour descendre*)
  let go_down _ =
    Motor.set C.conn motor_captor_vert (Motor.speed (-10));
    Motor.set C.conn motor_captor_l (Motor.speed (-13));
    Motor.set C.conn motor_captor_r (Motor.speed (-13))

  (*méthode pour monter le capteur; vitesse pos pour monter*)
  let go_up _ =
    Motor.set C.conn motor_captor_vert (Motor.speed 10);
    Motor.set C.conn motor_captor_l (Motor.speed 20);
    Motor.set C.conn motor_captor_r (Motor.speed 20)

 (*méthode pour déplacer le capteur vers la droite*)
  let go_right _ =
    Motor.set C.conn motor_captor_l (Motor.speed (13));
    Motor.set C.conn motor_captor_r (Motor.speed (-12))

  (*méthode pour déplacer le capteur vers la gauche*)
  let go_left _ =
    Motor.set C.conn motor_captor_l (Motor.speed (-13));
    Motor.set C.conn motor_captor_r (Motor.speed 12)

  (*attendre d'avoir fait le déplacement vers la droite : c'est d'abord le
    moteur gauche qui a fini sa rotation puis le droit*)
  let wait_trans_right_r deg_new_pos_r f _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= deg_new_pos_r)
     (scan_light f)

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
      (scan_light f)

  let wait_trans_left_r deg_new_pos  f =
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
    else (scan_light f ())




  (*condition d'arret de l et r qd le mobile descend*)
  let wait_r_l_down_end angle_down diff_col deg_new_pos f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn motor_captor_r st;
    Motor.set C.conn motor_captor_l st;

    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= angle_down + adjust_r)
      (trans_hor diff_col deg_new_pos f)



  (*les moteurs l et r font presque tout à grande vitesse puis décélère sur
    les 30 derniers degrés*)
  let wait_r_l_down angle_down diff_col deg_new_pos f _ =
    Motor.set C.conn motor_captor_vert (Motor.speed 0);
    Robot.event meas_right (function
                        |None -> false
                        |Some d -> d <= angle_down+ adjust_r + 20)
     (wait_r_l_down_end angle_down diff_col deg_new_pos f)


  let wait_vert_down angle_down angle_up diff_col deg_new_pos f =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_up + adjust_v)
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


  let scan_case new_pos_line new_pos_col f =
    let diff_line = new_pos_line - !current_line and
        diff_col = new_pos_col - !current_col and
        deg_new_line = (rot_v new_pos_line,
                        rot_l new_pos_line !current_col,
                        rot_r new_pos_line !current_col) and
        deg_new_pos = (rot_v new_pos_line,
                       rot_l new_pos_line new_pos_col,
                       rot_r new_pos_line new_pos_col) in

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



  let rec scan_game (*alpha_beta*) _ =
    if (!next_col < 6) then
      (
        next_col := !next_col + 1;
        next_line := piece_in_col !next_col !current_game;
        if (!next_line = 6) then
          (
            scan_game ()
          )
        else
          (
            printf"%i\n%!" !next_line ;
            printf "%i\n%!" !next_col ;
            scan_case !next_line !next_col scan_game
          )
      )
    else
      (
        next_col := 0;
        next_line := piece_in_col !next_col !current_game;
        scan_case !next_line !next_col scan_game
      )

  let run () =
    scan_game () ;
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
