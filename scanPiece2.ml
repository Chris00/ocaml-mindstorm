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
let go_to_next = ref false
let current_game = ref 1635422 (*representation du jeu par un entier*)
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



(*tab des vitesses des trois moteurs (vert, r, l) lors des déplacements vers la
  gauche. pr aller à droite vitesse r = -vitesse l*)
(*speed_up.(i).(j) nous donne les vitesses des moteurs (vert, r, l) lorsqu'on
  doit se déplacer de i lignes vers le haut et de j col vers la gauche*)
let speed_up =
  [|
    [|(0, 0, 0); (0, 13, -12); (0, 13, -12); (0, 13, -12);
      (0, 13, -12); (0, 13, -12); (0, 13, -12)|];
    [|(10, 20, 20); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(10, 20, 20); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(10, 20, 20); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(10, 20, 20); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(10, 20, 20); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|]
  |]

(*meme chose mais vers le bas*)
let speed_down =
  [|
    [|(0, 0, 0); (0, 13, -12); (0, 13, -12); (0, 13, -12);
      (0, 13, -12); (0, 13, -12); (0, 13, -12)|];
    [|(-10, -13, -13); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(-10, -13, -13); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(-10, -13, -13); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(-10, -13, -13); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|];
    [|(-10, -13, -13); (0, 0, 0); (0, 0, 0); (0, 0, 0);
      (0, 0, 0); (0, 0, 0); (0, 0, 0)|]
  |]




module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct

  let fst (a, b, c) = a

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
    else
      (
        light := true;
        f ()
      )



  (*ajuste la position du capteur couleur*)
  and adj_l_l angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (-5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l)
      (scan_light f)

  and adj_l_r angle_l f =
    Motor.set C.conn motor_captor_l (Motor.speed (5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d >= angle_l)
      (scan_light f)


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





  (*attend d'avoir fini de monter (avec ralenti) avant de scanner la case*)
  let wait_up_end angle_v f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn motor_captor_vert st;
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v)
      (scan_light f)

  let wait_up angle_v f =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v - 20)
      (wait_up_end angle_v f)

  (*attend d'avoir fini ac le moteur r (ac ralenti) avant de scanner la case*)
  let wait_down_right_end angle_r f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn motor_captor_r st;
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d <= angle_r)
      (scan_light f)

  let wait_down_right angle_r f =
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d <= angle_r + 20)
      (wait_down_right_end angle_r f)

  (*attend d'avoir fini ac le moteur l (ac ralenti) avant de scanner la case*)
  let wait_down_left_end angle_l f _ =
    let st = {speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn motor_captor_l st;
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d <= angle_l)
      (scan_light f)


  let wait_down_left angle_l f =
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l + 20)
      (wait_down_left_end angle_l f)



  let scan_case new_pos_line new_pos_col f =
    let diff_line = new_pos_line - !current_line and
        diff_col = new_pos_col - !current_col and
        angle_v = rot_v new_pos_line and
        angle_r = rot_r new_pos_line new_pos_col and
        angle_l = rot_l new_pos_line new_pos_col in

    current_line := new_pos_line;
    current_col := new_pos_col;


    (*on fait les deux mouvements en meme temps*)
    if (diff_line >= 0) then (*on va vers le haut*)
      (
        (match (Robot.read meas_vert) with
         |Some m  ->
            Motor.set C.conn motor_captor_vert (Motor.speed ~tach_limit:
                     (angle_v - m + 5)
                     (fst speed_up.(diff_line).(diff_col)))
         |None -> assert false);


        if (diff_col >= 0) then (*on va vers la gauche*)
          (
            (match (Robot.read meas_right) with
             |Some m  ->
                Motor.set C.conn motor_captor_r (Motor.speed ~tach_limit:
                      (angle_r - m) (scd speed_up.(diff_line).(diff_col)))
             |None -> assert false);

            (match (Robot.read meas_left) with
             |Some m  ->
                Motor.set C.conn motor_captor_l (Motor.speed ~tach_limit:
                      (abs(angle_l - m)) (lst speed_up.(diff_line).(diff_col)))
             |None -> assert false)
          )
        else (*on va vers la droite*)
          (
            (match (Robot.read meas_right) with
             |Some m  ->
                Motor.set C.conn motor_captor_r (Motor.speed
                 ~tach_limit: (abs(angle_r - m))
                 ((-1)*(lst speed_up.(diff_line).(abs(diff_col)))))
             |None -> assert false);


            (match (Robot.read meas_left) with
             |Some m  ->
                Motor.set C.conn motor_captor_l (Motor.speed
                      ~tach_limit: (angle_l - m)
                      ((-1)*(scd speed_up.(diff_line).(abs(diff_col)))))
             |None -> assert false)
          );
        wait_up angle_v f
      )

    else (*on descend*)
      (
        (match (Robot.read meas_vert) with
         |Some m  ->
            Motor.set C.conn motor_captor_vert (Motor.speed ~tach_limit:
                     (abs(m -angle_v)) (fst speed_down.(diff_line).(diff_col)))
         |None -> assert false);


        if (diff_col >= 0) then (*on va vers la gauche*)
          (
            (match (Robot.read meas_right) with
             |Some m  ->
                Motor.set C.conn motor_captor_r (Motor.speed ~tach_limit:
                   (abs(angle_r - m)) (scd speed_up.(diff_line).(diff_col)))
             |None -> assert false);

            (match (Robot.read meas_left) with
             |Some m  ->
                Motor.set C.conn motor_captor_l (Motor.speed ~tach_limit:
                      (abs(m - angle_l + 5)) (lst speed_up.(diff_line).(diff_col)))
             |None -> assert false);

            wait_down_left angle_l f
          )
        else (*on va vers la droite*)
          (
            (match (Robot.read meas_right) with
             |Some m  ->
                Motor.set C.conn motor_captor_r (Motor.speed
                   ~tach_limit: (abs(m - angle_r + 5))
                   ((-1)*(lst speed_up.(diff_line).(diff_col))))
             |None -> assert false);

            (match (Robot.read meas_left) with
             |Some m  ->
                Motor.set C.conn motor_captor_l (Motor.speed
                      ~tach_limit: (abs(angle_l - m))
                      ((-1)*(scd speed_up.(diff_line).(diff_col))))
             |None -> assert false);

            wait_down_right angle_r f
          )
      )





  let rec scan_game  next _ =
    if (!go_to_next) then
      (
        go_to_next := false;
        next_col := -1;
        printf"passe à next\n%!";
        next ()
      )
    else
      (
        if(not !light) then go_to_next := true;

        if (!next_col < 6) then
          (
            next_col := !next_col + 1;
            next_line := piece_in_col !next_col !current_game;
            if (!next_line = 6) then
              (
                scan_game next ()
              )
            else
              (
                printf"%i\n%!" !next_line ;
                printf "%i\n%!" !next_col ;
                scan_case !next_line !next_col (scan_game next)
              )
          )
        else
          (
            next_col := 0;
            next_line := piece_in_col !next_col !current_game;
            scan_case !next_line !next_col (scan_game next)
          )
      )


  let run () =
    scan_game stop ();(*petit test pr direct rescanner le nv jeu*) 
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
