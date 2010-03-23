open Printf
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor


let color_port = `S4
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c

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

let when_some cond v = match v with
  | None -> false
  | Some d -> cond d

module Run(C: sig val conn_scan : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) =
struct

  (* État du jeu *)
  let current_line = ref 0
  let current_col = ref 0
  let next_line = ref 0
  let next_col = ref (-1)
  let col_had_play = ref 0
  let light = ref true (*mettre à faux lorsqu'on veut juste remettre le capteur
                         à droite*)
  let go_to_next = ref false
  let current_game = ref 0 (*representation du jeu par un entier*)

  let get_angle motor = let _,_,_,a = Motor.get C.conn_scan motor in a

 (*nous retourne l'angle courant du moteur droit*)
  let meas_right = Robot.meas C.r (fun () -> get_angle motor_captor_r)

  (*nous retourne l'angle courant du moteur gauche*)
  let meas_left = Robot.meas C.r (fun () ->  get_angle motor_captor_l)

 (*nous retourne l'angle courant du moteur vertical*)
  let meas_vert = Robot.meas C.r (fun () -> get_angle motor_captor_vert)


  let stop _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0)

  let stop_motor_l () =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0)


  (*methode retournant le nbre de pions ds la col [col] du jeu [game]*)
  let piece_in_col col game =
    (game/expo_10.(col)) mod 10


  (*methode ajoutant une piece au jeu [game] en colonne [col]*)
  let add_piece col game =
    current_game := game + expo_10.(col)


  (* retourne la couleur devant le capteur*)
  let rec scan_light f =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    if !light then (
      Mindstorm.Sensor.set C.conn_scan color_port `Color_full `Pct_full_scale;
      usleep 0.25;
      let data  = Mindstorm.Sensor.get C.conn_scan color_port in
      let color = Sensor.color_of_data data in
      Mindstorm.Sensor.set C.conn_scan color_port `No_sensor `Raw;
      usleep 0.25;
      match color with
      | `Black | `Green | `White ->
          printf "Continuer\n%!";
          f () (*rappelle la fct scan_game qui appelera scan_case sur la
                 prochaine case*)
      | `Blue ->
          printf "Ajustement\n%!";
          adjustment !current_line !current_col f
      | `Yellow | `Red ->
          (*il a trouvé une piece*)
          add_piece !current_col !current_game;
          col_had_play := !current_col;
          (*retourner la colonne au prog de jésus et sab*)
          printf "%i\n%i\n%!" !col_had_play !current_game;
          light := false; (*pr ne pas rescanner*)
          next_col := -1; (*pr que scan_game renvoie le capteur à droite du
                            jeu*)
          f ()
    )
    else (
      light := true;
      f ()
    )

  (*ajuste la position du capteur couleur*)
  and adj_l angle_l f _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 0);
    match Robot.read meas_left with
    | Some m ->
        let speed, good_angle =
          if m <= angle_l then 5, (fun a -> a >= angle_l)
          else -5, (fun a -> a <= angle_l) in
        Motor.set C.conn_scan motor_captor_l (Motor.speed speed);
        Robot.event meas_left (when_some good_angle) (fun _ -> scan_light f)
    | None -> assert false

  and adj_r angle_r angle_l f _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    match Robot.read meas_right with
    | Some m  ->
        let speed_r, good_angle =
          if m < angle_r then 5, (fun a -> a >= angle_r)
          else -5, (fun a -> a <= angle_r) in
        Motor.set C.conn_scan motor_captor_r (Motor.speed speed_r);
        Motor.set C.conn_scan motor_captor_l (Motor.speed (- speed_r));
        Robot.event meas_right (when_some good_angle) (adj_l angle_l f)
    | None -> assert false

  (*ajustement de la position du capteur*)
  (*on ajuste d'abord le moteur vert à petite vitesse puis le r puis le l*)
  and adjustment line col f =
    let angle_v = rot_v line and
        angle_r = rot_r line col and
        angle_l = rot_l line col in
    match Robot.read meas_vert with
    | Some m  ->
        let speed_vert, speed, good_angle =
          if m <= angle_v then 5, 7, (fun a -> a >= angle_v) (* go up *)
          else -5, -7, (fun a -> a <= angle_v) in
        Motor.set C.conn_scan motor_captor_vert (Motor.speed speed_vert);
        Motor.set C.conn_scan motor_captor_l (Motor.speed speed);
        Motor.set C.conn_scan motor_captor_r (Motor.speed speed);
        Robot.event meas_vert (when_some good_angle) (adj_r angle_r angle_l f)
    | None -> assert false


  (*méthode pour descendre le capteur; vitesse neg pour descendre*)
  let go_down _ =
    Motor.set C.conn_scan motor_captor_vert (Motor.speed (-10));
    Motor.set C.conn_scan motor_captor_l (Motor.speed (-13));
    Motor.set C.conn_scan motor_captor_r (Motor.speed (-13))

  (*méthode pour monter le capteur; vitesse pos pour monter*)
  let go_up _ =
    Motor.set C.conn_scan motor_captor_vert (Motor.speed 10);
    Motor.set C.conn_scan motor_captor_l (Motor.speed 20);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 20)

 (*méthode pour déplacer le capteur vers la droite*)
  let go_right _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (13));
    Motor.set C.conn_scan motor_captor_r (Motor.speed (-12))

  (*méthode pour déplacer le capteur vers la gauche*)
  let go_left _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (-13));
    Motor.set C.conn_scan motor_captor_r (Motor.speed 12)

  (*attendre d'avoir fait le déplacement vers la droite : c'est d'abord le
    moteur gauche qui a fini sa rotation puis le droit*)
  let wait_trans_right_r deg_new_pos_r f _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0);
    Robot.event meas_right (when_some (fun d -> d <= deg_new_pos_r))
     (fun _ -> scan_light f)

  let wait_trans_right_l (_, deg_new_pos_l, deg_new_pos_r) f =
    Robot.event meas_left (when_some (fun d -> d >= deg_new_pos_l))
      (wait_trans_right_r deg_new_pos_r f)


  (*attendre d'avoir fait le déplacement vers la gauche*)
  let wait_trans_left_l deg_new_pos_l f _ =
    Motor.set C.conn_scan motor_captor_r (Motor.speed 0);
    Robot.event meas_left (when_some (fun d -> d <= deg_new_pos_l))
      (fun _ -> scan_light f)

  let wait_trans_left_r (_, deg_new_pos_l, deg_new_pos_r)  f =
    Robot.event meas_right (when_some (fun d -> d >= deg_new_pos_r))
      (wait_trans_left_l deg_new_pos_l f)


  (*déplacement horizontal pr etre ds la bonne colonne*)
  let trans_hor diff_col deg_new_pos f _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    if diff_col <> 0 then
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
    else scan_light f


  (*condition d'arret de l et r qd le mobile descend*)
  let wait_r_l_down_end angle_down diff_col deg_new_pos f _ =
    let st = {Motor.speed = 0; motor_on = true; brake = true;
              regulation = `Idle; turn_ratio = 100;
              run_state = `Ramp_down; tach_limit = 30} in
    Motor.set C.conn_scan motor_captor_r st;
    Motor.set C.conn_scan motor_captor_l st;

    Robot.event meas_right (when_some (fun d -> d <= angle_down + adjust_r))
      (trans_hor diff_col deg_new_pos f)



  (*les moteurs l et r font presque tout à grande vitesse puis décélère sur
    les 30 derniers degrés*)
  let wait_r_l_down angle_down diff_col deg_new_pos f _ =
    Motor.set C.conn_scan motor_captor_vert (Motor.speed 0);
    Robot.event meas_right (when_some (fun d -> d <= angle_down+ adjust_r + 20))
     (wait_r_l_down_end angle_down diff_col deg_new_pos f)


  let wait_vert_down angle_down angle_up diff_col deg_new_pos f =
    Robot.event meas_vert (when_some (fun d -> d <= angle_up + adjust_v))
      (wait_r_l_down angle_down diff_col deg_new_pos f)



  (*condition d'arret de vert qd le mobile monte*)
  let wait_vert_up_end angle_up diff_col deg_new_pos f _ =
    let st = { Motor.speed = 0; motor_on = true; brake = true;
               regulation = `Idle; turn_ratio = 100;
               run_state = `Ramp_down; tach_limit = 40 } in
    Motor.set C.conn_scan motor_captor_vert st;
    Robot.event meas_vert (when_some (fun d -> d >= angle_up))
      (trans_hor diff_col deg_new_pos f)


  (* le moteur vert monte vite d'abord et décélère pdt les 30 derniers degrés *)
  let wait_vert_up angle_up diff_col deg_new_pos f _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 0);
    Robot.event meas_vert (when_some (fun d -> d >= angle_up-30))
      (wait_vert_up_end angle_up diff_col deg_new_pos f)



  (*attente du travail des moteurs l et r*)
  let wait_r_l_up angle_down angle_up diff_col deg_new_pos f =
    Robot.event meas_right (when_some (fun d -> d >= angle_down))
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



  let rec scan_game next =
    if !go_to_next then
      (
        go_to_next := false;
        next_col := -1;
        printf"passe à next\n%!";
        next !col_had_play
      )
    else
      (
        go_to_next := not !light;

        if !next_col < 6 then
          (
            next_col := !next_col + 1;
            next_line := piece_in_col !next_col !current_game;
            if !next_line = 6 then
                scan_game next
            else
              (
                printf"%i\n%i\n%!" !next_line !next_col;
                scan_case !next_line !next_col (fun () -> scan_game next)
              )
          )
        else
          (
            next_col := 0;
            next_line := piece_in_col !next_col !current_game;
            scan_case !next_line !next_col (fun () -> scan_game next)
          )
      )

  let scan col_new_piece next =
    if col_new_piece <> -1 then
      (
        add_piece col_new_piece !current_game;
        printf "%i\n%!" !current_game;
      );
    scan_game next


  let run () =
    scan 0 (fun c -> printf "%i\n%!" c); (* qd il a fini, il affiche où l'autre
                                         a joué*)
    Robot.run C.r

end
