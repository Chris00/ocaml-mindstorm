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


let shift_h = 208
let shift_up_v = 131
let shift_up_r = 184
let shift_up_l = 184


(* angle de rotation de moteur_captor_vert pr la ligne [l] *)
let rot_v l =
  shift_up_v * l

(*angle de rotation de moteur_captor_r pr la ligne [l], colonne [c]*)
let rot_r l c =
  (shift_up_r*l)+(c*shift_h)

(*angle de rotation de moteur_captor_l pr la ligne [l], colonne [c]*)
let rot_l l c =
  (shift_up_l*l)-(c*shift_h)



(*tab des vitesses des trois moteurs (vert, r, l) lors des déplacements vers la
  gauche. pr aller à droite vitesse, le tableau nous donne (vert, l, r)*)
(*speed_up.(i).(j) nous donne les vitesses des moteurs (vert, r, l) lorsqu'on
  doit se déplacer de i lignes vers le haut et de j col vers la gauche*)
let speed_up =
  [|
    [|(0, 0, 0); (0, 15, -14); (0, 15, -14); (0, 15, -14);
      (0, 15, -14); (0, 15, -14); (0, 15, -14)|];
    [|(11, 18, 18); (8, 22, -2); (4, 21, -10); (3, 21, -13);
      (3, 22, -14); (2, 18, -14); (2, 22, -17)|];
    [|(11, 18, 18); (9, 20, 5); (8, 22, -2); (6, 22, -5);
      (4, 21, -11); (4, 22, -11); (3, 23, -15)|];
    [|(11, 17, 17); (10, 20, 8); (10, 24, 4); (8, 23, -2);
      (7, 25, -6); (6, 24, -7); (5, 25, -11)|];
    [|(11, 17, 17); (10, 20, 10); (9, 20, 5); (9, 24, 2);
      (7, 22, -2); (7, 25, -6); (6, 24, -6)|];
    [|(11, 17, 17); (9, 15, 13); (8, 16, 8); (8, 19, 4);
      (8, 21, 2); (7, 21, -2); (6, 20, -3)|]
  |]


(* meme chose mais vers le bas *)
let speed_down =
  [|
    [|(0, 0, 0); (0, 15, -14); (0, 15, -14); (0, 15, -14);
      (0, 15, -14); (0, 15, -14); (0, 15, -14)|];
    [|(-10, -13, -13); (-9, 2, -22); (-7, 7, -18); (-3, 9, -17);
      (-4, 14, -24); (-3, 16, -22); (-2, 15, -20)|];
    [|(-10, -13, -13); (-10, -5, -19); (-9, 2, -22); (-6, 6, -20);
      (-5, 7, -18); (-4, 11, -20); (-4, 12, -21)|];
    [|(-10, -13, -13); (-10, -8, -20); (-9, -3, -22); (-9, 1, -22);
      (-7, 4, -23); (-7, 7, -21); (-5, 10, -23)|];
    [|(-10, -13, -13); (-11, -12, -19); (-9, -4, -19); (-8, -2, -22);
      (-8, 1, -24); (-6, 4, -21); (-6, 7, -23)|];
    [|(-10, -13, -13); (-9, -11, -15); (-9, -6, -17); (-9, -4, -20);
      (-8, -2, -21); (-8, 1, -24); (-7, 3, -22)|]
  |]




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
  let scan_right = ref true (*on commence par le scannage de droite à gauche*)
  let go_to_next = ref false
  let current_game = ref 0 (*representation du jeu par un entier*)

 
  let get_angle motor = let _,_,_,a = Motor.get C.conn_scan motor in a

 (*nous retourne l'angle courant du moteur droit*)
  let meas_right = Robot.meas C.r (fun () -> get_angle motor_captor_r)

  (*nous retourne l'angle courant du moteur gauche*)
  let meas_left = Robot.meas C.r (fun () ->  get_angle motor_captor_l)

 (*nous retourne l'angle courant du moteur vertical*)
  let meas_vert = Robot.meas C.r (fun () -> get_angle motor_captor_vert)


  let stop i =
    Motor.set C.conn_scan Motor.all (Motor.speed 0)


  (*methode retournant le nbre de pions ds la col [col] du jeu [game]*)
  let piece_in_col col game =
    (game/expo_10.(col)) mod 10


  (*methode ajoutant une piece au jeu [game] en colonne [col]*)
  let add_piece col game =
    current_game := game + expo_10.(col)




  (* retourne la couleur devant le capteur*)
  let rec scan_light f =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    if !light then
      (
        Mindstorm.Sensor.set C.conn_scan color_port `Color_full `Pct_full_scale;
        usleep 0.25;
        let data  = Mindstorm.Sensor.get C.conn_scan color_port in
        let color_of_data data = match Sensor.color_of_data data with
          | `Black  -> -1 | `Blue -> 2 | `Green -> -1
          | `Yellow -> 1 | `Red  -> 0 | `White -> -1 in

        let color = color_of_data data in
        Mindstorm.Sensor.set C.conn_scan color_port `No_sensor `Raw;
        usleep 0.25;

        if (color = (-1)) then
          f () (*rappelle la fct scan_game qui appelera scan_case sur la
                   prochaine case*)
        else if (color = 2) then
          (
            printf "Ajustement\n%!";
            adjustment !current_line !current_col f
          )
        else
          (
            add_piece !current_col !current_game;
            col_had_play := !current_col; (*sera placé en param de la fct next*)
            printf "l'humain a joué dans la colonne : ";
            printf "%i\n%!" !col_had_play;
            printf "le jeu courant est : ";
            printf "%i\n%!" !current_game;
            light := false; (*pr ne pas rescanner*)
            if !current_col > 3 then
              (
                next_col := 7;
                scan_right := false
                  (*pr que scan_game renvoie le capteur à gauche du jeu*)
              )
            else
              (
                next_col := -1;
                scan_right := true
                  (*pr que scan_game renvoie le capteur à droite du jeu*)
              );
            f ()
          )
      )
    else
      (
        if !current_col = 6 then next_col := 7
        else next_col := -1;
        light := true;
        f ()
      )



  (*ajuste la position du capteur couleur*)
  and adj_l_l angle_l f =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (-5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l)
      (fun _ -> scan_light f)

  and adj_l_r angle_l f =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d >= angle_l)
      (fun _ -> scan_light f)

  and adj_l angle_l f _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 0);
    match (Robot.read meas_left) with
    |Some m ->
       (
         if m <= angle_l then
            adj_l_r angle_l f
         else adj_l_l angle_l f
        )
    |None -> assert false

  and adj_r_r angle_r angle_l f =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (5));
    Motor.set C.conn_scan motor_captor_r (Motor.speed (-5));
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= angle_r)
      (adj_l angle_l f)

  and adj_r_l angle_r angle_l f =
    Motor.set C.conn_scan motor_captor_l (Motor.speed (-5));
    Motor.set C.conn_scan motor_captor_r (Motor.speed (5));
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d >= angle_r)
      (adj_l angle_l f)

  and adj_r angle_r angle_l f _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    match (Robot.read meas_right) with
     |Some m  ->
        (
          if m < angle_r then
            adj_r_l angle_r angle_l f
          else adj_r_r angle_r angle_l f
        )
     |None -> assert false


  and adj_vert_down angle_v angle_r angle_l f =
    Motor.set C.conn_scan motor_captor_vert (Motor.speed (-5));
    Motor.set C.conn_scan motor_captor_l (Motor.speed (-7));
    Motor.set C.conn_scan motor_captor_r (Motor.speed (-7));
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_v)
      (adj_r angle_r angle_l f)



  and adj_vert_up angle_v angle_r angle_l f =
    Motor.set C.conn_scan motor_captor_vert (Motor.speed 5);
    Motor.set C.conn_scan motor_captor_l (Motor.speed 7);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 7);
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
  (* let wait_up_end angle_v f _ = *)
  (*     let st = {speed = 0; motor_on = true; brake = true; *)
  (*               regulation = `Idle; turn_ratio = 100; *)
  (*             run_state = `Ramp_down; tach_limit = 30} in *)
  (*     Motor.set C.conn_scan motor_captor_vert st; *)
  (*     Robot.event meas_vert (function *)
  (*                            |None -> false *)
  (*                            |Some d -> d >= angle_v) *)
  (*       (fun _ -> scan_light f) *)

  let wait_up angle_v f =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v)
      (* (wait_up_end angle_v f) *)
      (fun _ -> scan_light f)


  (*attend d'avoir fini ac le moteur r (ac ralenti) avant de scanner la case*)
  (*  let wait_down_right_end angle_r f _ = *)
  (*     let st = {speed = 0; motor_on = true; brake = true; *)
  (*               regulation = `Idle; turn_ratio = 100; *)
  (*               run_state = `Ramp_down; tach_limit = 30} in *)
  (*     Motor.set C.conn motor_captor_r st; *)
  (*     Robot.event meas_right (function *)
  (*                            |None -> false *)
  (*                            |Some d -> d <= angle_r) *)
  (*       (fun _ -> scan_light f) *)

  let wait_down_right angle_r f =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= angle_r)
      (* (wait_down_right_end angle_r f) *)
     (fun _ -> scan_light f)


  (*attend d'avoir fini ac le moteur l (ac ralenti) avant de scanner la case*)
  (* let wait_down_left_end angle_l f _ = *)
  (*     let st = {speed = 0; motor_on = true; brake = true; *)
  (*               regulation = `Idle; turn_ratio = 100; *)
  (*               run_state = `Ramp_down; tach_limit = 40} in *)
  (*     Motor.set C.conn motor_captor_l st; *)
  (*     Robot.event meas_right (function *)
  (*                            |None -> false *)
  (*                            |Some d -> d <= angle_l+5) *)
  (*       (fun _ -> scan_light f) *)


  let wait_down_left angle_l f =
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l)
      (* (wait_down_left_end angle_l f) *)
      (fun _ -> scan_light f)


  let rec scan_case new_pos_line new_pos_col f =
    let diff_line = new_pos_line - !current_line and
        diff_col = new_pos_col - !current_col and
        angle_v = rot_v new_pos_line and
        angle_r = rot_r new_pos_line new_pos_col and
        angle_l = rot_l new_pos_line new_pos_col in

    current_line := new_pos_line;
    current_col := new_pos_col;

    if(diff_line = 0 && diff_col = 0) then
      scan_light f
    else
      (
        (*on fait les deux mouvements en meme temps*)
        if (diff_line > 0) then (*on va vers le haut*)
          (
            let speed_vert, speed_m1, speed_m2 = speed_up.(diff_line).
              (abs(diff_col)) in
            (match (Robot.read meas_vert) with
             |Some m_v  ->
                (
                  Motor.set C.conn_scan motor_captor_vert (Motor.speed
                                                             speed_vert);
                  wait_up angle_v f
                )
             |None -> assert false);


            if (diff_col >= 0) then (*on va vers la gauche*)
              (
                (match (Robot.read meas_right) with
                 |Some m_r  ->
                    Motor.set C.conn_scan motor_captor_r (Motor.speed
                          ~tach_limit: (abs(angle_r - m_r)) speed_m1)
                 |None -> assert false);

                (match (Robot.read meas_left) with
                 |Some m_l  ->
                    Motor.set C.conn_scan motor_captor_l (Motor.speed
                        ~tach_limit: (abs(angle_l - m_l)) speed_m2)
                 |None -> assert false)
              )
            else (*on va vers la droite*)
              (

                (match (Robot.read meas_right) with
                 |Some m_r  ->
                    Motor.set C.conn_scan motor_captor_r (Motor.speed
                         ~tach_limit: (abs(angle_r - m_r)) speed_m2)
                 |None -> assert false);


                (match (Robot.read meas_left) with
                 |Some m_l  ->
                    Motor.set C.conn_scan motor_captor_l (Motor.speed
                           ~tach_limit: (abs(angle_l - m_l)) speed_m1)
                 |None -> assert false)
              )
          )

        else (*on descend*)
          (
            let speed_vert, speed_m1, speed_m2 = speed_down.(-diff_line).
              (abs(diff_col)) in
            (match (Robot.read meas_vert) with
             |Some m_v  ->
                Motor.set C.conn_scan motor_captor_vert (Motor.speed
                    ~tach_limit: (abs(m_v -angle_v)) speed_vert)
             |None -> assert false);


            if (diff_col >= 0) then (*on va vers la gauche*)
              (
                (match (Robot.read meas_right) with
                 |Some m_r  ->
                    Motor.set C.conn_scan motor_captor_r (Motor.speed
                          ~tach_limit: (abs(angle_r - m_r)) speed_m1)
                 |None -> assert false);

                (match (Robot.read meas_left) with
                 |Some m_l  ->
                    (
                      Motor.set C.conn_scan motor_captor_l (Motor.speed
                         speed_m2);
                      wait_down_left angle_l f
                    )
                 |None -> assert false);

              )
            else (*on va vers la droite*)
              (
                (match (Robot.read meas_right) with
                 |Some m_r  ->
                    (
                      Motor.set C.conn_scan motor_captor_r (Motor.speed
                             speed_m2);
                      wait_down_right angle_r f
                    )
                 |None -> assert false);

                (match (Robot.read meas_left) with
                 |Some m_l  ->
                    Motor.set C.conn_scan motor_captor_l (Motor.speed
                      ~tach_limit: (abs(angle_l - m_l)) speed_m1)
                 |None -> assert false);
              )
          )
      )



  let rec scan_game_right next =
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
              scan_case !next_line !next_col (fun () -> scan_game next)
          )
        else
          (
            next_col := -1;
            scan_game next
            (* next_line := piece_in_col !next_col !current_game; *)
            (* scan_case !next_line !next_col (fun () -> scan_game next) *)
          )
      )

  and scan_game_left next =
    if !go_to_next then
      (
        go_to_next := false;
        next_col := 7;
        printf"passe à next\n%!";
        next !col_had_play
      )
    else
      (
        go_to_next := not !light;

        if !next_col > 0 then
          (
            next_col := !next_col - 1;
            next_line := piece_in_col !next_col !current_game;
            if !next_line = 6 then
              scan_game next
            else
                scan_case !next_line !next_col (fun () -> scan_game next)
          )
        else
          (
            next_col := 7;
            scan_game next
          )
      )

  and scan_game next =
   if !scan_right then scan_game_right next
   else scan_game_left next


  let return_init_pos i =
    light := false;
    scan_case 0 0 stop



 let scan col_new_piece next =
   if col_new_piece <> -1 then
     (
       add_piece col_new_piece !current_game;
       printf "%i\n%!" !current_game;
     );
   scan_game next

  (* let run () = *)
    (* scan stop; *)

    (* current_col := 6; *)
    (* current_line := 5; *)
    (* scan_case 5 6 stop; *)
    (* scan_case 0 0 stop; *)

    (* adjustment 0 0 stop; *)
    (* Robot.run C.r *)

end

