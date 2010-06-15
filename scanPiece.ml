open Printf
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let color_port = `S4
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c

let usleep n =
  let start = Unix.gettimeofday() in
  let rec delay t =
    try
      ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. n -. now in
      if remaining > 0.0 then delay remaining in
  delay n

let adjust_r = 27
let adjust_v = 20


let shift_h = 208
let shift_up_v = 131
let shift_up_r = 184
let shift_up_l = 184

(* Angle de rotation de moteur_captor_vert pr la ligne [l]. *)
let rot_v l =
  shift_up_v * l

(* Angle de rotation de moteur_captor_r pr la ligne [l], colonne [c]. *)
let rot_r l c =
  (shift_up_r*l)+(c*shift_h)

(* Angle de rotation de moteur_captor_l pr la ligne [l], colonne [c]. *)
let rot_l l c =
  (shift_up_l*l)-(c*shift_h)

let when_some cond v = match v with
  | None -> false
  | Some d -> cond d


(* Tableau des vitesses des 3 moteurs (vert, r, l) lors des déplacements vers la
  gauche. Pour aller a droite vitesse, le tableau nous donne (vert, l, r). *)
(* speed_up.(i).(j) nous donne les vitesses des moteurs (vert, r, l) lorsqu'on
  doit se deplacer de i lignes vers le haut et de j col vers la gauche. *)
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


(* Meme chose mais vers le bas. *)
let speed_down =
  [|
    [|(0, 0, 0); (0, 20, -19); (0, 20, -19); (0, 20, -19);
      (0, 20, -19); (0, 20, -19); (0, 20, -19)|];
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

  let scan_right = ref true (* On commence par le scannage de droite a gauche.*)
  let number_piece = Array.make 7 0
    (* Nombre de pieces par colonne. Utile pour savoir ou scanner. *)
  let get_angle motor = let _,_,_,a = Motor.get C.conn_scan motor in a

  (* Nous retourne l'angle courant du moteur droit. *)
  let meas_right = Robot.meas C.r (fun () -> get_angle motor_captor_r)

  (* Nous retourne l'angle courant du moteur gauche. *)
  let meas_left = Robot.meas C.r (fun () ->  get_angle motor_captor_l)

  (* Nous retourne l'angle courant du moteur vertical. *)
  let meas_vert = Robot.meas C.r (fun () -> get_angle motor_captor_vert)


  let stop _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0)

  (* Methode retournant le nbre de pions ds la col [col] du jeu [game]. *)
  let piece_in_col col = number_piece.(col)

  (* Methode ajoutant une piece au jeu [game] en colonne [col]. *)
  let add_piece col = number_piece.(col) <- number_piece.(col) + 1

  let pieces_per_col() =
    String.concat "; " (Array.to_list (Array.map string_of_int number_piece))

  let stop_motors_and_do f _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    f()

  (* Ajuste la position du capteur couleur. *)
  let adj_l angle_l f _ =
    Motor.set C.conn_scan motor_captor_l (Motor.speed 0);
    Motor.set C.conn_scan motor_captor_r (Motor.speed 0);
    match Robot.read meas_left with
    | Some m ->
        let speed, good_angle =
          if m <= angle_l then 8, (fun a -> a >= angle_l)
          else -8, (fun a -> a <= angle_l) in
        Motor.set C.conn_scan motor_captor_l (Motor.speed speed);
        Robot.event meas_left (when_some good_angle) (stop_motors_and_do f)
    | None -> assert false

  let adj_r angle_r angle_l f _ =
    Motor.set C.conn_scan Motor.all (Motor.speed 0);
    match Robot.read meas_right with
    | Some m  ->
        let speed_r, good_angle =
          if m < angle_r then 8, (fun a -> a >= angle_r)
          else -8, (fun a -> a <= angle_r) in
        Motor.set C.conn_scan motor_captor_r (Motor.speed speed_r);
        Motor.set C.conn_scan motor_captor_l (Motor.speed (- speed_r));
        Robot.event meas_right (when_some good_angle) (adj_l angle_l f)
    | None -> assert false

  (* Ajustement de la position du capteur. *)
  (* On ajuste d'abord le moteur vert a petite vitesse puis le r puis le l. *)
  let adjustment line col f =
    let angle_v = rot_v line and
        angle_r = rot_r line col and
        angle_l = rot_l line col in
    match Robot.read meas_vert with
    | Some m  ->
        let speed_vert, speed, good_angle =
          if m <= angle_v then 6, 8, (fun a -> a >= angle_v) (* go up *)
          else -6, -8, (fun a -> a <= angle_v) in
        Motor.set C.conn_scan motor_captor_vert (Motor.speed speed_vert);
        Motor.set C.conn_scan motor_captor_l (Motor.speed speed);
        Motor.set C.conn_scan motor_captor_r (Motor.speed speed);
        Robot.event meas_vert (when_some good_angle) (adj_r angle_r angle_l f)
    | None -> assert false

  (* Scanne la case, si c'est une piece (couleur jaune ou rouge), il rescanne
    une 2eme fois pr etre sur du resultat, si c'est bleu, il se reajuste,
    sinon il continue *)
  let rec scan_light ?(count_col = 0) ?(count_inv_arg = 0) f =
    Mindstorm.Sensor.set C.conn_scan color_port `Color_full `Pct_full_scale;
    usleep 0.25;
    try
      let data  = Mindstorm.Sensor.get C.conn_scan color_port in
      let color = Sensor.color_of_data data in
      Mindstorm.Sensor.set C.conn_scan color_port `No_sensor `Raw;
      match color with
      | `Black | `Green | `White ->
          f None (* Rappelle la fct [scan] qui appelera move_to sur la
                    prochaine case. *)
      | `Blue ->
          adjustment !current_line !current_col (fun _ -> scan_light f)
      | `Yellow | `Red ->
          (* Il a trouve une piece. *)
          if count_col = 0 then scan_light ~count_col:1 f
          else
            (
              add_piece !current_col;
              f (Some !current_col)
            )
    with Invalid_argument msg ->
      printf "RAISED Invalid_argument(%S)\n%!" msg;
      if count_inv_arg = 3 then (
        Mindstorm.Sensor.set C.conn_scan color_port `No_sensor `Raw;
        f None
      )
      else scan_light ~count_inv_arg:(count_inv_arg + 1) f


  let wait_up angle_v f =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v) (stop_motors_and_do f)

  let wait_down_right angle_r f =
    Robot.event meas_right (function
                            |None -> false
                            |Some d -> d <= angle_r) (stop_motors_and_do f)

  let wait_down_left angle_l f =
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l) (stop_motors_and_do f)

  (* Move to the desired position, update the (position) state, and
     execute [f]. *)
  let move_to new_pos_line new_pos_col f =
    let diff_line = new_pos_line - !current_line and
        diff_col = new_pos_col - !current_col and
        angle_v = rot_v new_pos_line and
        angle_r = rot_r new_pos_line new_pos_col and
        angle_l = rot_l new_pos_line new_pos_col in

    current_line := new_pos_line;
    current_col := new_pos_col;
    if diff_line = 0 && diff_col = 0 then f()
    else (
      (* on fait les deux mouvements en meme temps *)
      if diff_line > 0 then ( (* on va vers le haut *)
        let speed_vert, speed_m1, speed_m2 =
          speed_up.(diff_line).(abs(diff_col)) in
        (match Robot.read meas_vert with
         | Some m_v  ->
             Motor.set C.conn_scan motor_captor_vert (Motor.speed speed_vert);
             wait_up angle_v f
         | None -> assert false);
        let speed_r, speed_l =
          if diff_col >= 0 then speed_m1, speed_m2 (* on va vers la gauche *)
          else speed_m2, speed_m1  (* on va vers la droite *) in
        (match Robot.read meas_right with
         | Some m_r  ->
             Motor.set C.conn_scan motor_captor_r
               (Motor.speed ~tach_limit:(abs(angle_r - m_r)) speed_r)
         | None -> assert false);
        (match Robot.read meas_left with
         | Some m_l  ->
             Motor.set C.conn_scan motor_captor_l
               (Motor.speed ~tach_limit:(abs(angle_l - m_l)) speed_l)
         | None -> assert false)
      )
      else ( (* on descend *)
        let speed_vert, speed_m1, speed_m2 =
          speed_down.(-diff_line).(abs(diff_col)) in
        (match Robot.read meas_vert with
         | Some m_v  ->
             Motor.set C.conn_scan motor_captor_vert
               (Motor.speed ~tach_limit:(abs(m_v -angle_v)) speed_vert)
         | None -> assert false);

        if diff_col >= 0 then ( (* on va vers la gauche *)
          (match Robot.read meas_right with
           | Some m_r  ->
               Motor.set C.conn_scan motor_captor_r
                 (Motor.speed ~tach_limit:(abs(angle_r - m_r)) speed_m1)
           | None -> assert false);
          (match Robot.read meas_left with
           | Some m_l  ->
               Motor.set C.conn_scan motor_captor_l (Motor.speed speed_m2);
               wait_down_left angle_l f
           | None -> assert false);
        )
        else ( (* on va vers la droite *)
          (match Robot.read meas_right with
           | Some m_r  ->
               Motor.set C.conn_scan motor_captor_r (Motor.speed speed_m2);
               wait_down_right angle_r f
           | None -> assert false);
          (match Robot.read meas_left with
           | Some m_l  ->
               Motor.set C.conn_scan motor_captor_l
                 (Motor.speed ~tach_limit: (abs(angle_l - m_l)) speed_m1)
           | None -> assert false);
        )
      )
    )


  exception Not_full of int

  (* Deplace la pince a l'extremite du jeu la plus proche en faisant attention
     a ce que la colonne ne soit pas pleine. *)
  let go_closer_non_full_col f =
    if !current_col > 3 then
      try
        for c = 6 downto 0 do
          if piece_in_col c < 6 then raise(Not_full c)
        done;
        f() (* all columns full *)
      with Not_full c -> move_to (piece_in_col c) c f
    else
      try
        for c = 0 to 6 do
          if piece_in_col c < 6 then raise(Not_full c)
        done;
        f() (* all columns full *)
      with Not_full c -> move_to (piece_in_col c) c f

  (* Retourne la premiere colonne a gauche ou a droite qui est non pleine. *)
  let rec next_col current_col =
    let col = (if !scan_right then
                 if current_col < 6 then current_col + 1 else 0
               else
                 if current_col > 0 then current_col - 1 else 6) in
    if piece_in_col col = 6 then next_col col
    else col

  (* Calcul du chemin que le scanner doit parcourir. *)
  let compute_way_scanner () =
    let list_col = ref [] in
    let n =
      let length_tab = ref 0 in
      for i = 0 to 6 do
        if piece_in_col i < 6 then
          (
            length_tab := !length_tab + 1;
            if i <> !current_col then list_col :=  i::!list_col
          )
      done;
      !length_tab in

    let tab = Array.init n (fun _ -> !current_col) in

    let rec list_to_tab list i =
      if i < n then
        (
          tab.(i) <- List.hd list;
          list_to_tab (List.tl list) (i+1)
        )
    in list_to_tab !list_col 1;

    (* Remplissage du tableau des sommets a parcourir en utilisant l'heuristique
       plus proche voisin. Si 2 cases sont aussi proches, on favorise celle qui
       minimise la difference de colonnes/largeurs. *)
    for i = 1 to n-2 do
      let min = ref(abs(tab.(i) - tab.(i-1)) + abs(piece_in_col tab.(i)
                                                   - piece_in_col tab.(i-1)))
      and pos_min = ref i in

      for j = i+1 to n-1 do
        let dist = abs(tab.(j) - tab.(i-1)) + abs(piece_in_col tab.(j)
                                                  - piece_in_col tab.(i-1))
        and diff_col_pos_min = abs(tab.(i-1) - !pos_min)
        and diff_col_j = abs(tab.(i-1) - tab.(j)) in

        if (dist < !min) || (dist = !min && diff_col_pos_min > diff_col_j)
        then
          (
            pos_min := j;
            min := dist
          )
      done;
      let temp = tab.(i) in
      tab.(i) <- tab.(!pos_min);
      tab.(!pos_min) <- temp
    done;
    tab

  (* Scanne le jeu jusqu'a trouver un nouveau pion. Retourne la colonne dans
     laquelle le nouveau pion a ete detecte. *)
  let scan_columns f =
    let tab = compute_way_scanner () in

    let rec scan_c f pos_tab =
      scan_light begin function
      | None ->
          let new_pos =
            if pos_tab < (Array.length tab - 1)
            then pos_tab + 1
            else 0 in
          move_to (piece_in_col tab.(new_pos)) tab.(new_pos)
            (fun () -> scan_c f new_pos)
      | Some col ->
          (* un pion a ete trouve en colonne [col] *)
          go_closer_non_full_col begin fun () ->
            f col
          end
      end in scan_c f 0

  (* Ajuster en hauteur si une piece a ete ajoutee par le robot.  Si
     la colonne est pleine, on passe à la suivante. Ensuite on commence le
     scannage jusqu'a trouver une nouvelle piece. *)
  let scan f =
    let npieces = piece_in_col !current_col in
    let col = if npieces < 6 then !current_col else next_col !current_col in
    move_to (piece_in_col col) col (fun () -> scan_columns f)


  let return_init_pos f =
    move_to 0 0 f

end

