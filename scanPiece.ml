open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor

let switch_port = `S1
let port_light = `S2
let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c
let init_game = 0 (*representation du jeu par un entier*)
(*num du jeu: de 0 a 6 de gauche à droit du cote du joueur*)
let expo_10 = [| 1000000; 100000; 10000; 1000; 100; 10; 1|]

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct


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


  let rec stop _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    raise Exit

  (*dir = 1 pour monter*)
  let go_vertical dir case =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_vert (Motor.speed (7 * dir)
                                          ~tach_limit:(130*case));
    Motor.set C.conn motor_captor_l  (Motor.speed (10 * dir)
                                        ~tach_limit:(200*case) ~sync:true);
    Motor.set C.conn motor_captor_r (Motor.speed (10 * dir)
                                       ~tach_limit:(200*case) ~sync:true)

  (*dir = 1 vers le distributeur de pieces*)
  let go_horizontal dir case =
    Robot.event_is touch stop;
    Motor.set C.conn motor_captor_l (Motor.speed ((-15) * dir)
                                       ~tach_limit:(208 * case));
    Motor.set C.conn motor_captor_r (Motor.speed (15 * dir)
                                       ~tach_limit:(208 * case))


  (*methode retournant le nbre de pions ds la col i du jeu [j]*)
  let piece_in_col i j =
    (j/expo_10.(i)) mod 10

  (*methode ajoutant une piece au jeu [j] en colonne i*)
  let add_piece i j =
    j + expo_10.(i)

  let f diff_col _ =
    go_horizontal (if diff_col > 0 then 1 else (-1)) (abs(diff_col))


  let wait_vert diff_line diff_col =
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> (abs d) > abs(diff_line*130))
      (f diff_col)


  let scan_case old_pos_line old_pos_col new_pos_line new_pos_col =
    reset_pos C.conn motor_captor_vert;
    let diff_line = new_pos_line - old_pos_line and
        diff_col = new_pos_col - old_pos_col in
    go_vertical (if diff_line > 0 then 1 else (-1)) (abs(diff_line));
    wait_vert diff_line diff_col



  let run () =
    scan_case 3 4 0 0;
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
