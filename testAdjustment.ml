open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor
module Sensor = Mindstorm.Sensor

let motor_captor_l = Motor.a
let motor_captor_r = Motor.b
let motor_captor_vert = Motor.c

let shift_h = 204
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



  let adj_l_l angle_l =
    Motor.set C.conn motor_captor_l (Motor.speed (-5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d <= angle_l)
      stop

  let adj_l_r angle_l =
    Motor.set C.conn motor_captor_l (Motor.speed (5));
    Robot.event meas_left (function
                           |None -> false
                           |Some d -> d >= angle_l)
      stop

  let adj_l angle_l _ =
    Motor.set C.conn motor_captor_l (Motor.speed 0);
    Motor.set C.conn motor_captor_r (Motor.speed 0);
    match (Robot.read meas_left) with
     |Some m ->
        (
          if m <= angle_l then
            adj_l_r angle_l
          else adj_l_l angle_l
        )
     |None -> assert false

  let adj_r_r angle_r angle_l =
    Motor.set C.conn motor_captor_l (Motor.speed (5));
    Motor.set C.conn motor_captor_r (Motor.speed (-5));
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d <= angle_r)
      (adj_l angle_l)

  let adj_r_l angle_r angle_l =
    Motor.set C.conn motor_captor_l (Motor.speed (-5));
    Motor.set C.conn motor_captor_r (Motor.speed (5));
    Robot.event meas_right (function
                           |None -> false
                           |Some d -> d >= angle_r)
      (adj_l angle_l)

  let adj_r angle_r angle_l _ =
    Motor.set C.conn Motor.all (Motor.speed 0);
    match (Robot.read meas_right) with
     |Some m  ->
        (
          if m < angle_r then
            adj_r_l angle_r angle_l
          else adj_r_r angle_r angle_l
        )
     |None ->
       assert false


  let adj_vert_down angle_v angle_r angle_l =
    Motor.set C.conn motor_captor_vert (Motor.speed (-5));
    Motor.set C.conn motor_captor_l (Motor.speed (-7));
    Motor.set C.conn motor_captor_r (Motor.speed (-7));
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d <= angle_v)
      (adj_r angle_r angle_l)



  let adj_vert_up angle_v angle_r angle_l =
    Motor.set C.conn motor_captor_vert (Motor.speed 5);
    Motor.set C.conn motor_captor_l (Motor.speed 7);
    Motor.set C.conn motor_captor_r (Motor.speed 7);
    Robot.event meas_vert (function
                           |None -> false
                           |Some d -> d >= angle_v)
      (adj_r angle_r angle_l)


  (*ajustement de la position du capteur*)
  let adjustment angle_v angle_r angle_l =
    match (Robot.read meas_vert) with
     |Some m  ->
        (
          if m <= angle_v then
               adj_vert_up angle_v angle_r angle_l
          else adj_vert_down angle_v angle_r angle_l
        )
     |None ->
        assert false

  let run () =
    (* test pour le monter d'une case et aller une case à gauche *)
    adjustment 0 0 0;
    (* adjustment (rot_v 1) (rot_r 1 1) (rot_l 1 1); *)
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
