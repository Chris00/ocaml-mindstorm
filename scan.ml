open Printf
open  Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor

let switch_port = `S1
let port_light = `S2
let motor_captor_color_l = Motor.a
let motor_captor_color_r = Motor.b
let motor_captor_vert = Motor.c
let motor_open_pincer = Motor.b
let motor_pincer = Motor.a
let motor_dist = Motor.c

let dir = 1
(*angle d'ouverture de la pince*)
let open_rot = 120
  (* translation initiale*)
let init_translation = 245
  (*rotation par case*)
let case_rot = 135

module Run(C: sig val conn1 : Mindstorm.bluetooth Mindstorm.conn
                  val conn2 : Mindstorm.bluetooth Mindstorm.conn end) =
struct

  
  let last (a, b, c, d) = d
  
  let r = Robot.make()
  
  let touch = Robot.touch C.conn1 switch_port r

(*nous retourne l'angle courant du moteur distribuant les pièces*)
  let meas_dist =
    Robot.meas r (fun _ -> last (Motor.get C.conn2 motor_dist))

 (*nous retourne l'angle courant du moteur déplaçant la pince*)
  let meas_translation_pincer =
    Robot.meas r (fun _ ->  last (Motor.get C.conn2 motor_pincer))
 
 (*nous retourne l'angle courant du moteur ouvrant la pince*)
  let meas_open_pincer =
    Robot.meas r (fun _ -> last (Motor.get C.conn2 motor_open_pincer))

 
  
  let rec stop _ =
    Motor.set C.conn1 Motor.all (Motor.speed 0);
    raise Exit

  let go_vertical_init() =
    Robot.event_is touch stop;
    Motor.set C.conn1 motor_pincer  (Motor.speed (20 * dir) ~sync:true);
    Motor.set C.conn1 motor_open_pincer (Motor.speed (20 * dir) ~sync:true)


  let go_vertical() =
    reset_pos C.conn1 motor_open_pincer;
    reset_pos C.conn1 motor_pincer;
    Motor.set C.conn1 motor_pincer (Motor.speed ~tach_limit: 130 10 ~sync:true);
    Motor.set C.conn1 motor_open_pincer (Motor.speed ~tach_limit:130 10
                                           ~sync:true)


  let put_in_pincer _ =
    (Motor.set C.conn2 motor_dist (Motor.speed ~tach_limit: 60 (-10)))

  (*attendre que le distributeur de pieces prenne un piece pour ensuite la
    mettre dans la pince*)
  let wait_dist _ =
    Robot.event meas_dist (function
                           |None -> false
                           |Some d -> d > 60)
    (put_in_pincer)


  let put_piece_in_pincer _ =
    (*positif prendre la pièce*)
    (*négatif faire tomber pièce*)
    Motor.set C.conn2 motor_open_pincer(Motor.speed 0);
    Motor.set C.conn2 motor_dist (Motor.speed  10);
    wait_dist ()

  let go_pincer r dir =
    (*négatif vers réserve*)
    Motor.set C.conn2 motor_pincer (Motor.speed  ~tach_limit:r (dir*15));
    Motor.set C.conn2 motor_open_pincer((Motor.speed ~tach_limit: ((r*9)/10))
                                          (dir*15))


 (*attendre que la pince soit en position initiale pour mettre une pièce dedans*)
  let wait_init_pos col _ = 
   Robot.event meas_translation_pincer (function
                                        |None -> false
                                        |Some d -> d < 5)
    (put_piece_in_pincer) 


  let return_init_pos col _ =
    reset_pos C.conn2  motor_open_pincer;
    go_pincer (case_rot*col + init_translation + 10) (-1);
    wait_init_pos col ()

  let wait_pincer_closed col =
    Robot.event meas_open_pincer (function
                                  |None -> false
                                  |Some d -> d >= 35)
    (return_init_pos col)

    
  let close_pincer col m =
    Motor.set C.conn2 motor_open_pincer (Motor.speed 10);
    wait_pincer_closed col

  let wait_pincer_opened col = 
    Robot.event meas_open_pincer (function
                                  | None -> false
                                  | Some d -> d <= -open_rot)
      (close_pincer col)

  let open_pincer col _ =
    (*négatif pour ouvrir la pince*)
    reset_pos C.conn2 motor_open_pincer;
    Motor.set C.conn2 motor_open_pincer (Motor.speed (-10));
    wait_pincer_opened col

  let wait_open_pincer col  =
    Robot.event meas_translation_pincer (function
                                         |None -> false
                                         |Some d -> d > (case_rot*col +
                                                           init_translation))
   (open_pincer col)

  (*methode pour déposer la pièce*)
  let put_piece col =
    reset_pos C.conn2 motor_open_pincer;
    reset_pos C.conn2 motor_pincer;
    reset_pos C.conn2 motor_dist;
    go_pincer (case_rot*col + init_translation + 10) 1;
    wait_open_pincer col

  let go_captor_color_horizontal () =
    Robot.event_is touch stop;
    Motor.set C.conn1 motor_captor_color_r (Motor.speed ~tach_limit:130 (10));
    Motor.set C.conn1 motor_captor_color_l (Motor.speed ~tach_limit:130 (10))

  let go_captor_color_vertical () =
    (*positif pour monter*)
    Robot.event_is touch stop;
    Motor.set C.conn1 motor_captor_color_r(Motor.speed(5))
    

  let run col =
   put_piece col;
   (*go_captor_color_horizontal();*)
   Robot.run r

end

let () =
  let (bt1,bt2,col)=
    if Array.length Sys.argv < 4 then (
      printf "%s <bluetooth addr><bluetooth addr><co>\n" Sys.argv.(0);
      exit 1;
    )
    else (Sys.argv.(1),Sys.argv.(2),Sys.argv.(3)) in
  let conn1 = Mindstorm.connect_bluetooth bt1
  and conn2 = Mindstorm.connect_bluetooth bt2
  and column = int_of_string col in
  let module R = Run(struct let conn1 = conn1 and conn2 = conn2 end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run(column)
