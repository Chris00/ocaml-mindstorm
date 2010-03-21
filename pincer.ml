(*rem : les colonnes sont numérotées de 0 à 6, la sixième étant celle près
  du distributeur*)

open Printf
open Mindstorm.Sensor
open Mindstorm.Motor
module Motor = Mindstorm.Motor

let switch_port = `S1
let motor_pincer = Motor.a
let motor_open_pincer = Motor.b
let motor_dist = Motor.c


  (*angle d'ouverture de la pince*)
let open_rot = 120
  (*rotation pr prendre une piece du jeu ds le distributeur*)
let rot_dist = 60
  (* let move_speed = 10 *)
let move_speed = 15
let open_speed = -10
let close_speed = 10

(* let rotation = [|1035; 900; 768; 635; 504; 373; 235|] vitesse 10 vitesse 12*)
let rotation = [|1033; 905; 763; 630; 499; 368; 230|]

(*ajuste l'angle à faire suivant la vitesse du moteur open pincer*)
let adjust_speed = 20   (* 4 pr une vitesse 10; 9 pr une vitesse 12 *)



module Run(C: sig val conn_pincer : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) =
struct

  let get_angle motor = let _,_,_,a = Motor.get C.conn_pincer motor in a

  (*nous retourne l'angle courant du moteur distribuant les pièces*)
  let meas_dist = Robot.meas C.r (fun () -> get_angle motor_dist)

  (*nous retourne l'angle courant du moteur déplaçant la pince*)
  let meas_translation_pincer = Robot.meas C.r (fun () ->  get_angle motor_pincer)

  (*nous retourne l'angle courant du moteur ouvrant la pince*)
  let meas_open_pincer = Robot.meas C.r (fun _ -> get_angle motor_open_pincer)


  let run () = Robot.run C.r

  let stop () =
    Motor.set C.conn_pincer Motor.all (Motor.speed 0)


  (*déplace la pince dans la direction [dir] ac une tach_limit [r]*)
  let go_pincer r dir =
    (* dir négatif vers la réserve de pièces *)
    Motor.set C.conn_pincer motor_pincer (Motor.speed  ~tach_limit:r
                                      (dir*move_speed));
    Motor.set C.conn_pincer motor_open_pincer(Motor.speed ~tach_limit:r
                                          (dir*move_speed))



  let wait_next next _ =
    Motor.set C.conn_pincer motor_dist (Motor.speed 0);
    next ()

  (*fait tomber la pièce dans la pince et attend pour lancer next*)
  let put_in_pincer next _ =
    Motor.set C.conn_pincer motor_dist (Motor.speed (-10));
    Robot.event meas_dist (function
                           |None -> false
                           |Some d -> d <= 0)
      (wait_next next)

  (*lorsque le distributeur de pièces a pris une pièce, il la fait tomber dans
    la pince*)
  let wait_dist next =
    Robot.event meas_dist (function
                           |None -> false
                           |Some d -> d > rot_dist)
      (put_in_pincer next)

  (*tourne le distributeur pr prendre une pièce*)
  let put_piece_in_pincer next _ =
    Motor.set C.conn_pincer motor_open_pincer(Motor.speed 0);
    Motor.set C.conn_pincer motor_dist (Motor.speed  10);
    wait_dist next


 (*lorsque la pince est en position initiale, le distributeur met une pièce
   dedans*)
  let wait_init_pos col next _ =
   Robot.event meas_translation_pincer (function
                                        |None -> false
                                        |Some d -> d < 15)
    (put_piece_in_pincer next)

  (*déplace la pince vers la réserve de pièces*)
  let return_init_pos col next _ =
    go_pincer (rotation.(col)) (-1);
    wait_init_pos col next ()

  (*lorsque la pince est fermée, elle retourne à sa position initiale *)
  let wait_pincer_closed col next =
    Robot.event meas_open_pincer (function
                                  |None -> false
                                  |Some d -> d >= rotation.(col) + adjust_speed)
    (return_init_pos col next)

  (*referme la pince et attend qu'elle soit fermée*)
  let close_pincer col next _ =
    Motor.set C.conn_pincer motor_open_pincer (Motor.speed close_speed);
    wait_pincer_closed col next

  (*lorsque la pince est assez ouverte, elle se referme*)
  let wait_pincer_opened col next =
    Robot.event meas_open_pincer (function
                                  | None -> false
                                  | Some d -> d <=(rotation.(col)-open_rot))
      (close_pincer col next)

  (*ouvre la pince et attend qu'elle soit assez ouverte*)
  let open_pincer col next _ =
    Motor.set C.conn_pincer motor_open_pincer (Motor.speed (open_speed));
    wait_pincer_opened col next

  (*lorsque la pince est au dessus de col, la pince s'ouvre*)
  let wait_open_pincer col next =
    Robot.event meas_translation_pincer (function
                                         |None -> false
                                         |Some d -> d >= (rotation.(col)))
   (open_pincer col next)


  (*déplace la pince et attend d'être au dessus de la colonne [col]*)
  let put_piece col next =
    go_pincer (rotation.(col)) 1;
    wait_open_pincer col next

end

