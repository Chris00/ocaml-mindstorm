open Printf
module S = Mindstorm.Sensor
module M = Mindstorm.Motor

let bt = "00:16:53:03:A5:32"

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  let switch = S.set comm `In1 `Switch `Raw
  and ultrasonic = S.set comm `In4 `Lowspeed_9v `Raw in
  let left = M.make conn `A and right = M.make conn `C in
  while S.get switch > 500 do
    let dist = S.get ultrasonic (* raw *) in
    let dist = min 50 (max 0 dist) in
    M.set left { M.state with M.power = dist };
    M.set right { M.state with M.power = 2 * dist - 50 };
  done;
  Mindstorm.close conn
