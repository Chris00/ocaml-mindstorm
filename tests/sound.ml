open Printf

let bt = "00:16:53:03:A5:32"
let soundfile = "Woops.rso"

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  printf "Connected!\n%!";
  Mindstorm.Sound.play_tone conn 800 500;
  Unix.sleep 1;
  Mindstorm.Sound.play_tone conn 200 500;
  printf "Tone played.\n%!";
  Unix.sleep 1;
  Mindstorm.Sound.play conn soundfile ~loop:true;
  printf "Sound %S playing... %!" soundfile;
  Unix.sleep 3;
  Mindstorm.Sound.stop conn;
  printf "done.\n%!"

