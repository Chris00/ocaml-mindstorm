open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.connect_bluetooth bt in
  printf "Files on the brick:\n%!";
  let it = Mindstorm.Find.patt conn "*.*" in
  try
    while true do
      printf " - %-20S  %-5i bytes\n%!"
        (Mindstorm.Find.current it) (Mindstorm.Find.current_size it);
      Mindstorm.Find.next it;
    done
  with Mindstorm.File_not_found -> ()
