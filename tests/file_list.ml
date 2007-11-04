open Printf
module M = Mindstorm

let bt = "00:16:53:03:A5:32"

let () =
  let conn = M.connect_bluetooth bt in
  printf "Files on the brick:\n%!";
  let it = M.find conn "*.*" in
  try
    while true do
      printf " - %-20S  %-5i bytes\n%!" (M.filename it) (M.filesize it);
      M.next it;
    done
  with M.File_not_found -> ()
