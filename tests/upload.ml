open Format

let fname = "OCaml.ml"
let data = "Uploaded by OCaml!"

let bt =
  if Array.length Sys.argv < 2 then begin
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  end
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.connect_bluetooth bt in

  let fh = Mindstorm.open_out conn (`File (String.length data)) fname in
  let w = Mindstorm.output fh data 0 (String.length data) in
  printf "Wrote %i bytes to %S.\n%!" w fname;
  Mindstorm.close_out fh;

  let fh = Mindstorm.open_in conn fname in
  let len = Mindstorm.in_channel_length fh in
  let s = String.create len in
  let r = Mindstorm.input fh s 0 len in
  printf "Contents (%i bytes): %S\n%!" r s;
  Mindstorm.close_in fh;

  printf "@[<2>Files on the brick: ";
  Mindstorm.Find.iter conn "*.*"  ~f:begin fun name _ ->
    printf "%S  @," name;
  end;
  printf "@]\n%!";

  Mindstorm.remove conn fname
