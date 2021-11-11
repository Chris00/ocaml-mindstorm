(* Generate interfaces for standard IO and Lwt. *)

let read_all fname =
  (* Avoid Bytes for backward compatibility. *)
  let fh = open_in fname in
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let n = ref 0 in
  while n := input fh b 0 4096;  !n > 0 do
    Buffer.add_subbytes buf b 0 !n
  done;
  Buffer.contents buf

let write fname txt =
  (try Unix.chmod fname 0o666; Unix.unlink fname with _ -> ());
  let fh = open_out fname in
  output_string fh txt;
  close_out fh;
  (try Unix.chmod fname 0o466
   with Unix.Unix_error(e, _, _) ->
     prerr_endline("Warning: chmod " ^ fname ^ ": " ^ Unix.error_message e))

let substitute fname_in ~dir fname_out tr =
  if Sys.file_exists fname_in && Sys.file_exists dir then (
    let txt = read_all fname_in in
    let txt = List.fold_left (fun t (re, s) ->
                  Str.global_replace (Str.regexp re) s t) txt tr in
    write (Filename.concat dir fname_out) txt
  )

let () =
  let pp = Filename.concat "src" "mindstorm__NXT.mli.pp" in
  substitute pp ~dir:"src" "mindstorm__NXT.mli"
    [" +LWT_t", "";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", "\\2";
     "MINDSTORM\\.NXT", "Mindstorm.NXT";
    ];
  substitute pp ~dir:"lwt" "mindstorm_lwt__NXT.mli"
    [" +LWT_t", " Lwt.t";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", " \\1";
     "MINDSTORM\\.NXT", "Mindstorm_lwt.NXT";
    ]
