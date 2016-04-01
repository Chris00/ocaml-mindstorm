(* Generate interfaces for standard IO and Lwt. *)

#load "str.cma";;
#load "unix.cma";;

let read_all fname =
  (* Avoid Bytes for backward compatibility. *)
  let fh = open_in fname in
  let len = in_channel_length fh in
  let b = Buffer.create len in
  Buffer.add_channel b fh len;
  Buffer.contents b

let write fname txt =
  (try Unix.unlink fname with _ -> ());
  let fh = open_out fname in
  output_string fh txt;
  close_out fh;
  Unix.chmod fname 0o444

let substitute fname_in fname_out tr =
  let txt = read_all fname_in in
  let txt = List.fold_left (fun t (re, s) ->
                Str.global_replace (Str.regexp re) s t) txt tr in
  write fname_out txt

let () =
  let pp = Filename.concat "src" "mindstorm_NXT.pp.mli" in
  substitute pp (Filename.concat "src" "mindstorm_NXT.mli")
    [" +LWT", "";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", "\\2";
    ];
  substitute pp (Filename.concat "src" "mindstorm_NXT_lwt.mli")
    [" +LWT", " Lwt.t";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", " \\1";
     "Mindstorm\\.NXT", "Mindstorm.NXT_lwt";
    ]
