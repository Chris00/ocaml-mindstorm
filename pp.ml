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

let lwt =
  let data = read_all "setup.data" in
  let data = Str.split (Str.regexp "[\r\n]") data in
  List.exists (fun l -> String.compare l "lwt=\"true\"" = 0) data

let () =
  let pp = Filename.concat "src" "mindstorm_NXT.pp.mli" in
  substitute pp (Filename.concat "src" "mindstorm_NXT.mli")
    [" +LWT", "";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", "\\2";
    ];
  let lwt_intf = (Filename.concat "src" "mindstorm_NXT_lwt.mli") in
  if lwt then
    substitute pp lwt_intf
      [" +LWT", " Lwt.t";
       " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", " \\1";
       "Mindstorm\\.NXT", "Mindstorm.NXT_lwt";
      ]
  else
    (* Make an empty .mli *)
    write lwt_intf
      "(* The package Lwt was not detected so the module\n   \
       Mindstorm.NXT_lwt was not compiled. *)"
