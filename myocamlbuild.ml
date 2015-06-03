(* OASIS_START *)
(* OASIS_STOP *)

open Printf
open Ocamlbuild_plugin

let env = BaseEnvLight.load() (* setup.data *)

let arch = BaseEnvLight.var_get "architecture" env
let os_type = BaseEnvLight.var_get "os_type" env
let has_usb = bool_of_string(BaseEnvLight.var_get "has_usb" env)

let my_dispatch = function
  | After_rules ->
     (* C files whose dep ocamlbuild cannot infer (we cannot declare
        them in _oasis, otherwise ocamlbuild tries to compile them). *)
     dep ["c"; "compile"] ["src" / "mindstorm_unix.c";
                           "src" / "unixsupport_unix.h";
                           "src" / "mindstorm_win.c";
                           "src" / "unixsupport_win.h"];
     (* _tags cannot used in FilesAB (sic), configure preprocessing here. *)
     let pp = sprintf "cppo -D %s -D %s%s"
                      (String.uppercase os_type) (String.uppercase arch)
                      (if has_usb then " -D HAS_USB" else "") in
     let pp = S[A "-pp"; A  pp] in
     flag ["pp_mindstorm"; "ocamldep"] pp;
     flag ["pp_mindstorm"; "ocaml"; "compile"] pp;

     dep ["doc"; "docdir"; "extension:html"; "ocaml"] &
       ["doc/intro.txt"];
     flag ["doc"; "docdir"; "extension:html"; "ocaml"] &
       (S[A"-t"; A"Mindstorm user guide"; A"-colorize-code";
          A"-intro"; P"doc/intro.txt" ]);

     flag ["program"; "byte"] (A"-custom"); (* run examples w/o install *)
     flag ["compile"; "native"] (S[A"-inline"; A"10"]);
     (* mark_tag_used "tests"; (\* disable warning *\) *)
  | _ -> ()

let () =
  Ocamlbuild_plugin.dispatch
    (MyOCamlbuildBase.dispatch_combine [my_dispatch; dispatch_default])
