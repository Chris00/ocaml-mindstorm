(* OASIS_START *)
(* OASIS_STOP *)
#4

open Printf
open Ocamlbuild_plugin

let env = BaseEnvLight.load() (* setup.data *)

let arch = String.uppercase(BaseEnvLight.var_get "architecture" env)
let system = String.uppercase(BaseEnvLight.var_get "system" env)
let has_usb = bool_of_string(BaseEnvLight.var_get "has_usb" env)
let ocaml_version = BaseEnvLight.var_get "ocaml_version" env

let my_dispatch = function
  | After_rules ->
     (* C files whose dep ocamlbuild cannot infer (we cannot declare
        them in _oasis, otherwise ocamlbuild tries to compile them). *)
     dep ["c"; "compile"] ["src" / "mindstorm_unix.c";
                           "src" / "unixsupport_unix.h";
                           "src" / "mindstorm_win.c";
                           "src" / "unixsupport_win.h"];
     (* _tags cannot used in FilesAB (sic), configure preprocessing here. *)
     let pp = sprintf "cppo -D %s -D %s%s -V OCAML:%s"
                      system arch (if has_usb then " -D HAS_USB" else "")
                      ocaml_version in
     let pp = S[A "-pp"; A  pp] in
     flag ["pp_mindstorm"; "ocamldep"] pp;
     flag ["pp_mindstorm"; "ocaml"; "compile"] pp;

     dep ["doc"; "docdir"; "extension:html"; "ocaml"] &
       ["doc/intro.txt"];
     flag ["doc"; "docdir"; "extension:html"; "ocaml"] &
       (S[A"-t"; A"Mindstorm user guide"; A"-colorize-code";
          A"-intro"; P"doc/intro.txt";
          A"-charset"; A"utf-8" ]);
     flag ["doc"; "extension:mli"; "ocaml"] &
       (S[A"-charset"; A"utf-8";
          A"-pp"; A(sprintf "cppo -V OCAML:%s" ocaml_version) ]);

     flag ["program"; "byte"] (A"-custom"); (* run examples w/o install *)
     flag ["compile"; "native"] (S[A"-inline"; A"10"]);
     (* mark_tag_used "tests"; (\* disable warning *\) *)
  | _ -> ()

let () =
  Ocamlbuild_plugin.dispatch
    (MyOCamlbuildBase.dispatch_combine [my_dispatch; dispatch_default])
