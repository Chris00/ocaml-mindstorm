(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules as e ->
     dep ["doc"; "docdir"; "extension:html"; "ocaml"] &
       ["doc/intro.txt"];
     flag ["doc"; "docdir"; "extension:html"; "ocaml"] &
       (S[A"-t"; A"Mindstorm user guide"; A"-colorize-code";
          A"-intro"; P"doc/intro.txt" ]);
     dispatch_default e
  | e ->
     dispatch_default e

let () =
  Ocamlbuild_plugin.dispatch dispatch
