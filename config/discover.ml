open Stdio
open Printf
module C = Configurator

let split_ws str =
  Base.(String.(split str ~on:' ' |> List.filter ~f:((<>) "")))

(* Naive libusb location detection *)
let usb_include () =
  if Caml.Sys.file_exists "/usr/include/libusb-1.0/libusb.h" then
    "/usr/include/libusb-1.0/"
  else ""

let get_flags c =
  let c_flags = if Sys.word_size = 64 then ["-DARCH64"] else [] in
  let usb_include = usb_include () in
  let sys = C.ocaml_config_var_exn c "system"in
  if sys = "linux" then
    if String.(usb_include <> "") then
      ("-DHAS_USB" :: "-I" :: usb_include :: c_flags,
       ["-lbluetooth"; "-lusb-1.0"])
    else (c_flags, ["-lbluetooth"])
  else if sys = "msvc" then
    ("/nologo" :: "/DWIN32" :: c_flags,
     ["/I\"C:\\Program Files\\Microsoft Platform SDK\\Include\""])
  else if sys = "macosx" then
    ("-DMACOSX" :: c_flags,
     ["-I"; "/System/Library/Frameworks/IOBluetooth.framework/Headers"])
  else
    C.die "System %S currently not supported.  Please contact the \
           mindstorm developers." sys

let discover c =
  let c_flags, libs = get_flags c in
  let c_flags =
    match Caml.Sys.getenv "MINDSTORM_CFLAGS" with
    | exception Not_found -> c_flags
    | alt_cflags -> split_ws alt_cflags in
  let libs =
    match Caml.Sys.getenv "MINDSTORM_LIBS" with
    | exception Not_found -> libs
    | alt_libs -> "-lm" :: split_ws alt_libs in

  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Base.Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" (Base.sexp_of_list Base.sexp_of_string c_flags);
  write_sexp "c_library_flags.sexp" (Base.sexp_of_list Base.sexp_of_string libs)

let cppo file c =
  let ocaml_version = C.ocaml_config_var_exn c "version" in
  let system = C.ocaml_config_var_exn c "system" in
  let arch = C.ocaml_config_var_exn c "architecture" in
  let has_usb = String.(usb_include () <> "") in
  let cmd = sprintf "cppo -D %s -D %s%s -V OCAML:%s %s"
                       system arch (if has_usb then " -D HAS_USB" else "")
                       ocaml_version (Filename.quote file) in
  ignore(Caml.Sys.command cmd)

let () =
  let cppo_file = ref "" in
  let specs = [
      ("--cppo", Caml.Arg.Set_string cppo_file,
       " run cppo with the right arguments")] in
  Caml.Arg.parse specs (fun _ -> raise(Caml.Arg.Bad "no anonymous arg"))
    "discover";
  Configurator.main ~name:"mindstorm"
    (if !cppo_file <> "" then cppo !cppo_file else discover)
