(* Basic interface to Velena.

   Use a pipe interface for simplicity (speed is not an issue).
*)

open Printf

type level = Weak | Normal | Strong

let answer_re = Str.regexp ">.*\\([0-9]+\\)"

exception Play of int

(* WARNING: Velena numbers the columns 1..7 but we use 0..6. *)

let move_for ?(level=Strong) game =
  let l = match level with Weak -> 'a' | Normal -> 'b' | Strong -> 'c' in
  let moves = List.map (fun c -> string_of_int(c+1)) game in
  let fh = Unix.open_process_in (sprintf "echo \"%c%s0\nq\" | ./velena/veleng"
                                   l (String.concat "" moves)) in
  (* Read answer *)
  try
    while true do
      let a = input_line fh in
      (* printf "%S\n%!" a; *)
      if Str.string_match answer_re a 0 then
        raise (Play (int_of_string(Str.matched_group 1 a)));
      if a = ">?Positional error" then
        raise(Play 0)
    done;
    assert false
  with Play c ->
    if c = 0 then None else Some(c-1)

