
type slot_content = Red | Yellow | Empty
    (** A grid hole can be either [Empty] of occupied by a [Red] or
        [Yellow] piece. *)


type t
  (** A mutable value representing the state of the game. *)

val make : unit -> t
  (** Create a new game, with all slots [Empty]. *)

val copy : t -> t
  (** [copy g] returns a copy of the game state [g]. *)

val get : t -> int -> int -> slot_content
  (** [get g i j] returns the content of the slot at the [i]th line
      (the bottom one being numbered [0]) and the [j]th column (the
      leftmost one having index [0]) in the game state [g]. *)

val move : t -> int -> slot_content -> unit

