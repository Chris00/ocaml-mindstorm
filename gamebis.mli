
type slot_content = Red | Yellow | Empty
    (** A grid hole can be either [Empty] of occupied by a [Red] or
        [Yellow] piece. *)

type couple_current_piece = {
  mutable current_piece : slot_content;
  mutable tab_line_piece : int array}

type event = { col : int; line : int; piece : slot_content; }

type t
  (** A mutable value representing the state of the game. *)

val make : unit -> t
  (** Create a new game, with all slots [Empty]. *)

val copy : t -> t
  (** [copy g] returns a copy of the game state [g]. *)

val get : t -> int -> int -> slot_content
  (** [get g row col] returns the content of the slot at the [row]th
      line (the bottom one being numbered [0]) and the [col]th column
      (the leftmost one having index [0]) in the game state [g]. *)

exception Column_full
  (** Raised to indicate that one tries to add a piece to a full
      column. *)

val move : t -> int -> slot_content -> unit
  (** [move g col piece] modifiy [g] by adding [piece] to the column
      [col].  If the column is full, raise [Column_full]. *)

val reset : t -> unit
  (** [reset game] reinitialize the game (all slots being [Empty]). *)

val is_winning : t -> bool
  (** [is_winning game]  *)


val number_of_moves : t -> int
  (** Returns the number of moves to go from the initial state of the
      game to the current one. *)

val last_move_col : t -> int
val last_move : t -> event
val npieces_col : t -> int -> int
val get_tab :  t -> slot_content array array
val get_tab_line_piece : t -> int -> int -> int array
  (* Should not be here ?? *)
