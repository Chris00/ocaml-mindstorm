type t
  (** A mutable value representing the state of the game. *)

val make : unit -> t
  (** Create a new game, with all slots [Empty]. *)

val copy : t -> t
  (** [copy g] returns a copy of the game state [g]. *)

val get_row : t -> int -> int
  (** [get_raw g col] returns the row index of the [col]th column
      where there is a token *)

val get : t -> int -> int -> int
  (** [get g row col] returns the content of the slot at the [row]th
      line (the bottom one being numbered [0]) and the [col]th column
      (the leftmost one having index [0]) in the game state [g].
      (Yellow, Red and Empty are respectively represented by 1, 0 and 2) *)

exception Column_full
  (** Raised to indicate that one tries to add a piece to a full
      column. *)

val move : t -> int -> int -> unit
  (** [move g col color] modifiy [g] by adding [color] to the column
      [col].  If the column is full, raise [Column_full]. *)

val reset : t -> unit
  (** [reset game] reinitialize the game (all slots being [Empty]). *)

val is_winning : t -> int -> bool
  (** [is_winning game]  *)
