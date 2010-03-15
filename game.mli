type color = Yellow | Red

val color_invers : color -> color

type t =
    { mutable cols_left : int;
      mutable cols_right : int
    }
  (** A mutable value representing the state of the game. *)

val make : unit -> t
  (** Create a new game, with all slots [Empty]. *)

val copy : t -> t
  (** [copy g] returns a copy of the game state [g]. *)

val get_color : t -> int -> int -> color option
  (** [get_color g row col] returns the content of the slot at the [row]th
      line (the bottom one being numbered [0]) and the [col]th column
      (the leftmost one having index [0]) in the game state [g]. *)

val get_row : t -> int -> int
  (** [get_raw g col] returns the row index of the [col]th column
      where there is a token *)

val nbr_token : t -> int
  (** [nbr_token g] returns the number of token of the current_game [g] *)

val comparate : t -> t -> int
  (** [comparate game other_game] return 0 si games are equal
      1 if game is greater than other_game
      -1 if game is lower than other_game *)

exception Column_full
  (** Raised to indicate that one tries to add a piece to a full
      column. *)
exception Column_empty
  (** Raised to indicate that one tries to remove a piece to a empty
      column. *)

val move : t -> int -> color -> unit
  (** [move g col color] modifiy [g] by adding [color] to the column
      [col].  If the column is full, raise [Column_full]. *)

val remove : t -> int -> color -> unit
  (** [remove g col color] modifiy [g] by remove [color] to the column
      [col]. if the column is empty, raise [column_empty]. *)

val reset : t -> unit
  (** [reset game] reinitialize the game (all slots being [Empty]). *)

val is_draw : t -> bool
  (** [is_draw game] *)

val is_winning : t -> int -> bool
  (** [is_winning game j]  *)

val next_win : t -> color -> int
  (** [next_win game color] return the column to play if the player
  can win the next move, or return 7 otherwise*)
