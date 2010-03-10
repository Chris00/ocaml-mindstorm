
module Run(C: sig val conn1 : Mindstorm.bluetooth Mindstorm.conn
                  val conn2 : Mindstorm.bluetooth Mindstorm.conn end) :
sig
  val put_piece_in_pincer : 'i -> unit
    (**[put_piece_in_pincer] put a piece in the pincer *)

  val go_pincer : int -> int -> unit
    (** [go_pincer r dir] advance the pincer of [r] degrees in the direction
        [dir] *)

  val put_piece : int -> unit
    (** [put_piece col] put the piece in the column [col] starting from the
        initial position, return to the starting position and put a piece in
        the pincer. *)
end
