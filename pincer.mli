module Run(C: sig val conn2 : Mindstorm.bluetooth Mindstorm.conn end) :
sig
  val put_piece_in_pincer : (unit -> unit) -> 'g -> unit
    (**[put_piece_in_pincer] put a piece in the pincer *)

  val go_pincer : int -> int -> unit
    (** [go_pincer r dir] advance the pincer of [r] degrees in the direction
        [dir] *)

  val put_piece : int  -> (unit -> unit)  -> 'a ->  unit
    (** [put_piece col next] put the piece in the column [col] starting from the
        initial position, return to the starting position, put a piece in
        the pincer and do [w] *)
end
