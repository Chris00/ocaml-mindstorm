module Run(C: sig val conn_pincer : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) :
sig

  val go_pincer : int -> int -> unit
    (** [go_pincer r dir] advance the pincer of [r] degrees in the direction
        [dir] *)

  val put_piece : int  -> (unit -> unit) -> unit
    (** [put_piece col next] put the piece in the column [col] starting from the
        initial position, return to the starting position, put a piece in
        the pincer and do [w] *)
    (**rem : la colonne 6 est celle près du distributeur*)

  val run : unit -> unit
    (** Starts the event loop associated with the pincer. *)

  val stop : unit -> unit
    (** Stop all motors associated with the pincer. *)
end
