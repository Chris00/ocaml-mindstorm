module Run(C: sig val conn_pincer : Mindstorm.bluetooth Mindstorm.conn
                  val r : Robot.t end) :
sig

  val go_pincer : int -> int -> unit
    (** [go_pincer r dir] advances the pincer of [r] degrees in the direction
        [dir] *)

  val put_piece : int  -> (unit -> unit) -> unit
    (** [put_piece col next] puts the piece in the column [col] starting from
        the initial position, returns to the starting position, puts a piece in
        the pincer and does [next] *)
    (** remark : the sixth column is close to the distributor*)
 
  val run : unit -> unit
    (** Start the event loop associated with the pincer. *)

  val stop : unit -> unit
    (** Stop all motors associated with the pincer. *)

end
