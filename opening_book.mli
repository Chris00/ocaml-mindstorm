(*The first array is compressed position of 14 int, the second
array will be modified to be the corresponding position*)
val expand_block : int array -> int array -> unit

(*The first array is an usual position, the second array
will be modified to be the corresponding compression*)
val collapse_position : int array -> int array -> unit

(*Compares the first len elements of 2 arrays, using
lexical order*)
val mybincmp : 'a array -> 'a array -> int -> int

(*Checks which one of the current position of his mirror
position has the lowest compressed array and returns
the lowest one*)
val get_lower : int ref array -> int array

(*Checks if the one of the next seven moves possible current
board is stored in the in the book for the compressed position
cmparray.*)
val check_book : Board.t -> int array -> int -> bool

(*Checks if the book gives the best next move for a given
side. Returns the column if an answer is found, -1 if not*)
val use_opening_book : Board.t -> int -> int
