exception Error
type info = { mutable max_tree_depth : int; mutable bestmove : int; }

type node

(*Prints the position stroed in a node*)
val show : node -> unit

(*Usual binary tree*)
type bintree = {
  mutable parent : bintree option;
  mutable lson : bintree option;
  mutable rson : bintree option;
  mutable node : node;
}

(*Create an empty node*)
val make_node : unit -> node

(*The current node, shared for all the functions*)
val rootnode : node ref

(*The current board, shared for all the functions*)
val auxboard : Structure.board

(*The number of nodes expanded, shared for all the functions*)
val her_node_not_expanded : int ref

(*The number of nodes not expanded, shared for all the functions*)
val her_node_expanded : int ref

(*A better sequence the check the different children*)
val nodeseq : int array

(*Creates a copy of a node*)
val copy : node -> node

(*Generate all the possible children of a node*)
val her_generate_all_children : node -> unit

(*Calculate the best answer with an heruistic for the current board,
for a given side and a maximum depth*)
val explore_tree : Structure.board -> int -> int -> int

(*Evaluates a node*)
val her_evaluate : node -> int

(*Sets the proof and disproof numbers of the node according
to his number of children*)
val her_set_pdv_according_to_children : node -> unit

(*Sets the proof and disproof numbers of the node accoring
to the proof and disproof numbers of his children*) 
val her_set_proof_and_disproof_numbers : node -> unit

(*Checks if the number of nodes visited is higher than a fixed limit*)
val her_ressources_available : int -> bool

(*Returns the node with the lowest proof needed to be proved, stores
the next move in info for a maximum depth*)
val her_select_most_proving_node : node -> info -> int -> node

(*Expand the node and sets his proof and disproof numbers*)
val her_develop_node : node -> unit

(*Update the proof and disproof numbers of the ancestors of the node*)
val her_update_ancestors : node -> unit

(*Tries to find a move, stored in info under a maximum of maxnodenum
nodes visited*)
val her_pn_search : node -> int -> info -> unit

(*Return the next best move if one move is proved to be winning
of if all the moves are proved to be losing, -1 if nothing
is proved with a maximum of maxnodenum nodes visited*)
val heuristic_play_best : Structure.board -> int -> int
