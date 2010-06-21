(*The possibilities of combinaison between the rules*)
val rulecombo : int array array

(*Exception when 2 rules are not compatible*)
exception Combinaison_error

(*Checks if the the solutions p1 and p2 of the current board are overlapping*) 
val overlap : Structure.board -> int -> int -> bool

(*Checks if the solutions p1 and p2 of the current board are compatible*)
val column_wdoe : Structure.board -> int -> int -> bool

(*Checks if the solutions p1 and p2 have a common solution for the current board*)
val comp_rules : Structure.board -> int -> int -> bool

(*Build the Adjacentry Matrix for the current board*)
val build_adjacency_matrix : Structure.board -> bool array array
