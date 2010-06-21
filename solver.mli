(*An array storing the names of the different rules*)
val rules_name : string array

(*A shared int storing the current solution checked*)
val tempsolused : int ref

(*Exception when no problem is found*)
exception No_problem_found

(*A problem is a possible win for the opponent. The type
contains his number, if he is solved, the possible solutions
and the number of solutions*) 
type problem = {
  mutable group : int;
  mutable solved : bool;
  solutions : int array;
  mutable solnumb : int;
}

(*A problem_list is a collection of problems*)
type problem_list = {
  mutable number : int;
  problem : problem array;
  pointer : int array;
  final : int array;
}

(*A up_solution is a collection of solutions*)
type up_solution = {
  mutable howmany : int;
  mutable which : int array;
  mutable hmprobs : int;
  mutable wprobs : int array;
}

(*Creates an object of the type problem with the parameters given*)
val make_problem : int -> bool -> int -> problem

(*Find the most difficult problem from the list for the current board
and returns a couple (a,b) a is the number of possible solutions and b
is the number of the problem*)
val find_most_difficult_problem : problem_list -> Structure.board -> int * int

(*Build the list of problems for the current board*)
val build_problem_list : Structure.board -> problem_list

(*Removes the impossible solutions from the list for the given board.
The array of array matrix contains is the solutions are possible
to be combined for the problem psol*)
val remove_solutions : problem_list -> Structure.board -> bool array array -> int -> up_solution

(*Restore the solutions if a problem is solved and doesn't destroy
a solution anymore for the current board*)
val restore_solutions : up_solution -> problem_list -> Structure.board -> unit

(*Checks the the problem list of the current board and the matrix of
combinaison has a solution*)
val solve_problem_list : problem_list -> Structure.board -> bool array array -> bool

(*Checks if the current position of the board has a solution*)
val problem_solver : Structure.board -> bool array array -> bool
