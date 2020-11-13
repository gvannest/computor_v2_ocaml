exception FunctionNotFoundError of string
val set_function : string -> string -> Solver.expr -> unit
(* val get_function : string -> Solver.expr

val to_string : string -> string *)

val print_state : unit -> unit