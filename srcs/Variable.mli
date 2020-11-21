exception VariableNotFoundError of string
val set_variable : string -> Solver.expr -> unit
val get_variable : string -> Complex.FloatParamComplex.t

val to_string : string -> string

val print_state : unit -> unit