type expr =
  | Literal_float of float
  | Literal_complex of Cpx.ComplexWithFloats.t
  | Literal_variable of Var.VariableWithComplex.t
  (* | Var of string *)
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Power of expr * expr

val eval_expr : expr -> Var.VariableWithComplex.t
val ast_to_string : expr -> string
val res_to_string : Var.VariableWithComplex.t -> string