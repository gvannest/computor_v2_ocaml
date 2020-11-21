type expr =
  | Literal_float of float
  | Literal_complex of Complex.FloatParamComplex.t
  | Var of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Power of expr * expr

val eval_expr : expr -> Complex.FloatParamComplex.t
val ast_to_string : expr -> string
val res_to_string : Complex.FloatParamComplex.t -> string