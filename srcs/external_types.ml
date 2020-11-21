

type parser_t =
  | Expr of Solver.expr
  | Equation of string * Solver.expr
  | Function of string * string * Solver.expr
