type expr =
  | Literal_float of float 
  | Literal_int of int 
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Power of expr * expr

let rec eval_expr (e:expr) = match e with
  | Literal_float n -> n
  | Literal_int n -> float_of_int n
  | Plus(e1, e2) -> Float.add (eval_expr e1) (eval_expr e2)
  | Minus(e1, e2) -> Float.sub (eval_expr e1) (eval_expr e2)
  | Times(e1, e2) -> Float.mul (eval_expr e1) (eval_expr e2)
  | Div(e1, e2) -> Float.div (eval_expr e1) (eval_expr e2)
  | Power(e1, e2) -> Float.pow (eval_expr e1) (eval_expr e2)

let rec expr_to_string (e:expr) = match e with
  | Literal_float n -> Printf.sprintf "%f" n
  | Literal_int n -> Printf.sprintf "%d" n
  | Plus(e1, e2) -> Printf.sprintf "Plus(%s, %s) " (expr_to_string e1) (expr_to_string e2)
  | Minus(e1, e2) -> Printf.sprintf "Minus(%s, %s) " (expr_to_string e1) (expr_to_string e2)
  | Times(e1, e2) -> Printf.sprintf "Times(%s, %s) " (expr_to_string e1) (expr_to_string e2)
  | Div(e1, e2) -> Printf.sprintf "Div(%s, %s) " (expr_to_string e1) (expr_to_string e2)
  | Power(e1, e2) -> Printf.sprintf "Power(%s, %s) " (expr_to_string e1) (expr_to_string e2)