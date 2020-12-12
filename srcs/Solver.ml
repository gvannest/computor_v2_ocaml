
exception ResultError of string


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
  (* | Operation of expr * expr * op *)


(* Preneur d'une meilleure option si existe. Le pb est : en OCaml chaque fonction doit renvoyer un seul type! 
Le but est de ne pas convertir les floats en complexes par ex. tant que je n'en ai pas besoin => opti 
Comment éviter le copier-coller de code verbeux ci-desous? Passer par un module intermediaire?
Functor qui ajoute eval_expr aux différents modules? *)


let rec eval_expr (e:expr) = match e with
  | Literal_float f -> begin 
    let c = Cpx.ComplexWithFloats.create_constant f in
    eval_expr (Literal_variable (Var.VariableWithComplex.create_constant c))
  end
  | Literal_complex c -> eval_expr (Literal_variable (Var.VariableWithComplex.create_constant c))
  (* | Var x -> eval_expr (Literal_variable (Var.VariableWithComplex.create_standard_var x)) *)
  | Literal_variable p -> p
  | Plus(e1, e2) -> Var.VariableWithComplex.add (eval_expr e1) (eval_expr e2)
  | Minus(e1, e2) -> Var.VariableWithComplex.sub (eval_expr e1) (eval_expr e2)
  | Times(e1, e2) -> Var.VariableWithComplex.mul (eval_expr e1) (eval_expr e2)
  | Div(e1, e2) -> Var.VariableWithComplex.div (eval_expr e1) (eval_expr e2)
  | Power(e1, e2) -> Var.VariableWithComplex.pow (eval_expr e1) (eval_expr e2)

let rec ast_to_string (e:expr) = match e with
  | Literal_float n -> Printf.sprintf "%F" n
  | Literal_complex c -> Printf.sprintf "(%s)" (Cpx.ComplexWithFloats.to_string c)
  (* | Var x -> Printf.sprintf "%s" x *)
  | Literal_variable p -> Printf.sprintf "%s" (Var.VariableWithComplex.to_string p)
  | Plus(e1, e2) -> Printf.sprintf "(%s + %s) " (ast_to_string e1) (ast_to_string e2)
  | Minus(e1, e2) -> Printf.sprintf "(%s - %s) " (ast_to_string e1) (ast_to_string e2)
  | Times(e1, e2) -> Printf.sprintf "(%s * %s) " (ast_to_string e1) (ast_to_string e2)
  | Div(e1, e2) -> Printf.sprintf "(%s / %s) " (ast_to_string e1) (ast_to_string e2)
  | Power(e1, e2) -> Printf.sprintf "(%s ^ %s) " (ast_to_string e1) (ast_to_string e2)

let rec res_to_string (res:Var.VariableWithComplex.t) =
  Printf.sprintf "%s" (Var.VariableWithComplex.to_string res)
  (* | Literal_float n -> Printf.sprintf "%F" n *)
  (* | Literal_complex c -> Printf.sprintf "%s" (Complex.FloatParamComplex.to_string c)
  | _ -> raise (ResultError "Error : Result is not final") *)
(* 
let solve (formula:expr) =
  eval_expr formula *)
