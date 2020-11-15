 
exception ResultError of string

(* type op =
  | Plus of expr * expr *)
  

type expr =
  | Literal_float of float
  | Literal_complex of Complex.t
  | Var of Variable.t
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
  (* | Operation(e1,e2,oper) -> oper (eval_expr e1) (eval_expr e2) *)
  | Plus(e1, e2) -> add_expr ((eval_expr e1),(eval_expr e2))
  | Minus(e1, e2) -> sub_expr ((eval_expr e1),(eval_expr e2))
  | Times(e1, e2) -> mul_expr ((eval_expr e1),(eval_expr e2))
  | Div(e1, e2) -> div_expr ((eval_expr e1),(eval_expr e2))
  | Power(e1, e2) -> pow_expr ((eval_expr e1),(eval_expr e2))
  | Var x -> begin match Variable.get_variable x with
    | Some e -> eval_expr e
    | None -> Var x
  | n -> n

and add_expr (e1,e2) = match (e1,e2) with
  | (Literal_float n1, Literal_float n2) -> Literal_float (Float.add n1 n2)
  | (Literal_complex c1, Literal_complex c2) -> Literal_complex (Complex.add c1 c2)
  | (Literal_complex c1, Literal_float n2) -> Literal_complex (Complex.add c1 (Complex.create n2 0.0))
  | (Literal_float n1, Literal_complex c2) -> Literal_complex (Complex.add (Complex.create n1 0.0) c2)
  | (v1, v2) -> add_expr (eval_expr v1, eval_expr v2)

and sub_expr (e1,e2) = match (e1,e2) with
  | (Literal_float n1, Literal_float n2) -> Literal_float (Float.sub n1 n2)
  | (Literal_complex c1, Literal_complex c2) -> Literal_complex (Complex.sub c1 c2)
  | (Literal_complex c1, Literal_float n2) -> Literal_complex (Complex.sub c1 (Complex.create n2 0.0))
  | (Literal_float n1, Literal_complex c2) -> Literal_complex (Complex.sub (Complex.create n1 0.0) c2)
  | (v1, v2) -> sub_expr (eval_expr v1, eval_expr v2)

and mul_expr (e1,e2) = match (e1,e2) with
  | (Literal_float n1, Literal_float n2) -> Literal_float (Float.mul n1 n2)
  | (Literal_complex c1, Literal_complex c2) -> Literal_complex (Complex.mul c1 c2)
  | (Literal_complex c1, Literal_float n2) -> Literal_complex (Complex.mul c1 (Complex.create n2 0.0))
  | (Literal_float n1, Literal_complex c2) -> Literal_complex (Complex.mul (Complex.create n1 0.0) c2)
  | (Literal_float n1, Var x) | (Var x, Literal_float n1) -> Var (Variable.mul (Variable.create_var n1 "x" 0.) x)
  | (v1, v2) -> mul_expr (eval_expr v1, eval_expr v2)

and div_expr (e1,e2) = match (e1,e2) with
  | (Literal_float n1, Literal_float n2) -> Literal_float (Float.div n1 n2)
  | (Literal_complex c1, Literal_complex c2) -> Literal_complex (Complex.div c1 c2)
  | (Literal_complex c1, Literal_float n2) -> Literal_complex (Complex.div c1 (Complex.create n2 0.0))
  | (Literal_float n1, Literal_complex c2) -> Literal_complex (Complex.div (Complex.create n1 0.0) c2)
  | (Literal_float n, Var x) -> Var (Variable.div (Variable.create_var n "x" 0.0) x)
  | (Var x, Literal_float n) -> Var (Variable.div x (Variable.create_var n "x" 0.0))
  | (v1, v2) -> div_expr (eval_expr v1, eval_expr v2)

and pow_expr (e1,e2) = match (e1,e2) with
  | (Literal_float n1, Literal_float n2) -> Literal_float (Float.pow n1 n2)
  | (Literal_complex c1, Literal_float n2) -> Literal_complex (Complex.pow c1 n2)
  | (Var x, Literal_float n) -> Var (Variable.pow x n)
  | (v1, v2) -> pow_expr (eval_expr v1, eval_expr v2)

let rec ast_to_string (e:expr) = match e with
  | Literal_float n -> Printf.sprintf "%F" n
  | Literal_complex c -> Printf.sprintf "(%s)" (Complex.to_string c)
  (* | Var x -> Printf.sprintf "%s" x *)
  | Plus(e1, e2) -> Printf.sprintf "Plus(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Minus(e1, e2) -> Printf.sprintf "Minus(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Times(e1, e2) -> Printf.sprintf "Times(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Div(e1, e2) -> Printf.sprintf "Div(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Power(e1, e2) -> Printf.sprintf "Power(%s, %s) " (ast_to_string e1) (ast_to_string e2)

let rec expr_to_string (res:expr) = match res with
  | Literal_float n -> Printf.sprintf "%F" n
  | Literal_complex c -> Printf.sprintf "%s" (Complex.to_string c)
  | _ -> raise (ResultError "Error : Result is not final")
(* 
let solve (formula:expr) =
  eval_expr formula *)
