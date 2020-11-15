 
exception ResultError of string

(* type op =
  | Plus of expr * expr *)
  

type expr =
  (* | Literal_float of float *)
  | Literal of Complex.t
  (* | Var of Variable.t *)
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
  | Literal of Complex.t
  | Plus(e1, e2) -> Complex.add (eval_expr e1) (eval_expr e2
  | Minus(e1, e2) -> Complex.sub (eval_expr e1) (eval_expr e2)
  | Times(e1, e2) -> Complex.mul (eval_expr e1) (eval_expr e2)
  | Div(e1, e2) -> Complex.div (eval_expr e1) (eval_expr e2)
  | Power(e1, e2) -> Complex.pow (eval_expr e1) (eval_expr e2)

let rec ast_to_string (e:expr) = match e with
  (* | Literal_float n -> Printf.sprintf "%F" n
  | Literal_complex c -> Printf.sprintf "(%s)" (Complex.to_string c) *)
  (* | Var x -> Printf.sprintf "%s" x *)
  | Plus(e1, e2) -> Printf.sprintf "Plus(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Minus(e1, e2) -> Printf.sprintf "Minus(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Times(e1, e2) -> Printf.sprintf "Times(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Div(e1, e2) -> Printf.sprintf "Div(%s, %s) " (ast_to_string e1) (ast_to_string e2)
  | Power(e1, e2) -> Printf.sprintf "Power(%s, %s) " (ast_to_string e1) (ast_to_string e2)

let rec expr_to_string (res:expr) = match res with
  (* | Literal_float n -> Printf.sprintf "%F" n *)
  | Literal c -> Printf.sprintf "%s" (Complex.to_string c)
  | _ -> raise (ResultError "Error : Result is not final")
(* 
let solve (formula:expr) =
  eval_expr formula *)
