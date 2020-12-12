let var_hashtbl = Hashtbl.create 20

exception VariableNotFoundError of string

let to_string var:string =
  Solver.res_to_string (Hashtbl.find var_hashtbl var)

let get_variable (x:string) = match Hashtbl.find_opt var_hashtbl x with
  | Some v -> Solver.Literal_variable v
  | None -> Solver.Literal_variable (Var.VariableWithComplex.create_standard_var x)

let print_state () =
  print_endline "State of current variables :" ;
  if Hashtbl.length var_hashtbl = 0 then
    print_endline "No variable set."
  else
    let print_variable (x:string) (value:Var.VariableWithComplex.t) =
      Printf.printf "  %s = %s\n" x (Solver.res_to_string value)
    in
    Hashtbl.iter print_variable var_hashtbl

let set_variable (var:string) (value:Solver.expr) =
  Hashtbl.replace var_hashtbl var (Solver.eval_expr value)
