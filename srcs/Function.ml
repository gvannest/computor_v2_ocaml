
(* fun_hashtbl stores functions under the form :
  key -> string => name of function
  value -> (string * Solver.Expr) => (variable name, Expression)
*)

let fun_hashtbl = Hashtbl.create 20

exception FunctionNotFoundError of string

let to_string func:string =
  let (var, e) = Hashtbl.find fun_hashtbl func in
  Printf.sprintf "(%s, %s)" var (Solver.res_to_string e)

(*let get_function (x:string) = match Hashtbl.find_opt var_hashtbl x with
  | Some v -> v
  | None -> raise (VariableNotFoundError (Printf.sprintf "Error : variable '%s' not found" x))
*)

let print_state () =
  print_endline "State of defined functions :" ;
  if Hashtbl.length fun_hashtbl = 0 then
    print_endline "No function defined."
  else
    let print_function (func:string) ((x:string), (value:Complex.FloatParamComplex.t)) =
      Printf.printf "  %s = (%s, %s)\n" func x (Solver.res_to_string value)
    in
    Hashtbl.iter print_function fun_hashtbl

let set_function (name:string) (var:string) (value:Solver.expr) =
  Hashtbl.replace fun_hashtbl name (var, (Solver.eval_expr value))
