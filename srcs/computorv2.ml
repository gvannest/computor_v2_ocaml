

let process_line (input:string) =
    let lexbuf = Lexer.set_filename "stdin" (Lexing.from_string input) in
    try
        let (parsing_output:External_types.parser_t) = Parser.parse_input Lexer.read lexbuf in match parsing_output with
            | Expr e -> print_endline (Solver.ast_to_string e) ;
                Printf.printf "%s\n" (Solver.res_to_string (Solver.eval_expr e))
            | Equation (var, e) ->
                Variable.set_variable var e ;
                Printf.printf "%s\n" (Variable.to_string var)
            | Function (func, var, e) -> Function.set_function func var e
    with
    | Lexer.TokenError msg -> Printf.printf "Error : %s\n" msg
    | Parser.Error -> Printf.printf "Error : %s -> syntax error in parsing\n" (Lexer.position lexbuf)
    | Cpx.ComplexWithFloats.PowerError msg -> Printf.printf "Error : %s\n" msg
    | Variable.VariableNotFoundError msg -> Printf.printf "Error : %s\n" msg
   
let main () =
    while true do
        print_string "> ";
        let str = read_line() in
        if str = "q" then exit 0;
        if String.length str <> 0 then
           process_line str ;
        Variable.print_state () ;
        Function.print_state ()
    done

let () = main ()