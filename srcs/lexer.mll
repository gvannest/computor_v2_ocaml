
(*-------- header ---------*)
{
    open Parser

    let position lexbuf =
        let p = lexbuf.Lexing.lex_curr_p in
            Printf.sprintf "%s:%d" p.Lexing.pos_fname p.Lexing.pos_cnum

    let set_filename (fname:string) (lexbuf:Lexing.lexbuf)  =
        (lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname } ; lexbuf )


    exception TokenError of string
    let token_error lexbuf fmt = 
        Printf.ksprintf (fun msg -> raise (TokenError ((position lexbuf)^" - "^msg))) fmt
    (* Note : fmt is the format string which will be passed as argument to the callback (ie 1st arg of ksprintf *)
    
}

(*-------- definitions ---------*)
let ws = [' ' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int_number = '-'? digit digit*
let float_number = int_number+ '.' digit*


(*-------- rules ---------*)
rule read = parse
    | ws+                   { read lexbuf }
    | int_number            { CONST_INT(int_of_string(Lexing.lexeme lexbuf)) }
    | float_number          { CONST_FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
    | alpha+                { VAR(Lexing.lexeme lexbuf) }
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '^'                   { POWER }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | eof                   { EOF }
    | _                     { token_error lexbuf "'%s' token unknown." (Lexing.lexeme lexbuf) }  


(*-------- trailer ---------*)
{

    (* let tokenize (input:string) =
        let lexbuf = set_filename "stdin" (Lexing.from_string input)
        in
        let rec loop acc = function
            | EOF   -> EOF :: acc |> List.rev
            | x     -> loop (x :: acc) (read lexbuf)
        in
        loop [] (read lexbuf) *)
        (* |> String.concat " " *)
        (* |> print_endline *)
}