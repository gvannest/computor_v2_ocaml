
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
        Printf.ksprintf (fun msg -> raise (TokenError ("Error at "^(position lexbuf)^" - "^msg))) fmt
    (* Note : fmt is the format string which will be passed as argument to the callback (ie 1st arg of ksprintf *)
    
}

(*-------- definitions ---------*)
let ws = [' ' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'h' 'j'-'z' 'A'-'H' 'J'-'Z']
let complex = ['i' 'I']
let give_value = '=' ws* '?'
let int_number = digit digit*
let float_number = int_number+ '.' digit*


(*-------- rules ---------*)
rule read = parse
    | ws+                   { read lexbuf }
    | int_number            { CONST_FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
    | float_number          { CONST_FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
    | alpha+                { VAR(Lexing.lexeme lexbuf) }
    | complex               { COMPLEX(Cpx.ComplexWithFloats.i) }
    | give_value            { GIVE_VALUE }
    | '='                   { EQUALS }
    | '+'                   { PLUS }
    | '?'                   { QUESTION_MARK }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '^'                   { POWER }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | eof                   { EOF }
    | _                     { token_error lexbuf "'%s' token unknown." (Lexing.lexeme lexbuf) }  


(*-------- trailer for testing purpose ---------*)
{
    let to_string = function
        | VAR(str)          -> Printf.sprintf "VAR(%s)" str
        | CONST_FLOAT(d)    -> Printf.sprintf "CONST_FLOAT(%f)" d
        | COMPLEX(i)        -> Printf.sprintf "COMPLEX(i)"
        | EQUALS            -> Printf.sprintf "EQUALS" 
        | PLUS              -> Printf.sprintf "PLUS"
        | GIVE_VALUE        -> Printf.sprintf "GIVE_VALUE"
        | MINUS             -> Printf.sprintf "MINUS"
        | TIMES             -> Printf.sprintf "TIMES"
        | QUESTION_MARK     -> Printf.sprintf "QUESTION_MARK"
        | DIV               -> Printf.sprintf "DIV"
        | POWER             -> Printf.sprintf "POWER"
        | LPAREN            -> Printf.sprintf "LPAREN"
        | RPAREN            -> Printf.sprintf "RPAREN"
        | EOF               -> Printf.sprintf "EOF"

    let tokenize (input:string) =
        let lexbuf = set_filename "stdin" (Lexing.from_string input)
        in
        try
            let rec loop acc = function
                | EOF   -> (to_string EOF) :: acc |> List.rev
                | x     -> loop ((to_string x) :: acc) (read lexbuf)
            in
            loop [] (read lexbuf)
            |> String.concat " "
        with
            | TokenError msg -> msg
}