
(* let to_string = function
  | Lexer.CONST_INT(d)      -> Printf.sprintf "CONST_INT(%d)" d
  | Lexer.CONST_FLOAT(f)    -> Printf.sprintf "CONST_FLOAT(%f)" f
  | Lexer.VAR(x)            -> Printf.sprintf "VAR(%s)" x
  | Lexer.PLUS              -> Printf.sprintf "PLUS"
  | Lexer.MINUS             -> Printf.sprintf "MINUS"
  | Lexer.TIMES             -> Printf.sprintf "TIMES"
  | Lexer.DIVIDED           -> Printf.sprintf "DIVIDED"
  | Lexer.POWER             -> Printf.sprintf "POWER"
  | Lexer.LEFT_PARENTHESIS  -> Printf.sprintf "LEFT_PARENTHESIS"
  | Lexer.RIGHT_PARENTHESIS -> Printf.sprintf "RIGHT_PARENTHESIS"
  | Lexer.EOF               -> Printf.sprintf "EOF"

let tokenize (str:string) =
  try
    let token_list = Lexer.tokenize(str)
    in
    List.map to_string token_list |> String.concat " " (* |> print_endline *)
  with
    | Lexer.TokenError str -> Printf.sprintf "Error at %s\n" str *)

open Lexer
open Lexing


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let extract_formula (formula:Solver.expr option) = match formula with
  | None -> Printf.sprintf "No equation provided."
  | Some f -> Printf.sprintf "%f" (Solver.eval_expr f)

let process_line (input:string) =
  let lexbuf = Lexer.set_filename "stdin" (Lexing.from_string input)
  in
  try 
    let formula = Parser.parse_formula Lexer.read lexbuf
    in print_endline (Solver.expr_to_string formula) ; Solver.eval_expr formula
  with
    | TokenError msg -> Printf.fprintf stderr "%s\n" msg ; exit(-1)
    | Parser.Error -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf ; exit (-1)
