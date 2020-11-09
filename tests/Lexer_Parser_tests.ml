open OUnit2

let test_tokenize =
  "tests for tokenize()">:::
  (List.map
    (fun (arg, res) ->
      let title = Printf.sprintf "%s -> %s" arg res in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> s) res (Lexer_Parser.tokenize arg))  
    )
    ["5 + 6.3 - x * l / i ^ 0.2 ( )", "CONST_INT(5) PLUS CONST_FLOAT(6.300000) MINUS VAR(x) TIMES VAR(l) DIVIDED VAR(i) POWER CONST_FLOAT(0.200000) LEFT_PARENTHESIS RIGHT_PARENTHESIS EOF";
    "", "EOF";
    "3+7.2-x", "CONST_INT(3) PLUS CONST_FLOAT(7.200000) MINUS VAR(x) EOF";
    "-7+5", "CONST_INT(-7) PLUS CONST_INT(5) EOF";
    "3 . 5", "Error at stdin:3 - '.' token unknown.\n"]
  )
  


  