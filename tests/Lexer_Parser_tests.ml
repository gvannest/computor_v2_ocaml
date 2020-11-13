open OUnit2

let test_tokenize =
  "tests for tokenize()">:::
  (List.map
    (fun (arg, res) ->
      let title = Printf.sprintf "%s -> %s" arg res in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> s) res (Lexer.tokenize arg))  
    )
    ["5 + 6.3 - x * l / z ^ 0.2 ( )", "CONST_FLOAT(5.000000) PLUS CONST_FLOAT(6.300000) MINUS VAR(x) TIMES VAR(l) DIV VAR(z) POWER CONST_FLOAT(0.200000) LPAREN RPAREN EOF";
    "", "EOF";
    "3+7.2-x", "CONST_FLOAT(3.000000) PLUS CONST_FLOAT(7.200000) MINUS VAR(x) EOF";
    "-7+5", "CONST_FLOAT(-7.000000) PLUS CONST_FLOAT(5.000000) EOF";
    "3 . 5", "Error at stdin:3 - '.' token unknown.";
    "3 + 5*i", "CONST_FLOAT(3.000000) PLUS CONST_FLOAT(5.000000) TIMES COMPLEX(i) EOF";
    "-8 * i / (12+i)", "CONST_FLOAT(-8.000000) TIMES COMPLEX(i) DIV LPAREN CONST_FLOAT(12.000000) PLUS COMPLEX(i) RPAREN EOF"]
  )
  


  