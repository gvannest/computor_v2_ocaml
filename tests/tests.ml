let complex_tests () =
  print_endline "Complex addition tests :" ;
  OUnit2.run_test_tt_main Complex_tests.test_add ;
  print_endline "" ;
  print_endline "Complex multipication tests :" ;
  OUnit2.run_test_tt_main Complex_tests.test_mul ;
  print_endline "" ;
  print_endline "Complex division tests :" ;
  OUnit2.run_test_tt_main Complex_tests.test_div ;
  OUnit2.run_test_tt_main Complex_tests.test_div_zero ;
  print_endline ""; 
  print_endline "Complex power tests :" ;
  OUnit2.run_test_tt_main Complex_tests.test_pow;
  print_endline ""; 
  print_endline "Complex power non valid tests :" ;
  OUnit2.run_test_tt_main Complex_tests.test_pow_non_valid ;
  print_endline ""

let variable_tests () =
  print_endline "Variables addition valid tests :" ;
  OUnit2.run_test_tt_main Variable_tests.test_add_valid ;
  print_endline "";
  print_endline "Variables addition invalid tests :" ;
  OUnit2.run_test_tt_main Variable_tests.test_add_invalid ;
  print_endline "";
  print_endline "Variables multiplication valid tests :" ;
  OUnit2.run_test_tt_main Variable_tests.test_mul_valid ;
  print_endline "";
  print_endline "Variables multiplication invalid tests :" ;
  OUnit2.run_test_tt_main Variable_tests.test_mul_invalid ;
  print_endline "";
  print_endline "Variables division valid tests :" ;
  OUnit2.run_test_tt_main Variable_tests.test_div_valid ;
  print_endline "";
  print_endline "Variables division by 0 raises error :" ;
  OUnit2.run_test_tt_main Variable_tests.test_div_invalid_0 ;
  print_endline "";
  print_endline "Variable divisions with different variable names are invalid :" ;
  OUnit2.run_test_tt_main Variable_tests.test_div_invalid ;
  print_endline "";
  print_endline "Variable power operations valid :" ;
  OUnit2.run_test_tt_main Variable_tests.test_pow_valid ;
  print_endline "";
  print_endline "Variable power operations invalid :" ;
  OUnit2.run_test_tt_main Variable_tests.test_pow_invalid ;
  print_endline ""


let () =
  (* OUnit2.run_test_tt_main Lexer_Parser_tests.test_tokenize *)
  (* complex_tests (); *)
  variable_tests ()
  
