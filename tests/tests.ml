

let () =
  (* OUnit2.run_test_tt_main Lexer_Parser_tests.test_tokenize *)
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
