open OUnit2

let equals_variables x1 x2 = (Var.VariableWithComplex.compare x1 x2 = 0)
let to_string = Var.VariableWithComplex.to_string
let create = Var.VariableWithComplex.create_var
let create_standard_var = Var.VariableWithComplex.create_standard_var
let create_constant = Var.VariableWithComplex.create_constant
let create_cpx = Cpx.ComplexWithFloats.create

let complex_0 = Cpx.ComplexWithFloats.create_constant 0.
let complex_1 = Cpx.ComplexWithFloats.create_constant 1.
let complex_2 = Cpx.ComplexWithFloats.create_constant 2.
let complex_re = Cpx.ComplexWithFloats.create_constant (-3.)
let complex_re_positive = Cpx.ComplexWithFloats.create_constant (3.)
let complex_re_float = Cpx.ComplexWithFloats.create_constant (3.2)
let complex_im = Cpx.ComplexWithFloats.create 0. 2.
let complex_fullpositive = Cpx.ComplexWithFloats.create 2. 5.
let complex_fullnegative = Cpx.ComplexWithFloats.create (Float.neg 2.) (Float.neg 5.)
let complex_pos_neg = Cpx.ComplexWithFloats.create 2. (Float.neg 4.)

let constant_one = create_constant complex_1
let constant_neg = create_constant complex_re
let constant_pos = create_constant complex_re_positive
let constant_float = create_constant complex_re_float
let constant_cpx = create_constant complex_fullpositive
let standard_var = create_standard_var "x"
let zero = Var.VariableWithComplex.zero
let cpx_posneg_x_0 = create complex_pos_neg "x" complex_0
let cpx_posneg_x_1 = create complex_pos_neg "x" complex_1
let cpx_posneg_x_neg = create complex_pos_neg "x" complex_re
let cpx_posneg_x_float = create complex_pos_neg "x" complex_re_float
let cpx_fullpos_x_1 = create complex_fullpositive "x" complex_1
let cpx_fullpos_x_float = create complex_fullpositive "x" complex_re_float
let cpx_fullneg_x_2 = create complex_fullnegative "x" complex_2
let cpx_posneg_x_2 = create complex_pos_neg "x" complex_2
let reneg_x_1 = create complex_re "x" complex_1
let refloat_x_1 = create complex_re_float "x" complex_1
let refloat_x_2 = create complex_re_float "x" complex_2
let cpx_fullpos_y_1 = create complex_fullpositive "y" complex_1
let cpx_x_cpx = create complex_fullpositive "x" complex_pos_neg
let cpx_x_neg = create complex_fullpositive "x" complex_re
let cpx_x_float = create complex_fullpositive "x" complex_re_float
let cpx_x_pos = create complex_fullpositive "x" complex_re_positive
let zero_x_pos = create complex_0 "x" complex_re_positive
let one_x_pos = create complex_1 "x" complex_re_positive


let test_add_valid =
  "Variable addition when allowed should give the good result">:::
  (List.map
    (fun (v1, v2, (coeff, var, power)) ->
      let title = Printf.sprintf "  (%s) + (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_variables (create coeff var power) (Var.VariableWithComplex.add v1 v2))
    )
    [
      constant_float, constant_pos, (create_cpx 6.2 0., "", complex_0);
      zero, constant_pos, (create_cpx 3. 0., "", complex_0);
      (* zero, standard_var, (create_cpx 1. 0., "x", create_cpx 1. 0.); *)
      standard_var, standard_var, (create_cpx 2. 0., "x", create_cpx 1. 0.);
      cpx_posneg_x_1, standard_var, (create_cpx 3. (Float.neg 4.), "x", create_cpx 1. 0.);
      cpx_posneg_x_2, cpx_fullneg_x_2, (create_cpx 0. (Float.neg 9.), "x", create_cpx 2. 0.);
      cpx_posneg_x_neg, cpx_posneg_x_neg, (create_cpx 4. (Float.neg 8.), "x", complex_re);
      cpx_posneg_x_neg, cpx_posneg_x_neg, (create_cpx 4. (Float.neg 8.), "x", complex_re);
      cpx_posneg_x_float, cpx_fullpos_x_float, (create_cpx 4. 1., "x", complex_re_float);
    ]
  )

let test_add_invalid =
  "variable addition should stop when variables cannot be added together">:::
  (List.map
    (fun (v1, v2) ->
      let title = Printf.sprintf "  (%s) + (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_raises (Failure "Error Params functor : add ()") (fun () -> Var.VariableWithComplex.add v1 v2))
    )
    [
      zero, standard_var;
      cpx_posneg_x_1, cpx_fullneg_x_2;
      cpx_posneg_x_1, cpx_fullpos_y_1;
    ]
  )

  
let test_mul_valid =
  "Variable addition when allowed should give the good result">:::
  (List.map
    (fun (v1, v2, (coeff, var, power)) ->
      let title = Printf.sprintf "  (%s) x (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_variables (create coeff var power) (Var.VariableWithComplex.mul v1 v2))
    )
    [
      zero, standard_var, (create_cpx 0. 0., "", complex_0);
      constant_pos, standard_var, (create_cpx 3. 0., "x", complex_1);
      cpx_fullpos_x_1, constant_pos, (create_cpx 6. 15., "x", complex_1);
      standard_var, standard_var, (create_cpx 1. 0., "x", complex_2);
      cpx_fullpos_x_1, cpx_posneg_x_2, (create_cpx 24. 2., "x", create_cpx 3. 0.);
      cpx_fullneg_x_2, cpx_posneg_x_neg, (create_cpx (Float.neg 24.) (Float.neg 2.), "x", create_cpx (Float.neg 1.) 0.);
    ]
  )
  
let test_mul_invalid =
  "variable multiplication should stop when variables cannot be multiplied together">:::
  (List.map
    (fun (v1, v2) ->
      let title = Printf.sprintf "  (%s) x (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_raises (Failure "Error Params functor : mul()") (fun () -> Var.VariableWithComplex.mul v1 v2))
    )
    [
      cpx_posneg_x_1, cpx_fullpos_y_1;
      cpx_fullpos_y_1, cpx_fullneg_x_2;
    ]
  )

let test_div_valid =
  "Variable division when allowed should give the good result">:::
  (List.map
    (fun (v1, v2, (coeff, var, power)) ->
      let title = Printf.sprintf "  (%s) / (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_variables (create coeff var power) (Var.VariableWithComplex.div v1 v2))
    )
    [
      zero, cpx_fullpos_x_1, (create_cpx 0. 0., "", complex_0);
      constant_one, cpx_posneg_x_2, (create_cpx (1. /. 10.) (1. /. 5.), "x", create_cpx (Float.neg 2.) 0.);
      cpx_posneg_x_2, constant_one, (complex_pos_neg, "x", complex_2);
      cpx_fullpos_x_1, cpx_fullpos_x_1, (complex_1, "", complex_0);
      cpx_posneg_x_2, cpx_fullpos_x_1, (create_cpx (Float.neg (16. /. 29.)) (Float.neg (18. /. 29.)), "x", complex_1);
      cpx_posneg_x_2, cpx_posneg_x_neg, (complex_1, "x", create_cpx 5. 0.);
    ]
  )
  
let test_div_invalid_0 =
  "variable division should raise error if denomiator is null">::
    (fun test_ctxt -> assert_raises (Division_by_zero) (fun () -> Var.VariableWithComplex.div cpx_posneg_x_2 zero))

let test_div_invalid =
  "variable division should stop when variables cannot be divided together">:::
  (List.map
    (fun (v1, v2) ->
      let title = Printf.sprintf "  (%s) / (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_raises (Failure "Error Params functor : div ()") (fun () -> Var.VariableWithComplex.div v1 v2))
    )
    [
      cpx_posneg_x_1, cpx_fullpos_y_1;
      cpx_fullpos_y_1, cpx_fullneg_x_2;
    ]
  )

let test_pow_valid =
  "Variable power operations, when allowed, should give the good result">:::
  (List.map
    (fun (v1, v2, (coeff, var, power)) ->
      let title = Printf.sprintf "  (%s) ^ (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_variables (create coeff var power) (Var.VariableWithComplex.pow v1 v2))
    )
    [
      cpx_fullpos_x_1, zero, (complex_1, "", complex_0);
      cpx_fullpos_x_1, constant_one, (complex_fullpositive, "x", complex_1);
      cpx_fullpos_x_1, constant_pos, (create_cpx (Float.neg 142.) (Float.neg 65.), "x", create_cpx 3. 0.);
      cpx_fullpos_x_1, constant_neg, (create_cpx (Float.neg (142. /. 24389.)) (65. /. 24389.), "x", create_cpx (Float.neg 3.) 0.);
      zero, constant_neg, (complex_0, "", complex_0);
      constant_one, constant_neg, (complex_1, "", complex_0);
      constant_cpx, constant_pos, (create_cpx (Float.neg 142.) (Float.neg 65.), "", complex_0);
    ]
  )

let test_pow_invalid =
  "variable power operations should not be permitted of power is not an integer">:::
  (List.map
    (fun (v1, v2) ->
      let title = Printf.sprintf "  (%s) ^ (%s)" (to_string v1) (to_string v2) in
      title >:: (fun test_ctxt -> assert_raises (Var.VariableWithComplex.PowerError "Failure in Var.pow : power is not an integer!") (fun () -> Var.VariableWithComplex.pow v1 v2))
    )
    [
      cpx_posneg_x_1, constant_float;
      cpx_fullpos_y_1, cpx_posneg_x_1;
      cpx_posneg_x_1, constant_cpx;
      constant_cpx, constant_float;
      constant_cpx, cpx_posneg_x_1;
      constant_cpx, constant_cpx;
      constant_pos, constant_cpx;
      constant_pos, constant_float;
    ]
  )