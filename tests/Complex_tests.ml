open OUnit2

let equals_complex c1 c2 = (Cpx.ComplexWithFloats.compare c1 c2 = 0)
let to_string = Cpx.ComplexWithFloats.to_string
let create = Cpx.ComplexWithFloats.create

let complex_0 = Cpx.ComplexWithFloats.create_constant 0.
let complex_1 = Cpx.ComplexWithFloats.create_constant 1.
let complex_re = Cpx.ComplexWithFloats.create_constant (-3.)
let complex_re_positive = Cpx.ComplexWithFloats.create_constant (3.)
let complex_re_float = Cpx.ComplexWithFloats.create_constant (3.2)
let complex_im = create 0. 2.
let complex_fullpositive = create 2. 5.
let complex_fullnegative = create (Float.neg 2.) (Float.neg 5.)
let complex_pos_neg = create 2. (Float.neg 4.)

let test_add =
  "Complex addition should give the good result">:::
  (List.map
    (fun (c1, c2, res) ->
      let title = Printf.sprintf "(%s) + (%s)" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_complex (create (fst res) (snd res)) (Cpx.ComplexWithFloats.add c1 c2))
    )
    [complex_0, complex_re, (Float.neg 3., 0.);
      complex_0, complex_im, (0., 2.);
      complex_fullpositive, complex_0, (2., 5.);
      complex_re, complex_re, (Float.neg 6., 0.);
      complex_re, complex_im, (Float.neg 3., 2.);
      complex_im, complex_re, (Float.neg 3., 2.);
      complex_im, complex_im, (0., 4.);
      complex_re, complex_pos_neg, (Float.neg 1., Float.neg 4.);
      complex_fullnegative, complex_re, (Float.neg 5., Float.neg 5.);
      complex_im, complex_fullpositive, (2., 7.);
      complex_pos_neg, complex_im, (2., Float.neg 2.);
      complex_fullnegative, complex_pos_neg, (0., Float.neg 9.)]
  )

let test_mul =
  "Complex multiplication should give the good result">:::
  (List.map
    (fun (c1, c2, res) ->
      let title = Printf.sprintf "(%s) x (%s)" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_complex (create (fst res) (snd res)) (Cpx.ComplexWithFloats.mul c1 c2))
    )
    [complex_0, complex_re, (0., 0.);
      complex_0, complex_im, (0., 0.);
      complex_fullpositive, complex_0, (0., 0.);
      complex_re, complex_re, (9., 0.);
      complex_re, complex_im, (0., Float.neg 6.);
      complex_im, complex_re, (0., Float.neg 6.);
      complex_im, complex_im, (Float.neg 4., 0.);
      complex_re, complex_pos_neg, (Float.neg 6., 12.);
      complex_fullnegative, complex_re, (6., 15.);
      complex_im, complex_fullpositive, (Float.neg 10., 4.);
      complex_pos_neg, complex_im, (8., 4.);
      complex_fullnegative, complex_pos_neg, (Float.neg 24., Float.neg 2.)]
  )

let test_div =
  "Complex division should give the good result">:::
  (List.map
    (fun (c1, c2, res) ->
      let title = Printf.sprintf "  (%s) / (%s)" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_complex (create (fst res) (snd res)) (Cpx.ComplexWithFloats.div c1 c2))
    )
    [complex_0, complex_re, (0., 0.);
      complex_0, complex_im, (0., 0.);
      complex_re, complex_re, (1., 0.);
      complex_re, complex_im, (0., 3. /. 2.);
      complex_im, complex_re, (0., 2. /. (Float.neg 3.));
      complex_im, complex_im, (1., 0.);
      complex_re, complex_pos_neg, ((Float.neg 3.) /. 10., (Float.neg 3.) /. 5.);
      complex_fullnegative, complex_re, (2. /. 3., 5. /. 3.);
      complex_im, complex_fullpositive, (10. /. 29., 4. /. 29.);
      complex_pos_neg, complex_im, (Float.neg 2., Float.neg 1.);
      complex_fullnegative, complex_pos_neg, (4. /. 5., (Float.neg 9.) /. 10.)]
  )

let test_div_zero =
  "Complex division by zero should not be permitted">::
      (fun test_ctxt -> assert_raises (Division_by_zero) (fun () -> Cpx.ComplexWithFloats.div complex_fullnegative complex_0))

let test_pow =
  "Power operations on complex should give the good result">:::
  (List.map
    (fun (c1, c2, res) ->
      let title = Printf.sprintf "  (%s) ^ (%s)" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> to_string s) ~cmp:equals_complex (create (fst res) (snd res)) (Cpx.ComplexWithFloats.pow c1 c2))
    )
    [complex_0, complex_re, (0., 0.);
    complex_re, complex_re_positive, (Float.neg 27., 0.);
    complex_re, complex_re, (Float.neg (1. /. 27.), 0.);
    complex_im, complex_re_positive, (0., Float.neg 8.);
    complex_im, complex_re, (0., 1. /. 8.);
    complex_pos_neg, complex_0, (1., 0.);
    complex_pos_neg, complex_1, (2., Float.neg 4.);
    complex_pos_neg, complex_re_positive, ((Float.neg 88.), 16.);
    complex_pos_neg, complex_re, ((Float.neg 11.) /. 1000., (Float.neg 1.) /. 500.);
    complex_fullnegative, complex_re_positive, (142., 65.);
    complex_fullnegative, complex_re, (142. /. 24389., Float.neg(65. /. 24389.));
    ]
  )

let test_pow_non_valid =
  "Power operations on complex where power is of another type than the type of coefficients should not be permitted">:::
  (List.map
    (fun (c1, c2) ->
      let title = Printf.sprintf "  (%s) ^ (%s)" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_raises (Cpx.ComplexWithFloats.PowerError "Failure in Complex.pow : power is not an integer!") (fun () -> Cpx.ComplexWithFloats.pow c1 c2))
    )
    [complex_0, complex_im;
    complex_re, complex_im;
    complex_pos_neg, complex_fullpositive;
    complex_im, complex_fullpositive;
    complex_pos_neg, complex_re_float;
    ]
  )