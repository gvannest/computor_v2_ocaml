open OUnit2

let equals_complex c1 c2 = (Cpx.ComplexWithFloats.compare c1 c1 = 0)
let to_string = Cpx.ComplexWithFloats.to_string
let create = Cpx.ComplexWithFloats.create

let complex_0 = Cpx.ComplexWithFloats.create_constant 0.
let complex_re = Cpx.ComplexWithFloats.create_constant (-3.)
let complex_im = Cpx.ComplexWithFloats.create 0. 2.
let complex_fullpositive = Cpx.ComplexWithFloats.create 2. 5.
let complex_fullnegative = Cpx.ComplexWithFloats.create -2. -5.
let complex_pos_neg = Cpx.ComplexWithFloats.create 2. -5.

let test_add =
  "Complex addition should give the good results">:::
  (List.map
    (fun (c1, c2, res) ->
      let title = Printf.sprintf "%s + %s" (to_string c1) (to_string c2) in
      title >:: (fun test_ctxt -> assert_equal ~printer:(fun s -> s) ~cmp:equals_complex (create res.fst res.snd) (Cpx.ComplexWithFloats.add c1 c2))
    )
    [
      complex_0, complexe_re, (-3., 0);
      complex_0, complexe_im, (0., 2.);
      complex_fullpositive, complex_0, (2., 5.);
      complex_re, complex_re, (-6., 0.;
      complex_re, complex_im, (-3., 2.;
      complex_im, complex_re, (-3., 2.;
      complex_im, complex_im, (0., 4.);
      complex_re, complex_pos_neg, (-1., -5.);
      complex_fullnegative, complex_re, (-5., -5.);
      complex_im, complex_fullpositive, (2., 7.);
      complex_pos_neg, complex_im, (2., -3.);
      complex_fullnegative, complex_pos_neg, (0., -10.);
    ]
  )




