(* 
Note : here it is a module and not a functor, because it only applies to Float
as the complex coefficients are floats. Hence the functor would only be applied to
float in any case... so a functor is useless.
A fucotr would be useful if we were to apply it to several modules (Matrices, functions...)
*)


type t = { re: float; im: float }

exception PowerError of string

let create a b = { re=a; im=b }
let zero = create 0.0 0.0
let one = create 1.0 0.0
let i = create 0.0 1.0
(* let i_power_2 = create (-1.0) 0.0 *)

let to_string complex = match (complex.re, complex.im) with
  | (a,b) when a = 0. && b = 0. -> "0"
  | (a,b) when a = 0. -> Printf.sprintf "%Fi" b
  | (a,b) when b = 0. -> Printf.sprintf "%F" a
  | (a,b) when b < 0. -> Printf.sprintf "%F - %Fi" a (Float.abs b)
  | _ -> Printf.sprintf "%F + %Fi" complex.re complex.im

let equals c1 c2 = (c1.re = c2.re && c1.im = c2.im)
let get_conjugate complex = create complex.re (Float.neg complex.im)

let add c1 c2 = create (Float.add c1.re c2.re) (Float.add c1.im c2.im)

let sub c1 c2 = create (Float.sub c1.re c2.re) (Float.sub c1.im c2.im)

let mul c1 c2 =
  let m1 = create (Float.mul c1.re c2.re) 0.0 in
  let m2 = create 0.0 (Float.mul c1.re c2.im) in
  let m3 = create 0.0 (Float.mul c2.re c1.im) in
  let m4 = create (Float.neg (Float.mul c1.im c2.im)) 0.0 in
  List.fold_left add zero [m1; m2; m3; m4]

let rec div c1 c2 =
  if c2.im = 0.0
  then
    let c2_conj = get_conjugate c2 in
    let numerator = mul c1 c2_conj in
    let denominator = mul c2 c2_conj in
    div numerator denominator
  else
    create (Float.div c1.re c2.re) (Float.div c1.im c2.re)

let pow (c:t) (exp:float) =
  let pair = (c, exp) in
  let rec loop_pow p = match p with
    | (v, _) when equals v zero -> zero
    | (_, e) when e = 0.0 -> one
    | (v, _) when equals v one -> one
    | (v, e) when e = 1.0 -> v
    | (v, e) when Float.rem e 2.0 = 0.0 -> mul (loop_pow (v, e /. 2.0)) (loop_pow (v, e /. 2.0))
    | (v, e) when Float.rem e 2.0 = 1.0 -> mul (loop_pow (v, e -. 1.0)) v
    | _ -> raise (PowerError "Failure in Complex.pow : no match found!\n")
  in
  loop_pow pair
  