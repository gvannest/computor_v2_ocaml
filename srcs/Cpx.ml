(* Functor which will make complexe from module repsenting type and operations fo the coefficients *)

module type COEFFICIENTS = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val neg : t -> t
  val zero : t
  val one : t
end

module type COMPLEX = sig
  type t_in
  type t
  exception PowerError of string
  val create : t_in -> t_in -> t
  val create_constant : t_in -> t
  val compare : t -> t -> int
  val neg : t -> t
  val zero : t
  val one : t
  val i : t
  val to_string : t -> string
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
end

module type MAKECOMPLEX = 
  functor (Coefficient : COEFFICIENTS) -> COMPLEX with type t_in = Coefficient.t

module MakeComplex : MAKECOMPLEX =
  functor (Coefficient: COEFFICIENTS) ->
    struct
      type t_in = Coefficient.t
      type t = { re: t_in; im: t_in }

      exception PowerError of string
      
      let create (a:t_in) (b:t_in) = { re=a; im=b }
      let create_constant (a:t_in) = { re=a; im=Coefficient.zero }
      let zero = create Coefficient.zero Coefficient.zero
      let one = create Coefficient.one Coefficient.zero
      let i = create Coefficient.zero Coefficient.one
      let neg c = create (Coefficient.neg c.re) (Coefficient.neg c.im)
      let is_coeff_null coeff = (Coefficient.compare coeff Coefficient.zero = 0) 
      let is_coeff_neg coeff = (Coefficient.compare coeff Coefficient.zero < 0) 
      let is_complex c = not (is_coeff_null c.im)
      let is_null c = (is_coeff_null c.re && is_coeff_null c.im)
      let is_one c = (Coefficient.compare c.re Coefficient.one = 0) && (is_coeff_null c.im)

      let compare c1 c2 = match (c1, c2) with
        | (a, b) when (Coefficient.compare a.re b.re = 0) && (Coefficient.compare a.im b.im = 0) -> 0
        | _ -> 1

      (* let is_real c = not (is_coeff_null c.re) && (is _coeff_nul c.im) 
      let is_im c = (is_coeff_null c.re) && (is_complex c) *)

      let to_string complex = match (complex.re, complex.im) with
        | (a,b) when is_coeff_null a && is_coeff_null b -> "0"
        | (a,b) when is_coeff_null a -> Printf.sprintf "%si" (Coefficient.to_string b)
        | (a,b) when is_coeff_null b -> Printf.sprintf "%s" (Coefficient.to_string a)
        | (a,b) when is_coeff_neg b -> Printf.sprintf "%s - %si" (Coefficient.to_string a) (Coefficient.to_string (Coefficient.neg b))
        | _ -> Printf.sprintf "%s + %si" (Coefficient.to_string complex.re) (Coefficient.to_string complex.im)
      
      let get_conjugate complex = create complex.re (Coefficient.neg complex.im)
      
      let add c1 c2 = create (Coefficient.add c1.re c2.re) (Coefficient.add c1.im c2.im)
      
      let sub c1 c2 = create (Coefficient.sub c1.re c2.re) (Coefficient.sub c1.im c2.im)
      
      let mul c1 c2 =
        let m1 = create (Coefficient.mul c1.re c2.re) Coefficient.zero in
        let m2 = create Coefficient.zero (Coefficient.mul c1.re c2.im) in
        let m3 = create Coefficient.zero (Coefficient.mul c2.re c1.im) in
        let m4 = create (Coefficient.neg (Coefficient.mul c1.im c2.im)) Coefficient.zero in
        List.fold_left add zero [m1; m2; m3; m4]
      
      let rec div c1 c2 =
        if c2.im <> Coefficient.zero
        then
          let c2_conj = get_conjugate c2 in
          let numerator = mul c1 c2_conj in
          let denominator = mul c2 c2_conj in
          div numerator denominator
        else
          create (Coefficient.div c1.re c2.re) (Coefficient.div c1.im c2.re)
      
      let pow (c:t) (exp:t) =
        let pair = (c, exp) in
        let rec loop_pow p = match p with
          | (_, e) when is_complex e -> raise (PowerError "Failure in Complex.pow : power is not a float!\n")
          | (v, _) when is_null v -> zero
          | (_, e) when is_null e -> one
          | (v, _) when is_one v -> one
          | (v, e) when is_one e -> v
          | (v, e) -> mul (loop_pow (v, create (Coefficient.sub e.re Coefficient.one) Coefficient.zero)) v
        in
        loop_pow pair

    end
  
module ComplexWithFloats : (COMPLEX with type t_in := float) = MakeComplex(Float)
  