(* Functor which will make complexe from module repsenting type and operations fo the coefficients *)

module type COEFFICIENTS = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val to_string : t -> string
  val neg : t -> t
end

module type COMPLEX = sig
  type t_in
  type t
  val create : t_in -> t_in -> t
  val neg : t -> t
  val zero : t
  val one : t
  val i : t
  val to_string : t -> string
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t_in -> t
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
      let zero = create Coefficient.zero Coefficient.zero
      let one = create Coefficient.one Coefficient.zero
      let i = create Coefficient.zero Coefficient.one
      let neg c = create (Coefficient.neg c.re) (Coefficient.neg c.im)
      let is_nul c = (Coefficient.is_nul c.re && Coefficient.is_nul c.im)
      let is_one c = (Coefficient.is_one c.re && Coefficient.is_one c.im)

      let to_string complex = match (complex.re, complex.im) with
        | (a,b) when Coefficient.is_nul a && Coefficient.is_nul b -> "0"
        | (a,b) when Coefficient.is_nul a -> Printf.sprintf "%si" (Coefficient.to_string b)
        | (a,b) Coefficient.is_nul b -> Printf.sprintf "%s" (Coefficient.to_string a)
        | (a,b) when Coefficient.is_neg b -> Printf.sprintf "%s - %si" (Coefficient.to_string a) (Coefficient.to_string (Coefficient.neg b))
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
      
      let pow (c:t) (exp:t_in) =
        let pair = (c, exp) in
        let rec loop_pow p = match p with
          | (_, e) when Coefficient.is_parametrized e -> raise (PowerError "Failure in Complex.pow : power is not a float!\n")
          | (v, _) when is_nul v -> zero
          | (_, e) when Coefficient.is_nul e -> one
          | (v, _) when is_one v -> one
          | (v, e) when Coefficient.is_one e -> v
          | (v, e) -> mul (loop_pow (v, Coefficient.sub e Coefficient.one) v)
          | _ -> raise (PowerError "Failure in Complex.pow : no match found!\n")
        in
        loop_pow pair

    end

module FloatParamComplex : (COEFFICIENTS with type t := Params.FloatParam.t) = MakeComplex(Params.FloatParam)



  