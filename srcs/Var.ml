(* Functor which will parametrized (ie introduced varibales) in the float or complex modules *)

module type COEFFPOWER = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val is_int : t -> bool
  val compare : t -> t -> int
  val neg : t -> t
  val zero : t
  val one : t
  val to_string : t -> string
end

module type PARAMETRIZED = sig
  type t_in
  type t
  exception PowerError of string
  val create_var : t_in -> string -> t_in -> t
  val create_constant : t_in -> t
  val create_standard_var : string -> t
  val compare : t -> t -> int
  val zero : t
  val one : t
  val neg : t -> t
  val to_string : t -> string
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  (* val get_coeff : t -> t_in
  val get_power : t -> t_in *)
end

module type MAKEPARAMS = 
  functor (Input : COEFFPOWER) -> PARAMETRIZED with type t_in = Input.t

module MakeParams : MAKEPARAMS =
  functor (Input: COEFFPOWER) ->
    struct
      type t_in = Input.t
      type t = { coeff: t_in; name: string; power: t_in }

      exception PowerError of string
      let ( =. ) (x:t_in) (y:t_in) = (Input.compare x y = 0)

      let create_var (coeff:t_in) (name:string) (power:t_in) = { coeff=coeff; name=name; power=power }
      let create_standard_var (name:string) = { coeff=Input.one; name=name; power=Input.one }
      let create_constant (coeff:t_in) = { coeff=coeff; name=""; power=Input.zero }
      let zero = create_var (Input.zero) "" (Input.zero)
      let one = create_var (Input.one) "" (Input.zero)
      let neg x = create_var (Input.neg x.coeff) x.name x.power
      let is_null x = (x.coeff =. Input.zero)
      let is_one x = (compare x one = 0)
      let is_int x = ((x.name = "") && (x.power =. Input.zero) && (Input.is_int x.coeff))
      (* let get_coeff x = x.coeff
      let get_power x = x.power *)

      let is_one x = (x.coeff =. Input.one && x.power =. Input.zero)

      let compare x1 x2 = match (x1, x2) with
      | (a, b) when (a.coeff =. b.coeff)
                    && (a.power =. b.power)
                    && (a.name = b.name) -> 0
      | _ -> 1

      let to_string (x:t) = match x with
        | {coeff; name; power} when coeff =. Input.zero -> "0"
        | {coeff; name; power} when (power =. Input.zero) && (coeff =. Input.one) -> "1"
        | {coeff; name; power} when name = "" && power =. Input.zero -> Input.to_string coeff
        | {coeff; name; power} when (coeff =. Input.one) && (power =. Input.one) -> name
        | {coeff; name; power} when (power =. Input.one) -> Printf.sprintf "%s%s" (Input.to_string coeff) name
        | {coeff; name; power} when (coeff =. Input.one) -> Printf.sprintf "%s^%s" (name) (Input.to_string power)
        | {coeff; name; power} -> Printf.sprintf "%s%s^%s" (Input.to_string coeff) name (Input.to_string power)
        
      let add x1 x2 =
        if x1.name = x2.name && (x1.power =. x2.power)
        then create_var (Input.add x1.coeff x2.coeff) x1.name x1.power
        else raise (Failure "Error Params functor : add ()")

      let sub x1 x2 =
        if x1.name = x2.name && (x1.power =. x2.power)
        then  create_var (Input.sub x1.coeff x2.coeff) x1.name x1.power
        else raise (Failure "Error Params functor : sub ()")

      let mul x1 x2 = match (x1, x2) with
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when x1.coeff =. Input.zero || x2.coeff =. Input.zero -> zero
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name1 = "" -> create_var (Input.mul x1.coeff x2.coeff) x2.name x2.power
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name2 = "" -> create_var (Input.mul x1.coeff x2.coeff) x1.name x1.power
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name1 = name2 -> create_var (Input.mul x1.coeff x2.coeff) x1.name (Input.add x1.power x2.power)
        | _ -> raise (Failure "Error Params functor : mul()")

      let div x1 x2 = match (x1, x2) with
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when coeff1 =. Input.zero -> zero
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when coeff2 =. Input.zero -> raise Division_by_zero
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name2 = "" -> create_var (Input.div coeff1 coeff2) name1 pow1
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name1 = "" -> create_var (Input.div coeff1 coeff2) name2 (Input.neg pow2)
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when name1 <> name2 -> raise (Failure "Error Params functor : div ()")
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) when pow1 =. pow2 -> create_constant (Input.div coeff1 coeff2)
        | ({coeff=coeff1; name=name1; power=pow1},{coeff=coeff2; name=name2; power=pow2}) -> create_var (Input.div coeff1 coeff2) name1 (Input.sub pow1 pow2)

      let pow x exp =  
        let pair = (x, exp) in
        let rec loop_pow p = match p with
          | (v, _) when is_null v -> zero
          | (_, e) when is_null e -> one
          | (v, _) when is_one v -> one
          | (v, e) when is_one e -> v
          | (_, e) when not (is_int e) -> raise (PowerError "Failure in Var.pow : power is not an integer!")
          | (v, e) when Input.compare e.coeff Input.zero < 0 -> div one (loop_pow (v,(neg e)))
          | (v, e) -> mul (loop_pow (v, create_var (Input.sub e.coeff Input.one) "" Input.zero)) v
        in
        loop_pow pair


    end

module VariableWithComplex : (PARAMETRIZED with type t_in := Cpx.ComplexWithFloats.t) = MakeParams(Cpx.ComplexWithFloats)






