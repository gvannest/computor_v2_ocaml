(* Functor which will parametrized (ie introduced varibales) in the float module *)

module type COEFFPOWER = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val compare : t -> t -> int
  val rem : t -> t -> t
  val neg : t -> t
  val zero : t
  val one : t
  val to_string : t -> string
end

module type PARAMETRIZED = sig
  type t_in
  type t
  val create_var : t_in -> string -> t_in -> t
  val create_constant : t_in -> t
  val zero : t
  val one : t
  val neg : t -> t
  val is_nul : t -> bool
  val is_one : t -> bool
  val is_neg : t -> bool
  val is_parametrized : t -> bool
  val to_string : t -> string
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
end

module type MAKEPARAMS = 
  functor (Input : COEFFPOWER) -> PARAMETRIZED with type t_in = Input.t

module MakeParams : MAKEPARAMS =
  functor (Input: COEFFPOWER) ->
    struct
      type t_in = Input.t
      type t = { coeff: t_in; name: string; power: t_in }
      let ( =. ) (x:t_in) (y:t_in) = (Input.compare x y = 0)
      let create_var (coeff:t_in) (name:string) (power:t_in) = { coeff=coeff; name=name; power=power }
      let create_constant (coeff:t_in) = { coeff=coeff; name=""; power=Input.zero }
      let neg x = create_var (Input.neg x.coeff) x.name x.power
      let zero = create_var (Input.zero) "" (Input.zero)
      let one = create_var (Input.one) "" (Input.zero)
      let is_nul x = (x.coeff =. Input.zero)
      let is_neg x = (Input.compare x.coeff Input.zero < 0)

      let is_one x = match x with
        | {coeff; name; power} when power =. Input.zero && coeff =. Input.one -> true
        | _ -> false

      let is_parametrized x = match x with
        | {coeff; name; power} when Input.compare power Input.zero <> 0 -> true
        | _ -> false

      let to_string (x:t) = match x with
        | {coeff; name; power} when coeff =. Input.zero -> "0"
        | {coeff; name; power} when (power =. Input.zero) && (coeff =. Input.one) -> "1"
        | {coeff; name; power} when name = "" -> Input.to_string coeff
        | {coeff; name; power} when (coeff =. Input.one) && (power =. Input.one) -> name
        | {coeff; name; power} when (power =. Input.one) -> Printf.sprintf "%s%s" (Input.to_string coeff) name
        | {coeff; name; power} when (coeff =. Input.one) -> Printf.sprintf "%s^%s" (Input.to_string coeff) (Input.to_string power)
        | {coeff; name; power} -> Printf.sprintf "%s%s^%s" (Input.to_string coeff) name (Input.to_string power)
        
      let add x1 x2 =
        if x1.name = x2.name && (x1.power =. x2.power)
        then create_var (Input.add x1.coeff x2.coeff) x1.name x1.power
        else raise (Failure "Error Params functor : add ()")

      let sub x1 x2 =
        if x1.name = x2.name && (x1.power =. x2.power)
        then  create_var (Input.sub x1.coeff x2.coeff) x1.name x1.power
        else raise (Failure "Error Params functor : sub ()")

      let mul x1 x2 =
        if x1.name = x2.name
        then  create_var (Input.mul x1.coeff x2.coeff) x1.name (Input.add x1.power x2.power)
        else raise (Failure "Error Params functor : mul ()")
        
      let div x1 x2 =
        if Input.compare x2.coeff Input.zero <> 0 then begin
          if x1.name = x2.name
          then  create_var (Input.div x1.coeff x2.coeff) x1.name (Input.sub x1.power x2.power)
          else raise (Failure "Error Params functor : div ()")
        end
        else
          raise Division_by_zero

      let pow x1 x2 =  
        if Input.compare x2.power Input.zero = 0
        then create_var (Input.pow x1.coeff x2.coeff) x1.name (Input.mul x1.power x2.power)
        else raise (Failure "Error Params functor : pow ()")


    end

module FloatParam : (PARAMETRIZED with type t_in := float) = MakeParams(Float) 






