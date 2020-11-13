
type t
exception PowerError of string
val create : float -> float -> t
val to_string : t -> string
val i : t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val pow : t -> float -> t
