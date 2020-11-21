let pv_tbl = Hashtbl.create 20

let update_power (pow:Params.FloatParam.t) (c:Complex.FloatParamComplex.t) =
  match Hashtbl.find_opt pv_tbl pow with
  | Some v -> Hashtbl.replace pow (Complex.FloatParamComplex)