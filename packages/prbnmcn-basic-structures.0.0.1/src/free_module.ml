module Make
    (Basis : Basic_intf.Std)
    (R : Basic_intf.Ring_std)
    (M : Map.S with type key = Basis.t) :
  Basic_intf.Free_module_std
    with module R = R
     and module Basis = Basis
     and type basis = Basis.t
     and type t = R.t M.t = struct
  module R = R
  module Basis = Basis

  type basis = Basis.t

  type t = R.t M.t

  let zero = M.empty

  let binop op (vec1 : t) (vec2 : t) : t =
    M.union
      (fun _elt i1 i2 ->
        let res = op i1 i2 in
        if R.compare res R.zero = 0 then None else Some res)
      vec1
      vec2
    [@@inline]

  let add vec1 vec2 = binop R.add vec1 vec2

  let smul coeff vec =
    if R.compare coeff R.zero = 0 then zero
    else M.map (fun x -> R.mul coeff x) vec

  let neg vec = M.map R.neg vec

  let fold = M.fold

  let bind x f =
    fold (fun basis coeff acc -> add acc (smul coeff (f basis))) x zero

  let eval vec i = try M.find i vec with Not_found -> R.zero

  let of_list : (basis * R.t) list -> t =
   fun l ->
    let exception Not_all_distinct in
    try
      List.fold_left
        (fun vec (i, elt) ->
          if M.mem i vec then raise Not_all_distinct
          else if R.equal elt R.zero then vec
          else add vec (M.singleton i elt))
        zero
        l
    with Not_all_distinct -> invalid_arg "of_list"

  let delta x = of_list [(x, R.one)]

  let compare = M.compare R.compare

  let equal x y = compare x y = 0

  let hash (x : t) =
    M.fold (fun basis v hash -> Hashtbl.hash (basis, R.hash v, hash)) x 0

  let pp : Format.formatter -> t -> unit =
   fun fmtr vec ->
    if M.is_empty vec then Format.fprintf fmtr "∅"
    else
      let bindings = M.bindings vec in
      Format.pp_print_list
        ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";@,")
        (fun fmtr (k, v) -> Format.fprintf fmtr "%a ↦ %a" Basis.pp k R.pp v)
        fmtr
        bindings
end

module type Free_module_with_map = sig
  type r

  module Map : Map.S

  include
    Basic_intf.Free_module_std
      with type R.t = r
       and type Basis.t = Map.key
       and type t = r Map.t
end

module Make_with_map (X : Basic_intf.Std) (R : Basic_intf.Ring_std) :
  Free_module_with_map with type r = R.t and type Map.key = X.t = struct
  type r = R.t

  module Map = Map.Make (X)
  include Make (X) (R) (Map)
end

module Rational_valued = struct
  module Int = Make (Std.Int) (Std.Q) (Int_map)
  module String = Make (Std.String) (Std.Q) (String_map)
  module Float = Make (Std.Float) (Std.Q) (Float_map)
  module Bool = Make (Std.Bool) (Std.Q) (Bool_map)
  module Make_with_map (X : Basic_intf.Std) = Make_with_map (X) (Std.Q)
end

module Float_valued = struct
  module Int = Make (Std.Int) (Std.Float) (Int_map)
  module String = Make (Std.String) (Std.Float) (String_map)
  module Float = Make (Std.Float) (Std.Float) (Float_map)
  module Bool = Make (Std.Bool) (Std.Float) (Bool_map)
  module Make_with_map (X : Basic_intf.Std) = Make_with_map (X) (Std.Float)
end
