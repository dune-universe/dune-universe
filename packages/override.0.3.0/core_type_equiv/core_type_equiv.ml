module%override Longident = struct
  [%%recursive [%%types]] [@@deriving refl]
end

module%override Location = struct
  type t = Location.t [@opaque] [@@rewrite] [@@deriving refl]

  type 'a loc = _ [@@deriving refl]
end

module%override Asttypes = struct
  [%%recursive [%%types]] [@@deriving refl]
end

module%override Parsetree = struct
  [%%recursive [%%types]] [@@deriving refl]
end

let equal_loc equal_txt (l1 : 'a Location.loc) (l2 : 'a Location.loc) =
  equal_txt l1.txt l2.txt

let equiv_core_type (equiv : Parsetree.core_type -> Parsetree.core_type -> bool)
    t0 t1 =
  let sub_hook : type a b . (a, b) Refl.Eq.hook_fun =
  fun refl0 refl1 super x0 x1 ->
    match refl0, refl1 with
    | Parsetree.Refl_core_type, Parsetree.Refl_core_type ->
        equiv x0 x1
    | _ ->
        super x0 x1 in
  let hook : type a b . (a, b) Refl.Eq.hook_fun =
  fun _refl0 _refl1 super x0 x1 ->
    super ~hook:{ hook = sub_hook } x0 x1 in
  Refl.equal ~hook:{ hook } [%refl: Parsetree.core_type] [] t0 t1
