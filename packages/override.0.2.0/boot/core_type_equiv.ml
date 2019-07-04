module OCaml_version = Migrate_parsetree.OCaml_407

module Parsetree = OCaml_version.Ast.Parsetree

let rec equal_list p l0 l1 =
  match l0, l1 with
  | [], [] -> true
  | hd0 :: tl0, hd1 :: tl1 -> p hd0 hd1 && equal_list p tl0 tl1
  | _ -> false

let equal_loc p (l0 : 'a Location.loc) (l1 : 'a Location.loc) =
  p l0.txt l1.txt

let equal_pair px py (x0, y0) (x1, y1) =
  px x0 x1 && py y0 y1

let equal_payload equal_core_type
    (p0 : Parsetree.payload) (p1 : Parsetree.payload) =
  match p0, p1 with
  | PStr s0, PStr s1 ->
      failwith "TODO [%%override]: equal_payload not implemented for structures"
  | PTyp t0, PTyp t1 -> equal_core_type t0 t1
  | PPat (p0, a0), PPat (p1, a1) ->
      failwith "TODO [%%override]: equal_payload not implemented for patterns"
  | _ -> false

let equal_attributes equal_core_type l0 l1 =
  equal_list (equal_pair ( = ) (equal_payload equal_core_type)) l0 l1

let equal_object_field equal_core_type
    (f0 : Parsetree.object_field) (f1 : Parsetree.object_field) =
  match f0, f1 with
  | Otag (l0, a0, t0), Otag (l1, a1, t1) ->
      l0.txt = l1.txt && equal_attributes equal_core_type a0 a1 &&
      equal_core_type t0 t1
  | Oinherit t0, Oinherit t1 -> equal_core_type t0 t1
  | _ -> false

let equal_row_field equal_core_type
    (f0 : Parsetree.row_field) (f1 : Parsetree.row_field) =
  match f0, f1 with
  | Rtag (l0, a0, b0, t0), Rtag (l1, a1, b1, t1) ->
      l0.txt = l1.txt && equal_attributes equal_core_type a0 a1 && b0 = b1 &&
      equal_list equal_core_type t0 t1
  | Rinherit t0, Rinherit t1 -> equal_core_type t0 t1
  | _ -> false

let equiv_core_type equiv_rec (t0 : Parsetree.core_type)
    (t1 : Parsetree.core_type) =
  equal_attributes equiv_rec t0.ptyp_attributes t1.ptyp_attributes &&
  match t0.ptyp_desc, t1.ptyp_desc with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var x0, Ptyp_var x1 -> x0 = x1
  | Ptyp_arrow (l0, u0, v0), Ptyp_arrow (l1, u1, v1) ->
      l0 = l1 && equiv_rec u0 u1 && equiv_rec v0 v1
  | Ptyp_tuple l0, Ptyp_tuple l1 ->
      equal_list equiv_rec l0 l1
  | Ptyp_constr (c0, a0), Ptyp_constr (c1, a1) ->
      c0.txt = c1.txt && equal_list equiv_rec a0 a1
  | Ptyp_object (f0, c0), Ptyp_object (f1, c1) ->
      c0 = c1 && equal_list (equal_object_field equiv_rec) f0 f1
  | Ptyp_class (c0, t0), Ptyp_class (c1, t1) ->
      c0.txt = c1.txt && equal_list equiv_rec t0 t1
  | Ptyp_alias (t0, x0), Ptyp_alias (t1, x1) ->
      equiv_rec t0 t1 && x0 = x1
  | Ptyp_variant (r0, c0, l0), Ptyp_variant (r1, c1, l1) ->
      equal_list (equal_row_field equiv_rec) r0 r1 && c0 = c1 && l0 = l1
  | Ptyp_poly (x0, t0), Ptyp_poly (x1, t1) ->
      equal_list (equal_loc ( = )) x0 x1 && equiv_rec t0 t1
  | Ptyp_package (p0, l0), Ptyp_package (p1, l1) ->
      p0.txt = p1.txt &&
      equal_list (equal_pair (equal_loc ( = )) equiv_rec) l0 l1
  | Ptyp_extension (x0, e0), Ptyp_extension (x1, e1) ->
      x0.txt = x1.txt && equal_payload equiv_rec e0 e1
  | _ -> false
