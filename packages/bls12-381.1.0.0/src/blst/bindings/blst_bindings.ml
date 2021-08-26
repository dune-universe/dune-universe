(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type TYPES = sig
  (* Fr *)
  type blst_fr_t

  val blst_fr_t : blst_fr_t Ctypes.typ

  val allocate_fr : unit -> blst_fr_t Ctypes.ptr

  (* Scalar *)
  type blst_scalar_t

  val blst_scalar_t : blst_scalar_t Ctypes.typ

  val allocate_scalar : unit -> blst_scalar_t Ctypes.ptr

  (* Fq *)
  type blst_fq_t

  val blst_fq_t : blst_fq_t Ctypes.typ

  val allocate_fq : unit -> blst_fq_t Ctypes.ptr

  (* Fq2 *)
  type blst_fq2_t

  val blst_fq2_t : blst_fq2_t Ctypes.typ

  val allocate_fq2 : unit -> blst_fq2_t Ctypes.ptr

  val fq2_assign :
    blst_fq2_t Ctypes.ptr ->
    blst_fq_t Ctypes.ptr ->
    blst_fq_t Ctypes.ptr ->
    unit

  val fq2_get_x : blst_fq2_t Ctypes.ptr -> blst_fq_t Ctypes.ptr

  val fq2_get_y : blst_fq2_t Ctypes.ptr -> blst_fq_t Ctypes.ptr

  val fq2_get_fq_components :
    blst_fq2_t Ctypes.ptr -> blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr

  (* Fq *)
  type blst_fq6_t

  val blst_fq6_t : blst_fq6_t Ctypes.typ

  val allocate_fq6 : unit -> blst_fq6_t Ctypes.ptr

  val fq6_get_fq_components :
    blst_fq6_t Ctypes.ptr ->
    (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
    * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
    * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)

  (* Fq *)
  type blst_fq12_t

  val blst_fq12_t : blst_fq12_t Ctypes.typ

  val allocate_fq12 : unit -> blst_fq12_t Ctypes.ptr

  val fq12_get_fq_components :
    blst_fq12_t Ctypes.ptr ->
    ( (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
    * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
    * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr) )
    * ( (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
      * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr)
      * (blst_fq_t Ctypes.ptr * blst_fq_t Ctypes.ptr) )

  val fq12_assign_fq_component :
    blst_fq12_t Ctypes.ptr -> int -> blst_fq_t Ctypes.ptr -> unit

  (* G1 *)
  type blst_g1_t

  val blst_g1_t : blst_g1_t Ctypes.typ

  val allocate_g1 : unit -> blst_g1_t Ctypes.ptr

  type blst_g1_affine_t

  val blst_g1_affine_t : blst_g1_affine_t Ctypes.typ

  val allocate_g1_affine : unit -> blst_g1_affine_t Ctypes.ptr

  val g1_affine_set_x :
    blst_g1_affine_t Ctypes.ptr -> blst_fq_t Ctypes.ptr -> unit

  val g1_affine_set_y :
    blst_g1_affine_t Ctypes.ptr -> blst_fq_t Ctypes.ptr -> unit

  val g1_copy : blst_g1_t Ctypes.ptr -> blst_g1_t Ctypes.ptr

  (* G2 *)
  type blst_g2_t

  val blst_g2_t : blst_g2_t Ctypes.typ

  val allocate_g2 : unit -> blst_g2_t Ctypes.ptr

  val g2_copy : blst_g2_t Ctypes.ptr -> blst_g2_t Ctypes.ptr

  type blst_g2_affine_t

  val blst_g2_affine_t : blst_g2_affine_t Ctypes.typ

  val allocate_g2_affine : unit -> blst_g2_affine_t Ctypes.ptr

  val g2_affine_set_x :
    blst_g2_affine_t Ctypes.ptr -> blst_fq2_t Ctypes.ptr -> unit

  val g2_affine_set_y :
    blst_g2_affine_t Ctypes.ptr -> blst_fq2_t Ctypes.ptr -> unit

  type blst_pairing_t

  val blst_pairing_t : blst_pairing_t Ctypes.typ
end

module Types : TYPES = struct
  let limb_t = Ctypes.uint64_t

  let sizeof_limb_t = Ctypes.sizeof limb_t

  (* Fr *)
  type blst_fr

  type blst_fr_t = blst_fr Ctypes.structure

  let blst_fr_t : blst_fr_t Ctypes.typ =
    let nb_limb = 256 / (8 * sizeof_limb_t) in
    let l = Ctypes.array nb_limb limb_t in
    let anon_structure = Ctypes.structure "" in
    let _ = Ctypes.field anon_structure "l" l in
    let x = Ctypes.(typedef anon_structure "blst_fr") in
    Ctypes.seal x ;
    x

  (* We must be on arch 64 *)
  let () = assert (Ctypes.sizeof blst_fr_t = 32)

  let allocate_fr () =
    let x = Ctypes.make blst_fr_t in
    let addr = Ctypes.addr x in
    addr

  (* Scalar *)
  type blst_scalar

  type blst_scalar_t = blst_scalar Ctypes.structure

  let blst_scalar_t : blst_scalar_t Ctypes.typ =
    let nb_bytes = 32 in
    let field = Ctypes.array nb_bytes Ctypes.uint8_t in
    let anon_structure = Ctypes.structure "" in
    let _ = Ctypes.field anon_structure "b" field in
    let x = Ctypes.(typedef anon_structure "blst_scalar") in
    Ctypes.seal x ;
    x

  let allocate_scalar () =
    let x = Ctypes.make blst_scalar_t in
    let addr = Ctypes.addr x in
    addr

  (* Fq *)
  type blst_fq

  type blst_fq_t = blst_fq Ctypes.structure

  let blst_fq_t : blst_fq_t Ctypes.typ =
    let nb_limb = 384 / (8 * sizeof_limb_t) in
    let l = Ctypes.array nb_limb limb_t in
    let anon_structure = Ctypes.structure "" in
    let _ = Ctypes.field anon_structure "l" l in
    let x = Ctypes.(typedef anon_structure "blst_fp") in
    Ctypes.seal x ;
    x

  let allocate_fq () =
    let x = Ctypes.make blst_fq_t in
    let addr = Ctypes.addr x in
    addr

  (* Fq2 *)
  type blst_fq2

  type blst_fq2_t = blst_fq2 Ctypes.structure

  let blst_fq2_t_compo =
    let fq = Ctypes.array 2 blst_fq_t in
    let anon_structure = Ctypes.structure "" in
    let field_fq = Ctypes.field anon_structure "fp" fq in
    let x = Ctypes.(typedef anon_structure "blst_fp2") in
    Ctypes.seal x ;
    (anon_structure, field_fq, x)

  let blst_fq2_t : blst_fq2_t Ctypes.typ =
    let (_, _, p) = blst_fq2_t_compo in
    p

  let allocate_fq2 () =
    let x = Ctypes.make blst_fq2_t in
    let addr = Ctypes.addr x in
    addr

  let fq2_assign (p : blst_fq2_t Ctypes.ptr) (x : blst_fq_t Ctypes.ptr)
      (y : blst_fq_t Ctypes.ptr) =
    let (_, f, _) = blst_fq2_t_compo in
    let a = Ctypes.(getf !@p f) in
    Ctypes.(CArray.set a 0 !@x) ;
    Ctypes.(CArray.set a 1 !@y)

  let fq2_get_x (p : blst_fq2_t Ctypes.ptr) : blst_fq_t Ctypes.ptr =
    let (_, f, _) = blst_fq2_t_compo in
    let p = Ctypes.(getf !@p f) in
    Ctypes.(addr (CArray.get p 0))

  let fq2_get_y (p : blst_fq2_t Ctypes.ptr) : blst_fq_t Ctypes.ptr =
    let (_, f, _) = blst_fq2_t_compo in
    let p = Ctypes.(getf !@p f) in
    Ctypes.(addr (CArray.get p 1))

  let fq2_get_fq_components p =
    let (_, f, _) = blst_fq2_t_compo in
    let p = Ctypes.(getf !@p f) in
    (Ctypes.(addr (CArray.get p 0)), Ctypes.(addr (CArray.get p 1)))

  (* Fq6 *)
  type blst_fq6

  type blst_fq6_t = blst_fq6 Ctypes.structure

  let blst_fq6_t_compo =
    let fq = Ctypes.array 3 blst_fq2_t in
    let anon_structure = Ctypes.structure "" in
    let field_fq = Ctypes.field anon_structure "fp2" fq in
    let x = Ctypes.(typedef anon_structure "blst_fp6") in
    Ctypes.seal x ;
    (anon_structure, field_fq, x)

  let blst_fq6_t : blst_fq6_t Ctypes.typ =
    let (_, _, p) = blst_fq6_t_compo in
    p

  let allocate_fq6 () =
    let x = Ctypes.make blst_fq6_t in
    let addr = Ctypes.addr x in
    addr

  let fq6_get_fq_components x =
    let (_, field_fq2, _) = blst_fq6_t_compo in
    let p = Ctypes.(getf !@x field_fq2) in
    let x0 = Ctypes.(addr (CArray.get p 0)) in
    let x1 = Ctypes.(addr (CArray.get p 1)) in
    let x2 = Ctypes.(addr (CArray.get p 2)) in
    ( fq2_get_fq_components x0,
      fq2_get_fq_components x1,
      fq2_get_fq_components x2 )

  (* Fq12 *)
  type blst_fq12

  type blst_fq12_t = blst_fq12 Ctypes.structure

  let blst_fq12_t_compo =
    let fq = Ctypes.array 2 blst_fq6_t in
    let anon_structure = Ctypes.structure "" in
    let field_fq = Ctypes.field anon_structure "fp6" fq in
    let x = Ctypes.(typedef anon_structure "blst_fp12") in
    Ctypes.seal x ;
    (anon_structure, field_fq, x)

  let blst_fq12_t : blst_fq12_t Ctypes.typ =
    let (_, _, p) = blst_fq12_t_compo in
    p

  let allocate_fq12 () =
    let x = Ctypes.make blst_fq12_t in
    let addr = Ctypes.addr x in
    addr

  let fq12_get_fq_components x =
    let (_, field_fq6, _) = blst_fq12_t_compo in
    let p = Ctypes.(getf !@x field_fq6) in
    let fq6_x0 = Ctypes.(addr (CArray.get p 0)) in
    let fq6_x1 = Ctypes.(addr (CArray.get p 1)) in
    (fq6_get_fq_components fq6_x0, fq6_get_fq_components fq6_x1)

  let fq12_assign_fq_component x_ptr idx v_ptr =
    let (_, field_fq6, _) = blst_fq12_t_compo in
    let (idx_fq6, idx) = (idx / 6, idx mod 6) in
    let x_ptr = Ctypes.(getf !@x_ptr field_fq6) in
    let x_ptr = Ctypes.(addr (CArray.get x_ptr idx_fq6)) in
    let (idx_fq2, idx_fq) = (idx / 2, idx mod 2) in
    let (_, field_fq2, _) = blst_fq6_t_compo in
    let x_ptr = Ctypes.(getf !@x_ptr field_fq2) in
    let x_ptr = Ctypes.(addr (CArray.get x_ptr idx_fq2)) in
    let (_, field_fq, _) = blst_fq2_t_compo in
    let x_ptr = Ctypes.(getf !@x_ptr field_fq) in
    Ctypes.(CArray.(set x_ptr idx_fq !@v_ptr))

  (* G1 *)
  type blst_g1

  type blst_g1_t = blst_g1 Ctypes.structure

  let blst_g1_t_compo =
    let anon_structure = Ctypes.structure "" in
    let x_coord = Ctypes.field anon_structure "x" blst_fq_t in
    let y_coord = Ctypes.field anon_structure "y" blst_fq_t in
    let z_coord = Ctypes.field anon_structure "z" blst_fq_t in
    let x = Ctypes.(typedef anon_structure "blst_p1") in
    Ctypes.seal x ;
    ((x_coord, y_coord, z_coord), x)

  let blst_g1_t : blst_g1_t Ctypes.typ =
    let (_, x) = blst_g1_t_compo in
    x

  let allocate_g1 () =
    let x = Ctypes.make blst_g1_t in
    let addr = Ctypes.addr x in
    addr

  type blst_g1_affine

  type blst_g1_affine_t = blst_g1_affine Ctypes.structure

  let blst_g1_affine_t_compo =
    let anon_structure = Ctypes.structure "" in
    let field_x = Ctypes.field anon_structure "x" blst_fq_t in
    let field_y = Ctypes.field anon_structure "y" blst_fq_t in
    let blst_g1_affine_t = Ctypes.(typedef anon_structure "blst_p1_affine") in
    Ctypes.seal blst_g1_affine_t ;
    (anon_structure, field_x, field_y, blst_g1_affine_t)

  let blst_g1_affine_t : blst_g1_affine_t Ctypes.typ =
    let (_, _, _, t) = blst_g1_affine_t_compo in
    t

  let allocate_g1_affine () =
    let x = Ctypes.make blst_g1_affine_t in
    let addr = Ctypes.addr x in
    addr

  let g1_affine_set_x p x =
    let (_, field_x, _, _) = blst_g1_affine_t_compo in
    Ctypes.(setf !@p field_x !@x)

  let g1_affine_set_y p y =
    let (_, _, field_y, _) = blst_g1_affine_t_compo in
    Ctypes.(setf !@p field_y !@y)

  let g1_set_x p v_x =
    let ((x_coord, _, _), _) = blst_g1_t_compo in
    Ctypes.(setf !@p x_coord !@v_x)

  let g1_set_y p v_y =
    let ((_, y_coord, _), _) = blst_g1_t_compo in
    Ctypes.(setf !@p y_coord !@v_y)

  let g1_set_z p v_z =
    let ((_, _, z_coord), _) = blst_g1_t_compo in
    Ctypes.(setf !@p z_coord !@v_z)

  let g1_copy p =
    let buffer = allocate_g1 () in
    let ((x_coord, y_coord, z_coord), _) = blst_g1_t_compo in
    g1_set_x buffer Ctypes.(addr (getf !@p x_coord)) ;
    g1_set_y buffer Ctypes.(addr (getf !@p y_coord)) ;
    g1_set_z buffer Ctypes.(addr (getf !@p z_coord)) ;
    buffer

  (* G2 *)
  type blst_g2

  type blst_g2_t = blst_g2 Ctypes.structure

  let blst_g2_t_compo =
    let anon_structure = Ctypes.structure "" in
    let x_coord = Ctypes.field anon_structure "x" blst_fq2_t in
    let y_coord = Ctypes.field anon_structure "y" blst_fq2_t in
    let z_coord = Ctypes.field anon_structure "z" blst_fq2_t in
    let x = Ctypes.(typedef anon_structure "blst_p2") in
    Ctypes.seal x ;
    ((x_coord, y_coord, z_coord), x)

  let blst_g2_t : blst_g2_t Ctypes.typ =
    let (_, x) = blst_g2_t_compo in
    x

  let g2_set_x p v_x =
    let ((x_coord, _, _), _) = blst_g2_t_compo in
    Ctypes.(setf !@p x_coord !@v_x)

  let g2_set_y p v_y =
    let ((_, y_coord, _), _) = blst_g2_t_compo in
    Ctypes.(setf !@p y_coord !@v_y)

  let g2_set_z p v_z =
    let ((_, _, z_coord), _) = blst_g2_t_compo in
    Ctypes.(setf !@p z_coord !@v_z)

  let allocate_g2 () =
    let x = Ctypes.make blst_g2_t in
    let addr = Ctypes.addr x in
    addr

  let g2_copy p =
    let buffer = allocate_g2 () in
    let ((x_coord, y_coord, z_coord), _) = blst_g2_t_compo in
    g2_set_x buffer Ctypes.(addr (getf !@p x_coord)) ;
    g2_set_y buffer Ctypes.(addr (getf !@p y_coord)) ;
    g2_set_z buffer Ctypes.(addr (getf !@p z_coord)) ;
    buffer

  type blst_g2_affine

  type blst_g2_affine_t = blst_g2_affine Ctypes.structure

  let blst_g2_affine_t_compo =
    let anon_structure = Ctypes.structure "" in
    let field_x = Ctypes.field anon_structure "x" blst_fq2_t in
    let field_y = Ctypes.field anon_structure "y" blst_fq2_t in
    let blst_g2_affine_t = Ctypes.(typedef anon_structure "blst_p2_affine") in
    Ctypes.seal blst_g2_affine_t ;
    (anon_structure, field_x, field_y, blst_g2_affine_t)

  let blst_g2_affine_t : blst_g2_affine_t Ctypes.typ =
    let (_, _, _, p) = blst_g2_affine_t_compo in
    p

  let allocate_g2_affine () =
    let x = Ctypes.make blst_g2_affine_t in
    let addr = Ctypes.addr x in
    addr

  let g2_affine_set_x p x =
    let (_, field_x, _, _) = blst_g2_affine_t_compo in
    Ctypes.(setf !@p field_x !@x)

  let g2_affine_set_y p y =
    let (_, _, field_y, _) = blst_g2_affine_t_compo in
    Ctypes.(setf !@p field_y !@y)

  type blst_pairing

  type blst_pairing_t = blst_pairing Ctypes.structure

  let blst_pairing_t : blst_pairing_t Ctypes.typ =
    let anon_structure = Ctypes.structure "" in
    (* only adding one field to avoid having "struct with no fields" *)
    let _field_ctrl = Ctypes.field anon_structure "ctrl" Ctypes.uint in
    let blst_pairing_t = Ctypes.(typedef anon_structure "blst_pairing") in
    Ctypes.seal blst_pairing_t ;
    blst_pairing_t
end

module StubsFr (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let scalar_of_fr =
    foreign
      "blst_scalar_from_fr"
      (ptr blst_scalar_t @-> ptr blst_fr_t @-> returning void)

  let fr_of_scalar =
    foreign
      "blst_fr_from_scalar"
      (ptr blst_fr_t @-> ptr blst_scalar_t @-> returning void)

  let scalar_of_bytes_le =
    foreign
      "blst_scalar_from_lendian"
      (ptr blst_scalar_t @-> ocaml_bytes @-> returning void)

  let scalar_to_bytes_le =
    foreign
      "blst_lendian_from_scalar"
      (ocaml_bytes @-> ptr blst_scalar_t @-> returning void)

  let add =
    foreign
      "blst_fr_add"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> ptr blst_fr_t @-> returning void)

  let sub =
    foreign
      "blst_fr_sub"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> ptr blst_fr_t @-> returning void)

  let lshift =
    foreign
      "blst_fr_lshift"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> size_t @-> returning void)

  let rshift =
    foreign
      "blst_fr_rshift"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> size_t @-> returning void)

  let mul =
    foreign
      "blst_fr_mul"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> ptr blst_fr_t @-> returning void)

  let sqr =
    foreign "blst_fr_sqr" (ptr blst_fr_t @-> ptr blst_fr_t @-> returning void)

  (* blst_fr_inverse has the same implementation *)
  let eucl_inverse =
    foreign
      "blst_fr_eucl_inverse"
      (ptr blst_fr_t @-> ptr blst_fr_t @-> returning void)

  let check_scalar =
    foreign "blst_scalar_fr_check" (ptr blst_scalar_t @-> returning bool)
end

module StubsFq (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let of_bytes_le =
    foreign
      "blst_fp_from_lendian"
      (ptr blst_fq_t @-> ocaml_bytes @-> returning void)

  let to_bytes_le =
    foreign
      "blst_lendian_from_fp"
      (ocaml_bytes @-> ptr blst_fq_t @-> returning void)

  let add =
    foreign
      "blst_fp_add"
      (ptr blst_fq_t @-> ptr blst_fq_t @-> ptr blst_fq_t @-> returning void)

  let mul =
    foreign
      "blst_fp_mul"
      (ptr blst_fq_t @-> ptr blst_fq_t @-> ptr blst_fq_t @-> returning void)

  let sqrt =
    foreign "blst_fp_sqrt" (ptr blst_fq_t @-> ptr blst_fq_t @-> returning bool)

  let cneg =
    foreign
      "blst_fp_cneg"
      (ptr blst_fq_t @-> ptr blst_fq_t @-> bool @-> returning void)
end

module StubsFq2 (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let add =
    foreign
      "blst_fp2_add"
      (ptr blst_fq2_t @-> ptr blst_fq2_t @-> ptr blst_fq2_t @-> returning void)

  let mul =
    foreign
      "blst_fp2_mul"
      (ptr blst_fq2_t @-> ptr blst_fq2_t @-> ptr blst_fq2_t @-> returning void)

  let sqrt =
    foreign
      "blst_fp2_sqrt"
      (ptr blst_fq2_t @-> ptr blst_fq2_t @-> returning bool)

  let cneg =
    foreign
      "blst_fp2_cneg"
      (ptr blst_fq2_t @-> ptr blst_fq2_t @-> bool @-> returning void)
end

module StubsG1 (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let to_affine =
    foreign
      "blst_p1_to_affine"
      (ptr blst_g1_affine_t @-> ptr blst_g1_t @-> returning void)

  let from_affine =
    foreign
      "blst_p1_from_affine"
      (ptr blst_g1_t @-> ptr blst_g1_affine_t @-> returning void)

  let double =
    foreign "blst_p1_double" (ptr blst_g1_t @-> ptr blst_g1_t @-> returning void)

  (* Must be used for the complete version *)
  let dadd =
    foreign
      "blst_p1_add_or_double"
      (ptr blst_g1_t @-> ptr blst_g1_t @-> ptr blst_g1_t @-> returning void)

  let is_zero = foreign "blst_p1_is_inf" (ptr blst_g1_t @-> returning bool)

  let in_g1 = foreign "blst_p1_in_g1" (ptr blst_g1_t @-> returning bool)

  let equal =
    foreign
      "blst_p1_is_equal"
      (ptr blst_g1_t @-> ptr blst_g1_t @-> returning bool)

  let cneg = foreign "blst_p1_cneg" (ptr blst_g1_t @-> bool @-> returning void)

  let mult =
    foreign
      "blst_p1_mult"
      ( ptr blst_g1_t @-> ptr blst_g1_t @-> ocaml_bytes @-> size_t
      @-> returning void )

  let serialize =
    foreign
      "blst_p1_serialize"
      (ocaml_bytes @-> ptr blst_g1_t @-> returning void)

  let compress =
    foreign "blst_p1_compress" (ocaml_bytes @-> ptr blst_g1_t @-> returning void)

  let deserialize =
    foreign
      "blst_p1_deserialize"
      (ptr blst_g1_affine_t @-> ocaml_bytes @-> returning int)

  let uncompress =
    foreign
      "blst_p1_uncompress"
      (ptr blst_g1_affine_t @-> ocaml_bytes @-> returning int)

  let hash_to_curve =
    foreign
      "blst_hash_to_g1"
      ( ptr blst_g1_t @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t
      @-> ocaml_bytes @-> size_t @-> returning void )
end

module StubsG2 (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let to_affine =
    foreign
      "blst_p2_to_affine"
      (ptr blst_g2_affine_t @-> ptr blst_g2_t @-> returning void)

  let from_affine =
    foreign
      "blst_p2_from_affine"
      (ptr blst_g2_t @-> ptr blst_g2_affine_t @-> returning void)

  (* Must be used for the complete version *)
  let dadd =
    foreign
      "blst_p2_add_or_double"
      (ptr blst_g2_t @-> ptr blst_g2_t @-> ptr blst_g2_t @-> returning void)

  let double =
    foreign "blst_p2_double" (ptr blst_g2_t @-> ptr blst_g2_t @-> returning void)

  let is_zero = foreign "blst_p2_is_inf" (ptr blst_g2_t @-> returning bool)

  let equal =
    foreign
      "blst_p2_is_equal"
      (ptr blst_g2_t @-> ptr blst_g2_t @-> returning bool)

  let cneg = foreign "blst_p2_cneg" (ptr blst_g2_t @-> bool @-> returning void)

  let in_g2 = foreign "blst_p2_in_g2" (ptr blst_g2_t @-> returning bool)

  let mult =
    foreign
      "blst_p2_mult"
      ( ptr blst_g2_t @-> ptr blst_g2_t @-> ocaml_bytes @-> size_t
      @-> returning void )

  let serialize =
    foreign
      "blst_p2_serialize"
      (ocaml_bytes @-> ptr blst_g2_t @-> returning void)

  let compress =
    foreign "blst_p2_compress" (ocaml_bytes @-> ptr blst_g2_t @-> returning void)

  let deserialize =
    foreign
      "blst_p2_deserialize"
      (ptr blst_g2_affine_t @-> ocaml_bytes @-> returning int)

  let uncompress =
    foreign
      "blst_p2_uncompress"
      (ptr blst_g2_affine_t @-> ocaml_bytes @-> returning int)

  let hash_to_curve =
    foreign
      "blst_hash_to_g2"
      ( ptr blst_g2_t @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t
      @-> ocaml_bytes @-> size_t @-> returning void )
end

module StubsFq12 (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let mul =
    foreign
      "blst_fp12_mul"
      ( ptr blst_fq12_t @-> ptr blst_fq12_t @-> ptr blst_fq12_t
      @-> returning void )

  let equal =
    foreign
      "blst_fp12_is_equal"
      (ptr blst_fq12_t @-> ptr blst_fq12_t @-> returning bool)

  let is_one = foreign "blst_fp12_is_one" (ptr blst_fq12_t @-> returning bool)

  let inverse =
    foreign
      "blst_fp12_inverse"
      (ptr blst_fq12_t @-> ptr blst_fq12_t @-> returning void)

  let sqr =
    foreign
      "blst_fp12_sqr"
      (ptr blst_fq12_t @-> ptr blst_fq12_t @-> returning void)
end

module StubsPairing (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let miller_loop =
    foreign
      "blst_miller_loop"
      ( ptr blst_fq12_t @-> ptr blst_g2_affine_t @-> ptr blst_g1_affine_t
      @-> returning void )

  let final_exponentiation =
    foreign
      "blst_final_exp"
      (ptr blst_fq12_t @-> ptr blst_fq12_t @-> returning void)
end

module StubsSignature (S : Cstubs.FOREIGN) = struct
  open Ctypes
  open Types
  open S

  let keygen =
    foreign
      "blst_keygen"
      ( ptr blst_scalar_t @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t
      @-> returning void )

  let sk_to_pk =
    foreign
      "blst_sk_to_pk_in_g1"
      (ptr blst_g1_t @-> ptr blst_scalar_t @-> returning void)

  let sign =
    foreign
      "blst_sign_pk_in_g1"
      (ptr blst_g2_t @-> ptr blst_g2_t @-> ptr blst_scalar_t @-> returning void)

  let pairing_init =
    foreign
      "blst_pairing_init"
      (ptr blst_pairing_t @-> bool @-> ocaml_bytes @-> size_t @-> returning void)

  let core_verify =
    foreign
      "blst_core_verify_pk_in_g1"
      ( ptr blst_g1_affine_t @-> ptr blst_g2_affine_t @-> bool @-> ocaml_bytes
      @-> size_t @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t
      @-> returning int )

  let aggregate_signature =
    foreign
      "blst_pairing_aggregate_pk_in_g1"
      ( ptr blst_pairing_t @-> ptr blst_g1_affine_t @-> ptr blst_g2_affine_t
      @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t @-> returning int )

  let sizeof_pairing = foreign "blst_pairing_sizeof" (void @-> returning size_t)

  let malloc = foreign "malloc" (size_t @-> returning (ptr void))

  let free = foreign "free" (ptr void @-> returning void)

  let pairing_commit =
    foreign "blst_pairing_commit" (ptr blst_pairing_t @-> returning void)

  let pairing_finalverify =
    foreign
      "blst_pairing_finalverify"
      (ptr blst_pairing_t @-> ptr blst_fq12_t @-> returning bool)

  let pairing_chk_n_mul_n_aggr_pk_in_g1 =
    foreign
      "blst_pairing_chk_n_mul_n_aggr_pk_in_g1"
      ( ptr blst_pairing_t @-> ptr blst_g1_affine_t @-> bool
      @-> ptr blst_g2_affine_t @-> bool @-> ocaml_bytes @-> size_t
      @-> ocaml_bytes @-> size_t @-> ocaml_bytes @-> size_t @-> returning int )
end
