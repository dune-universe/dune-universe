let%c () = header {|
#include <gmp.h>
#include <zarith.h>
|}

module MPZ = struct
  let%c t = abstract "__mpz_struct"

  type ptr = t Ctypes.abstract Ctypes.ptr

  external clear : t ptr -> void = "mpz_clear" [@@noalloc]

  external init : t ptr -> void = "mpz_init"

  let make () =
    (* allocate_n zero initializes the memory. It's safe to pass
       such a struct to mpz_clear. *)
    let r = Ctypes.allocate_n ~finalise:clear t ~count:1 in
    init r;
    r

  [%%c
  external set : zt_:(Z.t[@ocaml_type]) -> tptr_:t ptr -> void
    = {|
   value z = $zt_; /* not converted. The usual rules for stub code must be
                      obeyed (accessors, memory management (GC), etc.) */
   __mpz_struct * p = $tptr_; /* already converted to a native c type, will not
                                 be garbage collected during the stub code */
   ml_z_mpz_set_z(p, z);
|}]

  let of_z x =
    let r = make () in
    set x r;
    r

  [%%c
  external to_z : ptr_:t ptr -> (Z.t[@ocaml_type])
    = {|
  return (ml_z_from_mpz($ptr_));
|}]

  let t_ptr = Ctypes.ptr t

  let zarith : Z.t Ctypes.typ =
    Ctypes.view
      ~format_typ:(fun k fmt -> Format.fprintf fmt "mpz_ptr%t" k)
      ~read:to_z ~write:of_z t_ptr
end

module MPQ = struct
  let%c t = abstract "__mpq_struct"

  type ptr = t Ctypes.abstract Ctypes.ptr

  external clear : t ptr -> void = "mpq_clear" [@@noalloc]

  external init : t ptr -> void = "mpq_init"

  let make () =
    (* allocate_n zero initializes the memory. It's safe to pass
       such a struct to mpz_clear. *)
    let r = Ctypes.allocate_n ~finalise:clear t ~count:1 in
    init r;
    r

  [%%c
  external set_zz :
    num_:(Z.t[@ocaml_type]) -> den_:(Z.t[@ocaml_type]) -> tptr_:t ptr -> void
    = {|
   value num = $num_; /* not converted. The usual rules for stub code must be
                      obeyed (accessors, memory management (GC), etc.) */
   value den = $den_; /* not converted. The usual rules for stub code must be
                      obeyed (accessors, memory management (GC), etc.) */
   __mpq_struct * p = $tptr_; /* already converted to a native c type, will not
                                 be garbage collected during the stub code */
   ml_z_mpz_set_z(&p->_mp_num, num);
   ml_z_mpz_set_z(&p->_mp_den, den);
   mpq_canonicalize(p);
|}]

  let set x r = set_zz (Q.num x) (Q.den x) r

  let of_q x =
    let r = make () in
    set x r;
    r

  [%%c
  external num : ptr_:t ptr -> (Z.t[@ocaml_type])
    = {|
  __mpq_struct * p = $ptr_;
  return (ml_z_from_mpz(&p->_mp_num));
|}]

  [%%c
  external den : ptr_:t ptr -> (Z.t[@ocaml_type])
    = {|
  __mpq_struct * p = $ptr_;
  return (ml_z_from_mpz(&p->_mp_den));
|}]

  let to_q x = Q.make (num x) (den x)

  let t_ptr = Ctypes.ptr t

  let zarith : Q.t Ctypes.typ =
    Ctypes.view
      ~format_typ:(fun k fmt -> Format.fprintf fmt "mpq_ptr%t" k)
      ~read:to_q ~write:of_q t_ptr
end
