open Bitwuzla_c

external set_termination_callback : t -> ('a -> int) * 'a -> unit
  = "ocaml_bitwuzla_set_termination_callback"

module Session () = struct

  type nonrec 'a sort = sort
  type nonrec 'a term = term
  type 'a value = 'a term

  type bv = [ `Bv ]
  type rm = [ `Rm ]
  type fp = [ `Fp ]

  type ('a, 'b) ar = [ `Ar of ('a -> 'b) ]
    constraint 'a = [< bv | rm | fp ]
    constraint 'b = [< bv | rm | fp ]

  type ('a, 'b) fn = [ `Fn of ('a -> 'b) ]
    constraint 'b = [< bv ]

  let t = create ()
  let () = set_option t Produce_models 2

  let unsafe_close () = delete t

  module Sort = struct

    type 'a t = 'a sort

    let bool = mk_bool_sort t

    let bv size = mk_bv_sort t size
    let size = sort_bv_get_size

    let fp ~exponent size = mk_fp_sort t exponent @@ size - exponent
    let exponent = sort_fp_get_exp_size
    let significand = sort_fp_get_sig_size

    let rm = mk_rm_sort t

    let ar index element = mk_array_sort t index element
    let index = sort_array_get_index
    let element = sort_array_get_element

    type 'a variadic =
      |  []  : unit variadic
      | (::) : ([< bv | rm | fp ] as 'a) sort * 'b variadic ->
          ('a -> 'b) variadic
    external of_variadic : 'a variadic -> Bitwuzla_c.sort list = "%identity"
    external to_variadic : Bitwuzla_c.sort list -> 'a variadic = "%identity"

    let fn a b = mk_fun_sort t (Array.of_list @@ of_variadic a) b

    let arity = sort_fun_get_arity
    let domain s = to_variadic @@ Array.to_list @@ sort_fun_get_domain_sorts s
    let codomain s = sort_fun_get_codomain s

    let hash = sort_hash

    let equal = sort_is_equal

    let pp f x = sort_dump x `Smt2 f

  end

  module Term = struct

    type 'a t = 'a term
      constraint 'a = [< bv | rm | fp | ('b, 'c) ar ]
    type 'a variadic =
      |  []  : unit variadic
      | (::) : ([< bv | rm | fp ] as 'a) term * 'b variadic ->
          ('a -> 'b) variadic
    external of_variadic : 'a variadic -> Bitwuzla_c.term list = "%identity"
    external to_variadic : Bitwuzla_c.term list -> 'a variadic = "%identity"

    let ite x0 x1 x2 = mk_term3 t Ite x0 x1 x2
    let equal x0 x1 = mk_term2 t Equal x0 x1
    let distinct x0 x1 = mk_term2 t Distinct x0 x1

    let bl_of_bv bv = mk_term1 t Bv_redor bv

    let const s n = mk_const t s n

    let hash x = term_hash x
    let sort x = term_get_sort x

    let pp f x =
      try Format.pp_print_string f @@ term_get_symbol x
      with Not_found -> term_dump x `Smt2 f

    module Bl = struct

      type t = bv term

      let false' = mk_false t
      let true'  = mk_true t

      let logand b0 b1 = mk_term2 t And b0 b1
      let lognand b0 b1 = mk_term2 t Bv_nand b0 b1
      let redand bs = mk_term t And bs
      let logor b0 b1 = mk_term2 t Or b0 b1
      let lognor b0 b1 = mk_term2 t Bv_nor b0 b1
      let redor bs = mk_term t Or bs
      let logxor b0 b1 = mk_term2 t Xor b0 b1
      let logxnor b0 b1 = mk_term2 t Bv_xnor b0 b1
      let redxor bs = mk_term t Xor bs
      let lognot b = mk_term1 t Not b

      let iff b0 b1 = mk_term2 t Iff b0 b1
      let implies b0 b1 = mk_term2 t Implies b0 b1

      let of_bool b = if b then true' else false'
      let of_bv = bl_of_bv

      let assignment b = not @@ term_is_bv_value_zero b

    end

    module Bv = struct

      type t = bv term

      let zero s = mk_bv_zero t s
      let one s = mk_bv_one t s
      let ones s = mk_bv_ones t s

      let min_signed s = mk_bv_min_signed t s
      let max_signed s = mk_bv_max_signed t s

      let of_int s v = mk_bv_value_int t s v
      let of_string s v =
        let len = String.length v in
        if len > 2 && String.get v 0 = '0' then
          if String.get v 1 = 'b' then
            mk_bv_value t s (String.sub v 2 (len - 2)) Bin
          else if String.get v 1 = 'x' then
            mk_bv_value t s (String.sub v 2 (len - 2)) Hex
          else mk_bv_value t s v Dec
        else mk_bv_value t s v Dec
      let of_z s v =
        if Z.fits_int v then
          of_int s @@ Z.to_int v
        else mk_bv_value t s (Z.format "%x" v) Hex

      type ('a, 'b) operator =
        | Add             : (t -> t -> t, t * t) operator
        | And             : (t -> t -> t, t * t) operator
        | Ashr            : (t -> t -> t, t * t) operator
        | Concat          : (t -> t -> t, t * t) operator
        | Extract         :
            (hi:int -> lo:int -> t -> t, int * int * t) operator
        | Mul             : (t -> t -> t, t * t) operator
        | Neg             : (t -> t, t) operator
        | Not             : (t -> t, t) operator
        | Or              : (t -> t -> t, t * t) operator
        | Rol             : (t -> t -> t, t * t) operator
        | Ror             : (t -> t -> t, t * t) operator
        | Sdiv            : (t -> t -> t, t * t) operator
        | Sge             : (t -> t -> t, t * t) operator
        | Sgt             : (t -> t -> t, t * t) operator
        | Shl             : (t -> t -> t, t * t) operator
        | Shr             : (t -> t -> t, t * t) operator
        | Sle             : (t -> t -> t, t * t) operator
        | Slt             : (t -> t -> t, t * t) operator
        | Smod            : (t -> t -> t, t * t) operator
        | Srem            : (t -> t -> t, t * t) operator
        | Sub             : (t -> t -> t, t * t) operator
        | Udiv            : (t -> t -> t, t * t) operator
        | Uge             : (t -> t -> t, t * t) operator
        | Ugt             : (t -> t -> t, t * t) operator
        | Ule             : (t -> t -> t, t * t) operator
        | Ult             : (t -> t -> t, t * t) operator
        | Urem            : (t -> t -> t, t * t) operator
        | Xor             : (t -> t -> t, t * t) operator

      let term : type a b. (a, b) operator -> a = function
        | Add           -> mk_term2 t Bv_add
        | And           -> mk_term2 t Bv_and
        | Ashr          -> mk_term2 t Bv_ashr
        | Concat        -> mk_term2 t Bv_concat
        | Extract       ->
          fun ~hi ~lo e -> mk_term1_indexed2 t Bv_extract e hi lo
        | Mul           -> mk_term2 t Bv_mul
        | Neg           -> mk_term1 t Bv_neg
        | Not           -> mk_term1 t Bv_not
        | Or            -> mk_term2 t Bv_or
        | Rol           -> mk_term2 t Bv_rol
        | Ror           -> mk_term2 t Bv_ror
        | Sdiv          -> mk_term2 t Bv_sdiv
        | Sge           -> mk_term2 t Bv_sge
        | Sgt           -> mk_term2 t Bv_sgt
        | Shl           -> mk_term2 t Bv_shl
        | Shr           -> mk_term2 t Bv_shr
        | Sle           -> mk_term2 t Bv_sle
        | Slt           -> mk_term2 t Bv_slt
        | Smod          -> mk_term2 t Bv_smod
        | Srem          -> mk_term2 t Bv_srem
        | Sub           -> mk_term2 t Bv_sub
        | Udiv          -> mk_term2 t Bv_udiv
        | Uge           -> mk_term2 t Bv_uge
        | Ugt           -> mk_term2 t Bv_ugt
        | Ule           -> mk_term2 t Bv_ule
        | Ult           -> mk_term2 t Bv_ult
        | Urem          -> mk_term2 t Bv_urem
        | Xor           -> mk_term2 t Bv_xor

      let pred e = mk_term1 t Bv_dec e
      let succ e = mk_term1 t Bv_inc e
      let neg e = mk_term1 t Bv_neg e
      let add e0 e1 = mk_term2 t Bv_add e0 e1
      let sadd_overflow e0 e1 = mk_term2 t Bv_sadd_overflow e0 e1
      let uadd_overflow e0 e1 = mk_term2 t Bv_uadd_overflow e0 e1
      let sub e0 e1 = mk_term2 t Bv_sub e0 e1
      let ssub_overflow e0 e1 = mk_term2 t Bv_ssub_overflow e0 e1
      let usub_overflow e0 e1 = mk_term2 t Bv_usub_overflow e0 e1
      let mul e0 e1 = mk_term2 t Bv_mul e0 e1
      let smul_overflow e0 e1 = mk_term2 t Bv_smul_overflow e0 e1
      let umul_overflow e0 e1 = mk_term2 t Bv_umul_overflow e0 e1
      let sdiv e0 e1 = mk_term2 t Bv_sdiv e0 e1
      let sdiv_overflow e0 e1 = mk_term2 t Bv_sdiv_overflow e0 e1
      let udiv e0 e1 = mk_term2 t Bv_udiv e0 e1
      let smod e0 e1 = mk_term2 t Bv_smod e0 e1
      let srem e0 e1 = mk_term2 t Bv_srem e0 e1
      let urem e0 e1 = mk_term2 t Bv_urem e0 e1

      let logand e0 e1 = mk_term2 t Bv_and e0 e1
      let lognand e0 e1 = mk_term2 t Bv_nand e0 e1
      let redand e = mk_term1 t Bv_redand e
      let logor e0 e1 = mk_term2 t Bv_or e0 e1
      let lognor e0 e1 = mk_term2 t Bv_nor e0 e1
      let redor e = mk_term1 t Bv_redor e
      let logxor e0 e1 = mk_term2 t Bv_xor e0 e1
      let logxnor e0 e1 = mk_term2 t Bv_xnor e0 e1
      let redxor e = mk_term1 t Bv_redxor e
      let lognot e = mk_term1 t Bv_not e

      let shift_left e0 e1 = mk_term2 t Bv_shl e0 e1
      let shift_right_logical e0 e1 = mk_term2 t Bv_shr e0 e1
      let shift_right e0 e1 = mk_term2 t Bv_ashr e0 e1

      let rotate_left e0 e1 = mk_term2 t Bv_rol e0 e1
      let rotate_lefti e i = mk_term1_indexed1 t Bv_roli e i
      let rotate_right e0 e1 = mk_term2 t Bv_ror e0 e1
      let rotate_righti e i = mk_term1_indexed1 t Bv_rori e i

      let zero_extend i e = mk_term1_indexed1 t Bv_zero_extend e i
      let sign_extend i e = mk_term1_indexed1 t Bv_sign_extend e i
      let append e0 e1 = mk_term2 t Bv_concat e0 e1
      let concat es = mk_term t Bv_concat es
      let repeat i e = mk_term1_indexed1 t Bv_repeat e i

      let extract ~hi ~lo e = mk_term1_indexed2 t Bv_extract e hi lo

      let sge e0 e1 = mk_term2 t Bv_sge e0 e1
      let uge e0 e1 = mk_term2 t Bv_uge e0 e1
      let sgt e0 e1 = mk_term2 t Bv_sgt e0 e1
      let ugt e0 e1 = mk_term2 t Bv_ugt e0 e1
      let sle e0 e1 = mk_term2 t Bv_sle e0 e1
      let ule e0 e1 = mk_term2 t Bv_ule e0 e1
      let slt e0 e1 = mk_term2 t Bv_slt e0 e1
      let ult e0 e1 = mk_term2 t Bv_ult e0 e1

      let to_bl = bl_of_bv

      external assignment : t -> Z.t = "ocaml_bitwuzla_value_get_bv_bits"

    end

    module Rm = struct

      type t = rm term

      type 'a operator =
        | Rne : rm term operator
        | Rna : rm term operator
        | Rtn : rm term operator
        | Rtp : rm term operator
        | Rtz : rm term operator

      let rne = mk_rm_value t Rne
      let rna = mk_rm_value t Rna
      let rtn = mk_rm_value t Rtn
      let rtp = mk_rm_value t Rtp
      let rtz = mk_rm_value t Rtz

      let term : type a. a operator -> a = function
        | Rne -> rne
        | Rna -> rna
        | Rtn -> rtn
        | Rtp -> rtp
        | Rtz -> rtz

    end

    module Fp = struct

      type t = fp term

      let pos_zero s = mk_fp_pos_zero t s
      let neg_zero s = mk_fp_neg_zero t s
      let pos_inf s = mk_fp_pos_inf t s
      let neg_inf s = mk_fp_neg_inf t s
      let nan s = mk_fp_nan t s

      let of_real s rm v = mk_fp_value_from_real t s (Rm.term rm) v
      let of_rational s rm ~num ~den =
        mk_fp_value_from_rational t s (Rm.term rm) num den
      let of_float s rm f = of_real s rm @@ string_of_float f

      type ieee_754 =
        { sign: bv term; exponent: bv term; significand: bv term }
      type ('a, 'b, 'c) operator =
        | Abs          : (t -> t, t, fp) operator
        | Add          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        | Div          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        | Eq           : (t -> t -> bv term, t * t, bv) operator
        | Fma          :
            (rm term -> t -> t -> t -> t, rm term * t * t * t, fp) operator
        | Fp           :
            (sign:bv term -> exponent:bv term -> bv term -> t,
             ieee_754, fp) operator
        | Geq          : (t -> t -> bv term, t * t, bv) operator
        | Gt           : (t -> t -> bv term, t * t, bv) operator
        | Is_inf       : (t -> bv term, t, bv) operator
        | Is_nan       : (t -> bv term, t, bv) operator
        | Is_neg       : (t -> bv term, t, bv) operator
        | Is_normal    : (t -> bv term, t, bv) operator
        | Is_pos       : (t -> bv term, t, bv) operator
        | Is_subnormal : (t -> bv term, t, bv) operator
        | Is_zero      : (t -> bv term, t, bv) operator
        | Leq          : (t -> t -> bv term, t * t, bv) operator
        | Lt           : (t -> t -> bv term, t * t, bv) operator
        | Max          : (t -> t -> t, t * t, fp) operator
        | Min          : (t -> t -> t, t * t, fp) operator
        | Mul          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        | Neg          : (t -> t, t, fp) operator
        | Rem          : (t -> t -> t, t * t, fp) operator
        | Rti          : (rm term -> t -> t, rm term * t, fp) operator
        | Sqrt         : (rm term -> t -> t, rm term * t, fp) operator
        | Sub          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        | From_bv      : (exponent:int -> int -> bv term -> t,
                          int * int * bv term, fp) operator
        | From_fp      :
            (exponent:int -> int -> rm term -> t -> t,
             int * int * rm term * t, fp) operator
        | From_sbv     :
            (exponent:int -> int -> rm term -> bv term -> t,
             int * int * rm term * bv term, fp) operator
        | From_ubv     :
            (exponent:int -> int -> rm term -> bv term -> t,
             int * int * rm term * bv term, fp) operator
        | To_sbv       : (int -> rm term -> t -> bv term,
                          int * rm term * t, bv) operator
        | To_ubv       : (int -> rm term -> t -> bv term,
                          int * rm term * t, bv) operator

      let term : type a b c. (a, b, c) operator -> a = function
        | Abs          -> mk_term1 t Fp_abs
        | Add          -> mk_term3 t Fp_add
        | Div          -> mk_term3 t Fp_div
        | Eq           -> mk_term2 t Fp_eq
        | Fma          ->
          fun rm f0 f1 f2 -> mk_term t Fp_fma [| rm; f0; f1; f2 |]
        | Fp           ->
          fun ~sign ~exponent significand ->
            mk_term3 t Fp_fp sign exponent significand
        | Geq          -> mk_term2 t Fp_geq
        | Gt           -> mk_term2 t Fp_gt
        | Is_inf       -> mk_term1 t Fp_is_inf
        | Is_nan       -> mk_term1 t Fp_is_nan
        | Is_neg       -> mk_term1 t Fp_is_neg
        | Is_normal    -> mk_term1 t Fp_is_normal
        | Is_pos       -> mk_term1 t Fp_is_pos
        | Is_subnormal -> mk_term1 t Fp_is_subnormal
        | Is_zero      -> mk_term1 t Fp_is_zero
        | Leq          -> mk_term2 t Fp_leq
        | Lt           -> mk_term2 t Fp_lt
        | Max          -> mk_term2 t Fp_max
        | Min          -> mk_term2 t Fp_min
        | Mul          -> mk_term3 t Fp_mul
        | Neg          -> mk_term1 t Fp_neg
        | Rem          -> mk_term2 t Fp_rem
        | Rti          -> mk_term2 t Fp_rti
        | Sqrt         -> mk_term2 t Fp_sqrt
        | Sub          -> mk_term3 t Fp_sub
        | From_bv      ->
          fun ~exponent size bv ->
            mk_term1_indexed2 t Fp_to_fp_from_bv bv exponent (size - exponent)
        | From_fp      ->
          fun ~exponent size rm bv ->
            mk_term2_indexed2 t Fp_to_fp_from_fp
              rm bv exponent (size - exponent)
        | From_sbv     ->
          fun ~exponent size rm bv ->
            mk_term2_indexed2 t Fp_to_fp_from_sbv
              rm bv exponent (size - exponent)
        | From_ubv     ->
          fun ~exponent size rm bv ->
            mk_term2_indexed2 t Fp_to_fp_from_ubv
              rm bv exponent (size - exponent)
        | To_sbv       ->
          fun size rm fp -> mk_term2_indexed1 t Fp_to_sbv rm fp size
        | To_ubv       ->
          fun size rm fp -> mk_term2_indexed1 t Fp_to_ubv rm fp size

      let make ~sign ~exponent significand = mk_term3 t Fp_fp
          sign exponent significand
      let of_sbv ~exponent size rm bv =
        mk_term2_indexed2 t Fp_to_fp_from_sbv rm bv exponent (size - exponent)
      let of_ubv ~exponent size rm bv =
        mk_term2_indexed2 t Fp_to_fp_from_ubv rm bv exponent (size - exponent)
      let of_bv ~exponent size bv =
        mk_term1_indexed2 t Fp_to_fp_from_bv bv exponent (size - exponent)
      let of_fp ~exponent size rm bv =
        mk_term2_indexed2 t Fp_to_fp_from_fp rm bv exponent (size - exponent)

      let abs f = mk_term1 t Fp_abs f
      let neg f = mk_term1 t Fp_neg f
      let add rm f0 f1  = mk_term3 t Fp_add rm f0 f1
      let sub rm f0 f1  = mk_term3 t Fp_sub rm f0 f1
      let mul rm f0 f1  = mk_term3 t Fp_mul rm f0 f1
      let div rm f0 f1  = mk_term3 t Fp_div rm f0 f1
      let fma rm f0 f1 f2  = mk_term t Fp_fma [| rm; f0; f1; f2 |]
      let sqrt rm f = mk_term2 t Fp_sqrt rm f
      let rem f0 f1  = mk_term2 t Fp_rem f0 f1
      let rti rm f  = mk_term2 t Fp_rti rm f
      let min f0 f1  = mk_term2 t Fp_min f0 f1
      let max f0 f1  = mk_term2 t Fp_max f0 f1

      let leq f0 f1  = mk_term2 t Fp_leq f0 f1
      let lt f0 f1  = mk_term2 t Fp_lt f0 f1
      let geq f0 f1  = mk_term2 t Fp_geq f0 f1
      let gt f0 f1  = mk_term2 t Fp_gt f0 f1
      let eq f0 f1  = mk_term2 t Fp_eq f0 f1

      let is_normal f = mk_term1 t Fp_is_normal f
      let is_subnormal f = mk_term1 t Fp_is_subnormal f
      let is_zero f = mk_term1 t Fp_is_zero f
      let is_infinite f = mk_term1 t Fp_is_inf f
      let is_nan f = mk_term1 t Fp_is_nan f
      let is_negative f = mk_term1 t Fp_is_neg f
      let is_positive f = mk_term1 t Fp_is_pos f

      let to_sbv size rm f = mk_term2_indexed1 t Fp_to_sbv rm f size
      let to_ubv size rm f = mk_term2_indexed1 t Fp_to_ubv rm f size

      external to_float : rm term  -> fp value -> float
        = "ocaml_bitwuzla_value_get_fp"
      let assignment rm f = to_float (Rm.term rm) f

    end

    module Ar = struct

      type ('a, 'b) t = ('a, 'b) ar term

      let make s e = mk_const_array t s e

      let select a i = mk_term2 t Array_select a i
      let store a i e = mk_term3 t Array_store a i e

      let assignment a = get_array_value t a

    end

    module Uf = struct

      type ('a, 'b) t = ('a, 'b) fn term

      let lambda s f =
        let vs = List.map (fun s -> mk_var t s "") @@ Sort.of_variadic s in
        let e = f @@ to_variadic vs in
        List.fold_right (fun v e -> mk_term2 t Lambda v e) vs e

      let apply f es =
        mk_term t Apply @@ Array.of_list @@ f :: of_variadic es

      type 'a variadic =
        |  []  : unit variadic
        | (::) : ([< bv | rm | fp ] as 'a) value * 'b variadic ->
            ('a -> 'b) variadic

      external to_variadic : Bitwuzla_c.term list -> 'a variadic = "%identity"

      let assignment f =
        Array.map (fun a ->
            let arity = Array.length a in
            let args = ref List.[] in
            for i = arity - 2 downto 0 do
              args := a.(i) :: !args;
            done;
            to_variadic !args, a.(arity - 1))
        @@ get_fun_value t f

    end

    type 'a view =
      | Value    : 'a value -> 'a view
      | Const    : 'a sort * string -> 'a view
      | Var      :  ([< bv | rm | fp ] as 'a) sort -> 'a view
      | Lambda   : 'a variadic * 'b term -> ('a, 'b) fn view
      | Equal    :
          ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term -> bv view
      | Distinct :
          ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term -> bv view
      | Ite      :
          Bl.t * ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term
          -> 'a view
      | Bv       : ('a, 'b) Bv.operator * 'b -> bv view
      | Fp       : ('a, 'b, 'c) Fp.operator * 'b -> 'c view
      | Select   : ('a, 'b) ar term * 'a term -> 'b view
      | Store    : ('a, 'b) ar term * 'a term * 'b term -> ('a, 'b) ar view
      | Apply    : ('a, 'b) fn term * 'a variadic -> 'b view

    external unsafe_view : 'a view -> 'b view = "%identity"
    let view : type a. a term -> a view =
      fun e ->
      if term_is_value e || term_is_const_array e then
        Value e
      else if term_is_const e then
        Const (term_get_sort e, term_get_symbol e)
      else if term_is_var e then
        unsafe_view @@ Var (term_get_sort e)
      else
        let args = term_get_children e in
        let arity = Array.length args in
        match term_get_kind e with
        | And ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.And, (args.(0), args.(1)))
        | Apply ->
          let f = args.(0) in
          let args = List.tl @@ Array.to_list args in
          unsafe_view @@ Apply (f, to_variadic args)
        | Array_select ->
          assert (arity = 2);
          unsafe_view @@ Select (args.(0), args.(1))
        | Array_store ->
          assert (arity = 3);
          unsafe_view @@ Store (args.(0), args.(1), args.(2))
        | Bv_add ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Add, (args.(0), args.(1)))
        | Bv_and ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.And, (args.(0), args.(1)))
        | Bv_ashr ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Ashr, (args.(0), args.(1)))
        | Bv_comp -> assert false
        | Bv_concat ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Concat, (args.(0), args.(1)))
        | Bv_dec -> assert false
        | Bv_inc -> assert false
        | Bv_mul ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Mul, (args.(0), args.(1)))
        | Bv_nand -> assert false
        | Bv_neg ->
          assert (arity = 1);
          unsafe_view @@ Bv (Bv.Neg, (args.(0)))
        | Bv_nor -> assert false
        | Bv_not ->
          assert (arity = 1);
          unsafe_view @@ Bv (Bv.Not, (args.(0)))
        | Bv_or ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Or, (args.(0), args.(1)))
        | Bv_redand -> assert false
        | Bv_redor -> assert false
        | Bv_redxor -> assert false
        | Bv_rol ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Rol, (args.(0), args.(1)))
        | Bv_ror ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Ror, (args.(0), args.(1)))
        | Bv_sadd_overflow -> assert false
        | Bv_sdiv_overflow -> assert false
        | Bv_sdiv ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Sdiv, (args.(0), args.(1)))
        | Bv_sge ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Sge, (args.(0), args.(1)))
        | Bv_sgt ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Sgt, (args.(0), args.(1)))
        | Bv_shl ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Shl, (args.(0), args.(1)))
        | Bv_shr ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Shr, (args.(0), args.(1)))
        | Bv_sle ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Sle, (args.(0), args.(1)))
        | Bv_slt ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Slt, (args.(0), args.(1)))
        | Bv_smod ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Smod, (args.(0), args.(1)))
        | Bv_smul_overflow -> assert false
        | Bv_srem ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Srem, (args.(0), args.(1)))
        | Bv_ssub_overflow -> assert false
        | Bv_sub ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Sub, (args.(0), args.(1)))
        | Bv_uadd_overflow -> assert false
        | Bv_udiv ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Udiv, (args.(0), args.(1)))
        | Bv_uge ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Uge, (args.(0), args.(1)))
        | Bv_ugt ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Ugt, (args.(0), args.(1)))
        | Bv_ule ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Ule, (args.(0), args.(1)))
        | Bv_ult ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Ult, (args.(0), args.(1)))
        | Bv_umul_overflow -> assert false
        | Bv_urem ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Urem, (args.(0), args.(1)))
        | Bv_usub_overflow -> assert false
        | Bv_xnor -> assert false
        | Bv_xor ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Xor, (args.(0), args.(1)))
        | Distinct ->
          assert (arity = 2);
          unsafe_view @@ Distinct (args.(0), args.(1))
        | Equal ->
          assert (arity = 2);
          unsafe_view @@ Equal (args.(0), args.(1))
        | Exists ->
          assert false
        | Forall ->
          assert false
        | Fp_abs ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Abs, (args.(0)))
        | Fp_add ->
          assert (arity = 3);
          unsafe_view @@ Fp (Fp.Add, (args.(0), args.(1), args.(2)))
        | Fp_div ->
          assert (arity = 3);
          unsafe_view @@ Fp (Fp.Div, (args.(0), args.(1), args.(2)))
        | Fp_eq ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Eq, (args.(0), args.(1)))
        | Fp_fma ->
          assert (arity = 4);
          unsafe_view @@ Fp (Fp.Fma, (args.(0), args.(1), args.(2), args.(3)))
        | Fp_fp ->
          assert (arity = 3);
          unsafe_view @@ Fp (Fp.Fp, { sign=args.(0);
                                      exponent=args.(1);
                                      significand=args.(2) })
        | Fp_geq ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Geq, (args.(0), args.(1)))
        | Fp_gt ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Gt, (args.(0), args.(1)))
        | Fp_is_inf ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_inf, (args.(0)))
        | Fp_is_nan ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_nan, (args.(0)))
        | Fp_is_neg ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_neg, (args.(0)))
        | Fp_is_normal ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_normal, (args.(0)))
        | Fp_is_pos ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_pos, (args.(0)))
        | Fp_is_subnormal ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_subnormal, (args.(0)))
        | Fp_is_zero ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Is_zero, (args.(0)))
        | Fp_leq ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Leq, (args.(0), args.(1)))
        | Fp_lt ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Lt, (args.(0), args.(1)))
        | Fp_max ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Max, (args.(0), args.(1)))
        | Fp_min ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Min, (args.(0), args.(1)))
        | Fp_mul ->
          assert (arity = 3);
          unsafe_view @@ Fp (Fp.Mul, (args.(0), args.(1), args.(2)))
        | Fp_neg ->
          assert (arity = 1);
          unsafe_view @@ Fp (Fp.Neg, (args.(0)))
        | Fp_rem ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Rem, (args.(0), args.(1)))
        | Fp_rti ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Rti, (args.(0), args.(1)))
        | Fp_sqrt ->
          assert (arity = 2);
          unsafe_view @@ Fp (Fp.Sqrt, (args.(0), args.(1)))
        | Fp_sub ->
          assert (arity = 3);
          unsafe_view @@ Fp (Fp.Sub, (args.(0), args.(1), args.(2)))
        | Iff -> assert false
        | Implies -> assert false
        | Ite ->
          assert (arity = 3);
          unsafe_view @@ Ite (args.(0), args.(1), args.(2))
        | Lambda ->
          let e = args.(arity - 1) in
          let vars = Array.to_list @@ Array.sub args 0 (arity - 1) in
          unsafe_view @@ Lambda (to_variadic vars, e)
        | Not ->
          assert (arity = 1);
          unsafe_view @@ Bv (Bv.Not, (args.(0)))
        | Or ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Or, (args.(0), args.(1)))
        | Xor ->
          assert (arity = 2);
          unsafe_view @@ Bv (Bv.Xor, (args.(0), args.(1)))
        | Bv_extract ->
          assert (arity = 1);
          let indices = term_get_indices e in
          assert (Array.length indices = 2);
          unsafe_view @@ Bv (Bv.Extract, (indices.(0), indices.(1), args.(0)))
        | Bv_repeat -> assert false
        | Bv_roli -> assert false
        | Bv_rori -> assert false
        | Bv_sign_extend -> assert false
        | Bv_zero_extend -> assert false
        | Fp_to_fp_from_bv ->
          assert (arity = 1);
          let indices = term_get_indices e in
          assert (Array.length indices = 2);
          unsafe_view @@ Fp (Fp.From_bv,
                             (indices.(0), indices.(0) + indices.(1), args.(0)))
        | Fp_to_fp_from_fp ->
          assert (arity = 2);
          let indices = term_get_indices e in
          assert (Array.length indices = 2);
          unsafe_view @@ Fp (Fp.From_fp,
                             (indices.(0), indices.(0) + indices.(1),
                              args.(0), args.(1)))
        | Fp_to_fp_from_sbv ->
          assert (arity = 2);
          let indices = term_get_indices e in
          assert (Array.length indices = 2);
          unsafe_view @@ Fp (Fp.From_sbv,
                             (indices.(0), indices.(0) + indices.(1),
                              args.(0), args.(1)))
        | Fp_to_fp_from_ubv ->
          assert (arity = 2);
          let indices = term_get_indices e in
          assert (Array.length indices = 2);
          unsafe_view @@ Fp (Fp.From_ubv,
                             (indices.(0), indices.(0) + indices.(1),
                              args.(0), args.(1)))
        | Fp_to_sbv ->
          assert (arity = 2);
          let indices = term_get_indices e in
          assert (Array.length indices = 1);
          unsafe_view @@ Fp (Fp.To_sbv, (indices.(0), args.(0), args.(1)))
        | Fp_to_ubv ->
          assert (arity = 2);
          let indices = term_get_indices e in
          assert (Array.length indices = 1);
          unsafe_view @@ Fp (Fp.To_ubv, (indices.(0), args.(0), args.(1)))

  end

  let assert' ?name b =
    Option.iter (fun name -> term_set_symbol b name) name;
    mk_assert t b

  type nonrec result = result =
    | Sat
    | Unsat
    | Unknown

  let pp_result ppf = function
    | Sat   -> Format.pp_print_string ppf "sat"
    | Unsat -> Format.pp_print_string ppf "unsat"
    | Unknown -> Format.pp_print_string ppf "unknown"

  let check_sat =
    let no_interruption = Fun.id, 0 in
    fun ?interrupt () ->
      begin match interrupt with
        | None -> ignore @@ set_termination_callback t no_interruption
        | Some terminate -> ignore @@ set_termination_callback t terminate
      end;
      check_sat t

  let timeout =
    let check timestamp = Float.compare (Unix.gettimeofday ()) timestamp in
    fun timeout (f : ?interrupt:(('a -> int) * 'a) -> 'b) ->
      let timestamp = Unix.gettimeofday () +. timeout in
      f ~interrupt:(check, timestamp)

  let get_value e = if term_is_value e then e else get_value t e

end

module Once = Session

module Incremental () = struct

  include Session ()
  let () = set_option t Incremental 1

  let push level = push t level
  let pop level = pop t level

  let check_sat_assuming ?interrupt ?names assumptions =
    Option.iter (fun names ->
        Array.iter2 (fun n a -> term_set_symbol a n) names assumptions) names;
    Array.iter (mk_assume t) assumptions;
    check_sat ?interrupt ()

  let get_unsat_assumptions () = get_unsat_assumptions t

end

module Unsat_core () = struct

  include Incremental ()
  let () = set_option t Produce_unsat_cores 1

  let get_unsat_core () = get_unsat_core t

end
