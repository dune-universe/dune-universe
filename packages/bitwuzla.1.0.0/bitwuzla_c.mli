(** This is a straight one to one binding of the Bitwuzla C API.
    @see <https://bitwuzla.github.io/docs/c/api.html> *)

(** {1 Context} *)

(** The Bitwuzla solver. *)
type t

(**
   [create t]
   create a new Bitwuzla instance.

   The returned instance can be deleted earlier via {!val:delete}.

   @return A pointer to the created Bitwuzla instance.
*)
val create : unit -> t

(**
   [delete t]
   delete a Bitwuzla instance.

   The given instance must have been created via {!val:create}.

   @param t The Bitwuzla instance to delete.
*)
val delete : t -> unit

(**
   [reset t]
   reset a Bitwuzla instance.

   This deletes the given instance and creates a new instance in place.
   The given instance must have been created via {!val:create}.

   All sorts and terms associated with the given instance are released
       and thus invalidated.

   @param t The Bitwuzla instance to reset.
*)
val reset : t -> unit

(**
   [copyright t]
   get copyright information.

   @param t The Bitwuzla instance.
*)
val copyright : t -> string

(**
   [version t]
   get version information.

   @param t The Bitwuzla instance.
*)
val version : t -> string

(**
   [terminate t]
   if termination callback function has been configured via
   {!val:set_termination_callback}, call this termination function.

   @param t The Bitwuzla instance.

   @return true if [t] has been terminated.
*)
val terminate : t -> bool

(** A termination callback cookie *)
type 'a cookie

(**
   [set_termination_callback t f a]
   configure a termination callback function.

   The state [a] of the callback can be retrieved via
   {!val:get_termination_callback_state}.

  @param t The Bitwuzla instance.
  @param f The callback function, returns a value > 0 if [t] has
             been terminated.
  @param a The argument to the callback function.

   @return A cookie to retrieve the state [a].
*)
val set_termination_callback : t -> ('a -> int) ->  'a -> 'a cookie

(**
   [get_termination_callback_state c]
   get the state of the termination callback function.

   The returned object representing the state of the callback corresponds to
   the state [a] configured as argument to the callback function via
   {!val:set_termination_callback}.

   @param c The termination callback cookie.

   @return The object passed as argument [a] to the callback function.
*)
val get_termination_callback_state : 'a cookie -> 'a

(** {1:option Option} *)

(** The options supported by Bitwuzla.

    @see <https://bitwuzla.github.io/docs/c/options.html#> for advanced usage.
*)
type opt =
  | Engine
  (** Configure the solver engine.

      Values:
      - ["aigprop"]:
       The propagation-based local search QF_BV engine that operates on the
       bit-blasted formula (the AIG circuit layer).
      - ["fun"] \[{b default}\]:
       The default engine for all combinations of QF_AUFBVFP, uses lemmas on
       demand for QF_AUFBVFP, and eager bit-blasting (optionally with local
       searchin a sequential portfolio) for QF_BV.
      - ["prop"]:
       The propagation-based local search QF_BV engine.
      - ["sls"]:
        The stochastic local search QF_BV engine.
      - ["quant"]:
       The quantifier engine.
  *)
  | Exit_codes
  (** Use non-zero exit codes for sat and unsat results.

      When enabled, use Bitwuzla exit codes
      When disabled, return 0 on success (sat, unsat, unknown), and a non-zero
      exit code otherwise.

      Values:
      - [1]: enable \[{b default}\]
      - [0]: disable
  *)
  | Input_format
  (** Configure input file format.

      If unspecified, Bitwuzla will autodetect the input file format.

      Values:
     - ["none"] \[{b default}\]:
       Auto-detect input file format.
     - ["btor"]:
       BTOR format
     - ["btor2"]:
      BTOR2 format
     - ["smt2"]:
       SMT-LIB v2 format
  *)
  | Incremental
  (** Incremental solving.

      Values:
     - [1]: enable
     - [0]: disable \[{b default}\]

      Enabling this option turns off some optimization techniques.
      Enabling/disabling incremental solving after bitwuzla_check_sat()
      has been called is not supported.
      This option cannot be enabled in combination with option
      [Pp_unconstrained_optimization].
  *)
  | Loglevel
  (** Log level.

      Values:
      - An unsigned integer value ({b default}: [0]).
  *)
  | Output_format
  (** Configure output number format for bit-vector values.

      If unspecified, Bitwuzla will use BTOR format.

      Values:
      - ["aiger"]:
       AIGER ascii format
      - ["aigerbin"]:
       AIGER binary format
      - ["btor"] \[{b default}\]:
       BTOR format
      - ["smt2"]:
       SMT-LIB v2 format
  *)
  | Output_number_format
  (** Configure output number format for bit-vector values.

      If unspecified, Bitwuzla will use binary representation.

      Values:
      - ["bin"] \[{b default}\]:
      Binary number format.
      - ["hex"]:
      Hexadecimal number format.
      - ["dec"]:
      Decimal number format.
  *)
  | Pretty_print
  (** Pretty printing.

      Values:
      - [1]: enable \[{b default}\]
      - [0]: disable
  *)
  | Print_dimacs
  (** Print DIMACS.

      Print the CNF sent to the SAT solver in DIMACS format to stdout.

      Values:
      - [1]: enable
      - [0]: disable \[{b default}\]
  *)
  | Produce_models
  (** Model generation.

      Values:
      - [1]: enable, generate model for assertions only
      - [2]: enable, generate model for all created terms
      - [0]: disable \[{b default}\]

      This option cannot be enabled in combination with option
          [Pp_unconstrained_optimization].
  *)
  | Produce_unsat_cores
  (** Unsat core generation.

      Values:
      - [1]: enable
      - [0]: disable \[{b default}\]
  *)
  | Seed
  (** Seed for random number generator.

      Values:
      - An unsigned integer value ({b default}: [0]).
  *)
  | Verbosity
  (** Verbosity level.

      Values:
      - An unsigned integer value <= 4 ({b default}: [0]).
   *)
  | Pp_ackermann
  | Pp_beta_reduce
  | Pp_eliminate_extracts
  | Pp_eliminate_ites
  | Pp_extract_lambdas
  | Pp_merge_lambdas
  | Pp_nondestr_subst
  | Pp_normalize_add
  | Pp_skeleton_preproc
  | Pp_unconstrained_optimization
  | Pp_var_subst
  | Rw_extract_arith
  | Rw_level
  | Rw_normalize
  | Rw_normalize_add
  | Rw_simplify_constraints
  | Rw_slt
  | Rw_sort_aig
  | Rw_sort_aigvec
  | Rw_sort_exp
  | Fun_dual_prop
  | Fun_dual_prop_qsort
  | Fun_eager_lemmas
  | Fun_lazy_synthesize
  | Fun_just
  | Fun_just_heuristic
  | Fun_preprop
  | Fun_presls
  | Fun_store_lambdas
  | Sls_just
  | Sls_move_gw
  | Sls_move_inc_move_test
  | Sls_move_prop
  | Sls_move_prop_force_rw
  | Sls_move_prop_nprops
  | Sls_move_prop_nslss
  | Sls_move_rand_all
  | Sls_move_rand_range
  | Sls_move_rand_walk
  | Sls_move_range
  | Sls_move_segment
  | Sls_prob_move_rand_walk
  | Sls_nflips
  | Sls_strategy
  | Sls_use_restarts
  | Sls_use_bandit
  | Prop_ashr
  | Prop_const_bits
  | Prop_const_domains
  | Prop_entailed
  | Prop_flip_cond_const_delta
  | Prop_flip_cond_const_npathsel
  | Prop_infer_ineq_bounds
  | Prop_no_move_on_conflict
  | Prop_nprops
  | Prop_nupdates
  | Prop_path_sel
  | Prop_prob_fallback_random_value
  | Prop_prob_and_flip
  | Prop_prob_eq_flip
  | Prop_prob_flip_cond
  | Prop_prob_flip_cond_const
  | Prop_prob_random_input
  | Prop_prob_slice_flip
  | Prop_prob_slice_keep_dc
  | Prop_prob_use_inv_value
  | Prop_use_bandit
  | Prop_use_inv_lt_concat
  | Prop_use_restarts
  | Prop_sext
  | Prop_skip_no_progress
  | Prop_xor
  | Aigprop_nprops
  | Aigprop_use_bandit
  | Aigprop_use_restarts
  | Quant_cer
  | Quant_der
  | Quant_dual_solver
  | Quant_miniscope
  | Quant_synth
  | Quant_fixsynth
  | Quant_synth_ite_complete
  | Quant_synth_limit
  | Quant_synth_qi
  | Check_model
  | Check_unconstrained
  | Check_unsat_assumptions
  | Declsort_bv_witdh
  | Ls_share_sat
  | Parse_interactive
  | Sat_engine_cadical_freeze

(**
   [set_option t option value]
   set option.

   @param t The Bitwuzla instance.
   @param option The option.
   @param value The option value.
*)
val set_option : t -> opt -> int -> unit

(**
   [set_option_str t option value]
   set option value for string options.

   @param t The Bitwuzla instance.
   @param option The option.
   @param value The option string value.
*)
val set_option_str : t -> opt -> string -> unit

(**
   [get_option t option]
   get the current value of an option.

   @param t The Bitwuzla instance.
   @param option The option.

   @return The option value.
*)
val get_option : t -> opt -> int

(**
   [get_option_str t option]
   get the current value of an option as a string if option can be configured
   via a string value.

   @param t The Bitwuzla instance.
   @param option The option.

   @return The option value.
*)
val get_option_str : t -> opt -> string

(** {1:sort Sort} *)

(** A Bitwuzla sort. *)
type sort [@@immediate]

(** {2:sort_constructor Constructor} *)

(**
   [mk_array_sort t index element]
   create an array sort.

   @param t The Bitwuzla instance.
   @param index The index sort of the array sort.
   @param element The element sort of the array sort.

   @return An array sort which maps sort [index] to sort [element].
*)
val mk_array_sort : t -> sort -> sort -> sort

(**
   [mk_bool_sort t]
   create a Boolean sort.

   A Boolean sort is a bit-vector sort of size 1.

   @param t The Bitwuzla instance.

   @return A Boolean sort.
*)
val mk_bool_sort : t -> sort

(**
   [mk_bv_sort t size]
   create a bit-vector sort of given size.

   @param t The Bitwuzla instance.
   @param size The size of the bit-vector sort.

   @return A bit-vector sort of given size.
*)
val mk_bv_sort : t -> int -> sort

(**
   [mk_fp_sort t exp_size sig_size]
   create a floating-point sort of given exponent and significand size.

   @param t The Bitwuzla instance.
   @param exp_size The size of the exponent.
   @param sig_size The size of the significand (including sign bit).

   @return A floating-point sort of given format.
*)
val mk_fp_sort : t -> int -> int -> sort

(**
   [mk_fun_sort t domain codomain]
   create a function sort.

   @param t The Bitwuzla instance.
   @param domain The domain sorts (the sorts of the arguments).
   @param codomain The codomain sort (the sort of the return value).

   @return A function sort of given domain and codomain sorts.
 *)
val mk_fun_sort : t -> sort array -> sort -> sort

(**
   [mk_rm_sort t]
   create a Roundingmode sort.

   @param t The Bitwuzla instance.

   @return A Roundingmode sort.
*)
val mk_rm_sort : t -> sort

(** {2:sort_util Util} *)

(**
   [sort_dump sort format pp]
   print sort.

   @param sort The sort.
   @param format The output format for printing the term. Either [`Btor] for
                the BTOR format, or [`Smt2] for the SMT-LIB v2 format. Note
                for the [`Btor] this function won't do anything since BTOR
                sorts are printed when printing the term via {!val:term_dump}.
   @param pp The outpout formatter.
*)
val sort_dump : sort -> [ `Btor | `Smt2 ] -> Format.formatter -> unit


(** {2:sort_query Query} *)

(**
   [sort_hash sort]
   compute the hash value for a sort.

   @param sort The sort.

   @return The hash value of the sort.
*)
val sort_hash : sort -> int

(**
   [bv_get_size sort]
   get the size of a bit-vector sort.

   Requires that given sort is a bit-vector sort.

   @param sort The sort.

   @return The size of the bit-vector sort.
*)
val sort_bv_get_size : sort -> int

(**
   [sort_fp_get_exp_size sort]
   get the exponent size of a floating-point sort.

   Requires that given sort is a floating-point sort.

   @param sort The sort.

   @return The exponent size of the floating-point sort.
*)
val sort_fp_get_exp_size : sort -> int

(**
   [sort_fp_get_sig_size sort]
   get the significand size of a floating-point sort.

   Requires that given sort is a floating-point sort.

   @param sort The sort.

   @return The significand size of the floating-point sort.
*)
val sort_fp_get_sig_size : sort -> int

(**
   [sort_array_get_index sort]
   get the index sort of an array sort.

   Requires that given sort is an array sort.

   @param sort The sort.

   @return The index sort of the array sort.
*)
val sort_array_get_index : sort -> sort

(**
   [sort_array_get_element sort]
   get the element sort of an array sort.

   Requires that given sort is an array sort.

   @param sort The sort.

   @return The element sort of the array sort.
*)
val sort_array_get_element : sort -> sort

(**
   [sort_fun_get_domain_sorts sort]
   get the domain sorts of a function sort.

   Requires that given sort is a function sort.

   @param sort The sort.

   @return The domain sorts of the function sort as an array of sort.
*)
val sort_fun_get_domain_sorts : sort -> sort array

(**
   [sort_fun_get_codomain sort]
   get the codomain sort of a function sort.

   Requires that given sort is a function sort.

   @param sort The sort.

   @return The codomain sort of the function sort.
*)
val sort_fun_get_codomain : sort -> sort

(**
   [sort_fun_get_arity sort]
   get the arity of a function sort.

   @param sort The sort.

   @return The number of arguments of the function sort.
*)
val sort_fun_get_arity : sort -> int


(**
   [sort_is_equal sort0 sort1]
   determine if two sorts are equal.

   @param sort0 The first sort.
   @param sort1 The second sort.

   @return [true] if the given sorts are equal.
*)
val sort_is_equal : sort -> sort -> bool

(**
   [sort_is_array sort]
   determine if a sort is an array sort.

   @param sort The sort.

   @return [true] if [sort] is an array sort.
*)
val sort_is_array : sort -> bool

(**
   [sort_is_bv sort]
   determine if a sort is a bit-vector sort.

   @param sort The sort.

   @return [true] if [sort] is a bit-vector sort.
*)
val sort_is_bv : sort -> bool

(**
   [sort_is_fp sort]
   determine if a sort is a floating-point sort.

   @param sort The sort.

   @return [true] if [sort] is a floating-point sort.
*)
val sort_is_fp : sort -> bool

(**
   [sort_is_fun sort]
   determine if a sort is a function sort.

   @param sort The sort.

   @return [true] if [sort] is a function sort.
*)
val sort_is_fun : sort -> bool

(**
   [sort_is_rm sort]
   determine if a sort is a Roundingmode sort.

   @param sort The sort.

   @return [true] if [sort] is a Roundingmode sort.
*)
val sort_is_rm : sort -> bool

(** {1:term Term} *)

(** A Bitwuzla term. *)
type term [@@immediate]

(** The base for strings representing bit-vector values. *)
type bvbase =
  | Bin (** binary *)
  | Dec (** decimal *)
  | Hex (** hexadecimal *)

(**
   Rounding mode for floating-point operations.

  For some floating-point operations, infinitely precise results may not be
  representable in a given format. Hence, they are rounded modulo one of five
  rounding modes to a representable floating-point number.

  The following rounding modes follow the SMT-LIB theory for floating-point
  arithmetic, which in turn is based on IEEE Standard 754.
  The rounding modes are specified in Sections 4.3.1 and 4.3.2 of the IEEE
  Standard 754.
*)
type roundingmode =
  | Rne
  (** Round to the nearest even number.
      If the two nearest floating-point numbers bracketing an unrepresentable
      infinitely precise result are equally near, the one with an even least
      significant digit will be delivered.

      SMT-LIB: [RNE] roundNearestTiesToEven
  *)
  | Rna
  (** Round to the nearest number away from zero.
      If the two nearest floating-point numbers bracketing an unrepresentable
      infinitely precise result are equally near, the one with larger magnitude
      will be selected.

      SMT-LIB: [RNA] roundNearestTiesToAway
  *)
  | Rtn
  (** Round towards negative infinity (-oo).
      The result shall be the format’s floating-point number (possibly -oo)
      closest to and no less than the infinitely precise result.

      SMT-LIB: [RTN] roundTowardNegative
  *)
  | Rtp
  (** Round towards positive infinity (+oo).
      The result shall be the format’s floating-point number (possibly +oo)
      closest to and no less than the infinitely precise result.

      SMT-LIB: [RTP] roundTowardPositive
  *)
  | Rtz
  (** Round towards zero.
      The result shall be the format’s floating-point number closest to and no
      greater in magnitude than the infinitely precise result.

     SMT-LIB: [RTZ] roundTowardZero
  *)

(** The term kind. *)
type kind =
  | And
  (** Boolean and.

      SMT-LIB: [and] *)
  | Apply
  (** Function application. *)
  | Array_select
  (** Array select.

      SMT-LIB: [select] *)
  | Array_store
  (** Array store.

      SMT-LIB: [store] *)
  | Bv_add
  (** Bit-vector addition.

      SMT-LIB: [bvadd] *)
  | Bv_and
  (** Bit-vector and.

      SMT-LIB: [bvand] *)
  | Bv_ashr
  (** Bit-vector arithmetic right shift.

      SMT-LIB: [bvashr] *)
  | Bv_comp
  (** Bit-vector comparison.

      SMT-LIB: [bvcomp] *)
  | Bv_concat
  (** Bit-vector concat.

      SMT-LIB: [concat] *)
  | Bv_dec
  (** Bit-vector decrement.

      Decrement by one. *)
  | Bv_inc
  (** Bit-vector increment.

      Increment by one. *)
  | Bv_mul
  (** Bit-vector multiplication.

      SMT-LIB: [bvmul] *)
  | Bv_nand
  (** Bit-vector nand.

      SMT-LIB: [bvnand] *)
  | Bv_neg
  (** Bit-vector negation (two's complement).

      SMT-LIB: [bvneg] *)
  | Bv_nor
  (** Bit-vector nor.

      SMT-LIB: [bvnor] *)
  | Bv_not
  (** Bit-vector not (one's complement).

      SMT-LIB: [bvnot] *)
  | Bv_or
  (** Bit-vector or.

      SMT-LIB: [bvor] *)
  | Bv_redand
  (** Bit-vector and reduction.

      Bit-wise {b and} reduction, all bits are {b and}'ed together into a single
      bit.
      This corresponds to bit-wise {b and} reduction as known from Verilog. *)
  | Bv_redor
  (** Bit-vector reduce or.

      Bit-wise {b or} reduction, all bits are {b or}'ed together into a single
      bit.
      This corresponds to bit-wise {b or} reduction as known from Verilog. *)
  | Bv_redxor
  (** Bit-vector reduce xor.

      Bit-wise {b xor} reduction, all bits are {b xor}'ed together into a single
      bit.
      This corresponds to bit-wise {b xor} reduction as known from Verilog. *)
  | Bv_rol
  (** Bit-vector rotate left (not indexed).

      This is a non-indexed variant of SMT-LIB [rotate_left]. *)
  | Bv_ror
  (** Bit-vector rotate right.

      This is a non-indexed variant of SMT-LIB [rotate_right]. *)
  | Bv_sadd_overflow
  (** Bit-vector signed addition overflow test.

      Single bit to indicate if signed addition produces an overflow. *)
  | Bv_sdiv_overflow
  (** Bit-vector signed division overflow test.

      Single bit to indicate if signed division produces an overflow. *)
  | Bv_sdiv
  (** Bit-vector signed division.

      SMT-LIB: [bvsdiv] *)
  | Bv_sge
  (** Bit-vector signed greater than or equal.

      SMT-LIB: [bvsge] *)
  | Bv_sgt
  (** Bit-vector signed greater than.

      SMT-LIB: [bvsgt] *)
  | Bv_shl
  (** Bit-vector logical left shift.

      SMT-LIB: [bvshl] *)
  | Bv_shr
  (** Bit-vector logical right shift.

      SMT-LIB: [bvshr] *)
  | Bv_sle
  (** Bit-vector signed less than or equal.

      SMT-LIB: [bvsle] *)
  | Bv_slt
  (** Bit-vector signed less than.

      SMT-LIB: [bvslt] *)
  | Bv_smod
  (** Bit-vector signed modulo.

      SMT-LIB: [bvsmod] *)
  | Bv_smul_overflow
  (** Bit-vector signed multiplication overflow test.

      SMT-LIB: [bvsmod] *)
  | Bv_srem
  (** Bit-vector signed remainder.

      SMT-LIB: [bvsrem] *)
  | Bv_ssub_overflow
  (** Bit-vector signed subtraction overflow test.

      Single bit to indicate if signed subtraction produces an overflow. *)
  | Bv_sub
  (** Bit-vector subtraction.

      SMT-LIB: [bvsub] *)
  | Bv_uadd_overflow
  (** Bit-vector unsigned addition overflow test.

      Single bit to indicate if unsigned addition produces an overflow. *)
  | Bv_udiv
  (** Bit-vector unsigned division.

      SMT-LIB: [bvudiv] *)
  | Bv_uge
  (** Bit-vector unsigned greater than or equal.

      SMT-LIB: [bvuge] *)
  | Bv_ugt
  (** Bit-vector unsigned greater than.

      SMT-LIB: [bvugt] *)
  | Bv_ule
  (** Bit-vector unsigned less than or equal.

      SMT-LIB: [bvule] *)
  | Bv_ult
  (** Bit-vector unsigned less than.

      SMT-LIB: [bvult] *)
  | Bv_umul_overflow
  (** Bit-vector unsigned multiplication overflow test.

      Single bit to indicate if unsigned multiplication produces an overflow. *)
  | Bv_urem
  (** Bit-vector unsigned remainder.

      SMT-LIB: [bvurem] *)
  | Bv_usub_overflow
  (** Bit-vector unsigned subtraction overflow test.

      Single bit to indicate if unsigned subtraction produces an overflow. *)
  | Bv_xnor
  (** Bit-vector xnor.

      SMT-LIB: [bvxnor] *)
  | Bv_xor
  (** Bit-vector xor.

      SMT-LIB: [bvxor] *)
  | Distinct
  (** Disequality.

      SMT-LIB: [distinct] *)
  | Equal
  (** Equality.

      SMT-LIB: [=] *)
  | Exists
  (** Existential quantification.

      SMT-LIB: [exists] *)
  | Forall
  (** Universal quantification.

      SMT-LIB: [forall] *)
  | Fp_abs
  (** Floating-point absolute value.

      SMT-LIB: [fp.abs] *)
  | Fp_add
  (** Floating-point addition.

      SMT-LIB: [fp.add] *)
  | Fp_div
  (** Floating-point division.

      SMT-LIB: [fp.div] *)
  | Fp_eq
  (** Floating-point equality.

      SMT-LIB: [fp.eq] *)
  | Fp_fma
  (** Floating-point fused multiplcation and addition.

      SMT-LIB: [fp.fma] *)
  | Fp_fp
  (** Floating-point IEEE 754 value.

      SMT-LIB: [fp] *)
  | Fp_geq
  (** Floating-point greater than or equal.

      SMT-LIB: [fp.geq] *)
  | Fp_gt
  (** Floating-point greater than.

      SMT-LIB: [fp.gt] *)
  | Fp_is_inf
  (** Floating-point is infinity tester.

      SMT-LIB: [fp.isInfinite] *)
  | Fp_is_nan
  (** Floating-point is Nan tester.

      SMT-LIB: [fp.isNaN] *)
  | Fp_is_neg
  (** Floating-point is negative tester.

      SMT-LIB: [fp.isNegative] *)
  | Fp_is_normal
  (** Floating-point is normal tester.

      SMT-LIB: [fp.isNormal] *)
  | Fp_is_pos
  (** Floating-point is positive tester.

      SMT-LIB: [fp.isPositive] *)
  | Fp_is_subnormal
  (** Floating-point is subnormal tester.

      SMT-LIB: [fp.isSubnormal] *)
  | Fp_is_zero
  (** Floating-point is zero tester.

      SMT-LIB: [fp.isZero] *)
  | Fp_leq
  (** Floating-point less than or equal.

      SMT-LIB: [fp.leq] *)
  | Fp_lt
  (** Floating-point less than.

      SMT-LIB: [fp.lt] *)
  | Fp_max
  (** Floating-point max.

      SMT-LIB: [fp.max] *)
  | Fp_min
  (** Floating-point min.

      SMT-LIB: [fp.min] *)
  | Fp_mul
  (** Floating-point multiplcation.

      SMT-LIB: [fp.mul] *)
  | Fp_neg
  (** Floating-point negation.

      SMT-LIB: [fp.neg] *)
  | Fp_rem
  (** Floating-point remainder.

      SMT-LIB: [fp.rem] *)
  | Fp_rti
  (** Floating-point round to integral.

      SMT-LIB: [fp.roundToIntegral] *)
  | Fp_sqrt
  (** Floating-point round to square root.

      SMT-LIB: [fp.sqrt] *)
  | Fp_sub
  (** Floating-point round to subtraction.

      SMT-LIB: [fp.sqrt] *)
  | Iff
  (** Boolean if and only if.

      SMT-LIB: [=] *)
  | Implies
  (** Boolean implies.

      SMT-LIB: [=>] *)
  | Ite
  (** If-then-else.

      SMT-LIB: [ite] *)
  | Lambda
  (** Lambda. *)
  | Not
  (** Boolean not.

      SMT-LIB: [not] *)
  | Or
  (** Boolean or.

      SMT-LIB: [or] *)
  | Xor
  (** Boolean xor.

      SMT-LIB: [xor] *)
  | Bv_extract
  (** Bit-vector extract.

      SMT-LIB: [extract] (indexed) *)
  | Bv_repeat
  (** Bit-vector repeat.

      SMT-LIB: [repeat] (indexed) *)
  | Bv_roli
  (** Bit-vector rotate left by integer.

      SMT-LIB: [rotate_left] (indexed) *)
  | Bv_rori
  (** Bit-vector rotate right by integer.

      SMT-LIB: [rotate_right] (indexed) *)
  | Bv_sign_extend
  (** Bit-vector sign extend.

      SMT-LIB: [sign_extend] (indexed) *)
  | Bv_zero_extend
  (** Bit-vector zero extend.

      SMT-LIB: [zero_extend] (indexed) *)
  | Fp_to_fp_from_bv
  (** Floating-point to_fp from IEEE 754 bit-vector.

      SMT-LIB: [to_fp] (indexed) *)
  | Fp_to_fp_from_fp
  (** Floating-point to_fp from floating-point.

      SMT-LIB: [to_fp] (indexed) *)
  | Fp_to_fp_from_sbv
  (** Floating-point to_fp from signed bit-vector value.

      SMT-LIB: [to_fp] (indexed) *)
  | Fp_to_fp_from_ubv
  (** Floating-point to_fp from unsigned bit-vector value.

      SMT-LIB: [to_fp_unsigned] (indexed) *)
  | Fp_to_sbv
  (** Floating-point to_sbv.

      SMT-LIB: [fp.to_sbv] (indexed) *)
  | Fp_to_ubv
  (** Floating-point to_ubv.

      SMT-LIB: [fp.to_ubv] (indexed) *)

(** {2:term_constructor Constructor} *)

(** {3 Value} *)

(**
   [mk_true t]
   create a true value.

   This creates a bit-vector value 1 of size 1.

   @param t The Bitwuzla instance.

   @return A term representing the bit-vector value 1 of size 1.
*)
val mk_true : t -> term

(**
   [mk_false t]
   create a false value.

   This creates a bit-vector value 0 of size 1.

   @param t The Bitwuzla instance.

   @return A term representing the bit-vector value 0 of size 1.
*)
val mk_false : t -> term

(**
   [mk_bv_zero t sort]
   create a bit-vector value zero.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the bit-vector value 0 of given sort.
*)
val mk_bv_zero : t -> sort -> term

(**
   [mk_bv_one t sort]
   create a bit-vector value one.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the bit-vector value 1 of given sort.
*)
val mk_bv_one : t -> sort -> term

(**
   [mk_bv_ones t sort]
   create a bit-vector value where all bits are set to 1.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the bit-vector value of given sort
         where all bits are set to 1.
*)
val mk_bv_ones : t -> sort -> term

(**
   [mk_bv_min_signed t sort]
   create a bit-vector minimum signed value.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the bit-vector value of given sort where the MSB
         is set to 1 and all remaining bits are set to 0.
*)
val mk_bv_min_signed : t -> sort -> term

(**
   [mk_bv_max_signed t sort]
   create a bit-vector maximum signed value.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the bit-vector value of given sort where the MSB
         is set to 0 and all remaining bits are set to 1.
*)
val mk_bv_max_signed : t -> sort -> term

(**
   [mk_fp_pos_zero t sort]
   create a floating-point positive zero value (SMT-LIB: [+zero]).

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the floating-point positive zero value of given
         floating-point sort.
*)
val mk_fp_pos_zero : t -> sort -> term

(**
   [mk_fp_neg_zero t sort]
   create a floating-point negative zero value (SMT-LIB: [-zero]).

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the floating-point negative zero value of given
         floating-point sort.
*)
val mk_fp_neg_zero : t -> sort -> term

(**
   [mk_fp_pos_inf t sort]
   create a floating-point positive infinity value (SMT-LIB: [+oo]).

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the floating-point positive infinity value of
         given floating-point sort.
*)
val mk_fp_pos_inf : t -> sort -> term

(**
   [mk_fp_neg_inf t sort]
   create a floating-point negative infinity value (SMT-LIB: [-oo]).

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the floating-point negative infinity value of
         given floating-point sort.
*)
val mk_fp_neg_inf : t -> sort -> term

(**
   [mk_fp_nan t sort]
   create a floating-point NaN value.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.

   @return A term representing the floating-point NaN value of given
         floating-point sort.
*)
val mk_fp_nan : t -> sort -> term


(**
   [mk_bv_value t sort value base]
   create a bit-vector value from its string representation.

   Parameter [base] determines the base of the string representation.

   Given value must fit into a bit-vector of given size (sort).

   @param t The Bitwuzla instance.
   @param sort The sort of the value.
   @param value A string representing the value.
   @param base The base in which the string is given.

   @return A term representing the bit-vector value of given sort.
*)
val mk_bv_value : t -> sort -> string -> bvbase -> term

(**
   [mk_bv_value_int t sort value]
   create a bit-vector value from its unsigned integer representation.

   If given value does not fit into a bit-vector of given size (sort),
       the value is truncated to fit.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.
   @param value The unsigned integer representation of the bit-vector value.

   @return A term representing the bit-vector value of given sort.
*)
val mk_bv_value_int : t -> sort -> int -> term

(**
   [mk_fp_value t bv_sign bv_exponent bv_significand]
   create a floating-point value from its IEEE 754 standard representation
   given as three bitvectors representing the sign bit, the exponent and the
   significand.

   @param t The Bitwuzla instance.
   @param bv_sign The sign bit.
   @param bv_exponent The exponent bit-vector.
   @param bv_significand The significand bit-vector.

   @return A term representing the floating-point value.
*)
val mk_fp_value : t -> term -> term -> term -> term

(**
   [mk_fp_value_from_real t sort rm real]
   create a floating-point value from its real representation, given as a
   decimal string, with respect to given rounding mode.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.
   @param rm The rounding mode.
   @param real The decimal string representing a real value.

   @return A term representing the floating-point value of given sort.
*)
val mk_fp_value_from_real : t -> sort -> term -> string -> term

(**
   [mk_fp_value_from_rational t sort rm num den]
   create a floating-point value from its rational representation, given as a
   two decimal strings representing the numerator and denominator, with respect
   to given rounding mode.

   @param t The Bitwuzla instance.
   @param sort The sort of the value.
   @param rm The rounding mode.
   @param num The decimal string representing the numerator.
   @param den The decimal string representing the denominator.

   @return A term representing the floating-point value of given sort.
*)
val mk_fp_value_from_rational : t -> sort -> term -> string -> string -> term

(**
   [mk_rm_value t rm]
   create a rounding mode value.

   @param t The Bitwuzla instance.
   @param rm The rounding mode value.

   @return A term representing the rounding mode value.
*)
val mk_rm_value : t -> roundingmode -> term

(** {3 Expression} *)

(**
   [mk_term1 t kind arg]
   create a term of given kind with one argument term.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg The argument to the operator.

   @return A term representing an operation of given kind.
*)
val mk_term1 : t -> kind -> term -> term

(**
   [mk_term2 t kind arg0 arg1]
   create a term of given kind with two argument terms.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg0 The first argument to the operator.
   @param arg1 The second argument to the operator.

   @return A term representing an operation of given kind.
*)
val mk_term2 : t -> kind -> term -> term -> term

(**
   [mk_term3 t kind arg0 arg1 arg2]
   create a term of given kind with three argument terms.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg0 The first argument to the operator.
   @param arg1 The second argument to the operator.
   @param arg2 The third argument to the operator.

   @return A term representing an operation of given kind.
*)
val mk_term3 : t -> kind -> term -> term -> term -> term

(**
   [mk_term t kind args]
   create a term of given kind with the given argument terms.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param args The argument terms.

   @return A term representing an operation of given kind.
*)
val mk_term : t -> kind -> term array -> term

(**
   [mk_term1_indexed1 t kind arg idx]
   create an indexed term of given kind with one argument term and one index.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg The argument term.
   @param idx The index.

   @return A term representing an indexed operation of given kind.
*)
val mk_term1_indexed1 : t -> kind -> term -> int -> term

(**
   [mk_term1_indexed2 t kind arg idx0 idx1]
   create an indexed term of given kind with one argument term and two indices.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg The argument term.
   @param idx0 The first index.
   @param idx1 The second index.

   @return A term representing an indexed operation of given kind.
*)
val mk_term1_indexed2 : t -> kind -> term -> int -> int -> term


(**
   [mk_term2_indexed1 t kind arg0 arg1 idx]
   create an indexed term of given kind with two argument terms and one index.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg0 The first argument term.
   @param arg1 The second argument term.
   @param idx The index.

   @return A term representing an indexed operation of given kind.
*)
val mk_term2_indexed1 : t -> kind -> term -> term -> int -> term

(**
   [mk_term2_indexed2 t kind arg0 arg1 idx0 idx1]
   create an indexed term of given kind with two argument terms and two indices.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param arg0 The first argument term.
   @param arg1 The second argument term.
   @param idx0 The first index.
   @param idx1 The second index.

   @return A term representing an indexed operation of given kind.
*)
val mk_term2_indexed2 : t -> kind -> term -> term -> int -> int -> term

(**
   [mk_term_indexed t kind args idxs]
   create an indexed term of given kind with the given argument terms and
   indices.

   @param t The Bitwuzla instance.
   @param kind The operator kind.
   @param args The argument terms.
   @param idxs The indices.

   @return A term representing an indexed operation of given kind.
*)
val mk_term_indexed : t -> kind -> term array -> int array -> term

(**
   [mk_const t sort symbol]
   create a (first-order) constant of given sort with given symbol.

   This creates a 0-arity function symbol.

   @param t The Bitwuzla instance.
   @param sort The sort of the constant.
   @param symbol The symbol of the constant.

   @return A term representing the constant.
*)
val mk_const : t -> sort -> string -> term

(**
   [mk_const_array t sort value]
   create a one-dimensional constant array of given sort, initialized with
   given value.

   @param t The Bitwuzla instance.
   @param sort The sort of the array.
   @param value The value to initialize the elements of the array with.

   @return A term representing a constant array of given sort.
*)
val mk_const_array : t -> sort -> term -> term

(**
   [mk_var t sort symbol]
   create a variable of given sort with given symbol.

   This creates a variable to be bound by quantifiers or lambdas.

   @param t The Bitwuzla instance.
   @param sort The sort of the variable.
   @param symbol The symbol of the variable.

   @return A term representing the variable.
*)
val mk_var : t -> sort -> string -> term

(** {2 Util} *)

(**
   [substitute t term map]
   substitute a set of keys with their corresponding values in the given term.

   @param t The Bitwuzla instance.
   @param term The term in which the keys are to be substituted.
   @param map The key/value associations.

   @return The resulting term from this substitution.
*)
val substitute_term : t -> term -> (term * term) array -> term

(**
   [substitute_terms t terms map]
   substitute a set of keys with their corresponding values in the set of given
   terms.

   The terms in [terms] are replaced with the terms resulting from this
   substitutions.

   @param t The Bitwuzla instance.
   @param terms The terms in which the keys are to be substituted.
   @param map The key/value associations.
*)
val substitute_terms : t -> term array -> (term * term) array -> unit

(**
   [term_dump term format pp]
   print term.

   @param term The term.
   @param format The output format for printing the term. Either [`Btor] for the
                BTOR format, or [`Smt2] for the SMT-LIB v2 format.
   @param pp The outpout formatter.
*)
val term_dump : term -> [ `Btor | `Smt2 ] -> Format.formatter -> unit


(** {2:term_query Query} *)

(**
   [term_hash term]
   compute the hash value for a term.

   @param term The term.

   @return The hash value of the term.
*)
val term_hash : term -> int

(**
   [term_get_kind term]
   get the kind of a term.

   @param term The term.

   @return The kind of the given term.
*)
val term_get_kind : term -> kind

(**
   [term_get_children term]
   get the child terms of a term.

   @param term The term.

   @return The children of [term] as an array of terms.
*)
val term_get_children : term -> term array

(**
   [term_get_indices term]
   get the indices of an indexed term.

   Requires that given term is an indexed term.

   @param term The term.

   @return The children of [term] as an array of terms.
*)
val term_get_indices : term -> int array

(**
   [term_is_indexed term]
   determine if a term is an indexed term.

   @param term The term.

   @return [true] if [term] is an indexed term.
*)
val term_is_indexed : term -> bool

(**
   [term_get_sort term]
   get the sort of a term.

   @param term The term.

   @return The sort of the term.
*)
val term_get_sort :  term -> sort

(**
   [term_array_get_index_sort term]
   get the index sort of an array term.

   Requires that given term is an array or an array store term.

   @param term The term.

   @return The index sort of the array term.
*)
val term_array_get_index_sort : term -> sort

(**
   [term_array_get_element_sort term]
   get the element sort of an array term.

   Requires that given term is an array or an array store term.

   @param term The term.

   @return The element sort of the array term.
*)
val term_array_get_element_sort : term -> sort

(**
   [term_fun_get_domain_sorts term]
   get the domain sorts of a function term.

   Requires that given term is an uninterpreted function, a lambda term, an
   array store term, or an ite term over function terms.

   @param term The term.

   @return The domain sorts of the function term.
*)
val term_fun_get_domain_sorts : term -> sort array

(**
   [term_fun_get_codomain_sort term]
   get the codomain sort of a function term.

   Requires that given term is an uninterpreted function, a lambda term, an
   array store term, or an ite term over function terms.

   @param term The term.

   @return The codomain sort of the function term.
*)
val term_fun_get_codomain_sort : term -> sort

(**
   [term_bv_get_size term]
   get the bit-width of a bit-vector term.

   Requires that given term is a bit-vector term.

   @param term The term.

   @return The bit-width of the bit-vector term.
*)
val term_bv_get_size : term -> int

(**
   [term_fp_get_exp_size term]
   get the bit-width of the exponent of a floating-point term.

   Requires that given term is a floating-point term.

   @param term The term.

   @return The bit-width of the exponent of the floating-point term.
*)
val term_fp_get_exp_size : term -> int

(**
   [term_fp_get_sig_size term]
   get the bit-width of the significand of a floating-point term.

   Requires that given term is a floating-point term.

   @param term The term.

   @return The bit-width of the significand of the floating-point term.
*)
val term_fp_get_sig_size : term -> int

(**
   [term_fp_get_arity term]
   get the arity of a function term.

   Requires that given term is a function term.

   @param term The term.

   @return The arity of the function term.
*)
val term_fun_get_arity : term -> int

(**
   [term_get_symbol term]
   get the symbol of a term.

   @param term The term.

   @return The symbol of [term].

   @raise Not_found if no symbol is defined.
*)
val term_get_symbol : term -> string

(**
   [term_set_symbol term symbol]
   set the symbol of a term.

   @param term The term.
   @param symbol The symbol.
*)
val term_set_symbol : term -> string -> unit

(**
   [term_is_equal_sort term0 term1]
   determine if the sorts of two terms are equal.

   @param term0 The first term.
   @param term1 The second term.

   @return [true] if the sorts of [term0] and [term1] are equal.
*)
val term_is_equal_sort : term -> term -> bool

(**
   [term_is_array term]
   determine if a term is an array term.

   @param term The term.

   @return [true] if [term] is an array term.
*)
val term_is_array : term -> bool

(**
   [term_is_const term]
   determine if a term is a constant.

   @param term The term.

   @return [true] if [term] is a constant.
*)
val term_is_const : term -> bool

(**
   [term_is_fun term]
   determine if a term is a function.

   @param term The term.

   @return [true] if [term] is a function.
*)
val term_is_fun : term -> bool

(**
   [term_is_var term]
   determine if a term is a variable.

   @param term The term.

   @return [true] if [term] is a variable.
*)
val term_is_var : term -> bool

(**
   [term_is_bound_var term]
   determine if a term is a bound variable.

   @param term The term.

   @return [true] if [term] is a variable and bound.
*)
val term_is_bound_var : term -> bool

(**
   [term_is_value term]
   determine if a term is a value.

   @param term The term.

   @return [true] if [term] is a value.
*)
val term_is_value : term -> bool

(**
   [term_is_bv_value term]
   determine if a term is a bit-vector value.

   @param term The term.

   @return [true] if [term] is a bit-vector value.
*)
val term_is_bv_value : term -> bool

(**
   [term_is_fp_value term]
   determine if a term is a floating-point value.

   @param term The term.

   @return [true] if [term] is a floating-point value.
*)
val term_is_fp_value : term -> bool

(**
   [term_is_rm_value term]
   determine if a term is a rounding mode value.

   @param term The term.

   @return [true] if [term] is a rounding mode value.
*)
val term_is_rm_value : term -> bool

(**
   [term_is_bv term]
   determine if a term is a bit-vector term.

   @param term The term.

   @return [true] if [term] is a bit-vector term.
*)
val term_is_bv : term -> bool

(**
   [term_is_fp term]
   determine if a term is a floating-point term.

   @param term The term.

   @return [true] if [term] is a floating-point term.
*)
val term_is_fp : term -> bool

(**
   [term_is_rm term]
   determine if a term is a rounding mode term.

   @param term The term.

   @return [true] if [term] is a rounding mode term.
*)
val term_is_rm : term -> bool

(**
   [term_is_bv_value_zero term]
   determine if a term is a bit-vector value representing zero.

   @param term The term.

   @return [true] if [term] is a bit-vector zero value.
*)
val term_is_bv_value_zero : term -> bool

(**
   [term_is_bv_value_one term]
   determine if a term is a bit-vector value representing one.

   @param term The term.

   @return [true] if [term] is a bit-vector one value.
*)
val term_is_bv_value_one : term -> bool

(**
   [term_is_bv_value_ones term]
   determine if a term is a bit-vector value with all bits set to one.

   @param term The term.

   @return [true] if [term] is a bit-vector value with all bits set to one.
*)
val term_is_bv_value_ones : term -> bool

(**
   [term_is_bv_value_min_signed term]
   determine if a term is a bit-vector minimum signed value.

   @param term The term.

   @return [true] if [term] is a bit-vector value with the most significant bit
         set to 1 and all other bits set to 0.
*)
val term_is_bv_value_min_signed : term -> bool

(**
   [term_is_bv_value_max_signed term]
   determine if a term is a bit-vector maximum signed value.

   @param term The term.

   @return [true] if [term] is a bit-vector value with the most significant bit
         set to 0 and all other bits set to 1.
*)
val term_is_bv_value_max_signed : term -> bool

(**
   [term_is_fp_value_pos_zero term]
   determine if a term is a floating-point positive zero (+zero) value.

   @param term The term.

   @return [true] if [term] is a floating-point +zero value.
*)
val term_is_fp_value_pos_zero : term -> bool

(**
   [term_is_fp_value_neg_zero term]
   determine if a term is a floating-point value negative zero (-zero).

   @param term The term.

   @return [true] if [term] is a floating-point value negative zero.
*)
val term_is_fp_value_neg_zero : term -> bool

(**
   [term_is_fp_value_pos_inf term]
   determine if a term is a floating-point positive infinity (+oo) value.

   @param term The term.

   @return [true] if [term] is a floating-point +oo value.
*)
val term_is_fp_value_pos_inf : term -> bool

(**
   [term_is_fp_value_neg_inf term]
   determine if a term is a floating-point negative infinity (-oo) value.

   @param term The term.

   @return [true] if [term] is a floating-point -oo value.
*)
val term_is_fp_value_neg_inf : term -> bool

(**
   [term_is_fp_value_nan term]
   determine if a term is a floating-point NaN value.

   @param term The term.

   @return [true] if [term] is a floating-point NaN value.
*)
val term_is_fp_value_nan : term -> bool

(**
   [term_is_const_array term]
   determine if a term is a constant array.

   @param term The term.

   @return [true] if [term] is a constant array.
*)
val term_is_const_array : term -> bool

(** {1 Solver} *)

(** A satisfiability result. *)
type result =
  | Sat      (** sat *)
  | Unsat    (** unsat *)
  | Unknown  (** unknown *)

(**
   [parse t input pp]
   parse input file.

   The format of the input file is auto detected.
   Requires that no terms have been created yet.

   @param t The Bitwuzla instance.
   @param input The input file.
   @param pp The output log.

   @return [Sat] if the input formula was simplified to true,
         [Unsat] if it was simplified to false,
         and [Unknown] otherwise.

   @raise Failure in case of parsing error.
*)
val parse : t -> string -> Format.formatter -> result

(**
   [parse_format t format input pp]
   parse input file, assumed to be given in the specified format.

   Requires that no terms have been created yet.

   @param t The Bitwuzla instance.
   @param format The input format for printing the model. Either [`Btor] for
               the BTOR format, [`Btor2] for the BTOR2 format, or [`Smt2]
               for the SMT-LIB v2 format.
   @param input The input file.
   @param pp The output log.

   @return [Sat] if the input formula was simplified to true,
         [Unsat] if it was simplified to false,
         and [Unknown] otherwise.

   @raise Failure in case of parsing error.
*)
val parse_format : t -> [ `Btor | `Btor2 | `Smt2 ] -> string ->
  Format.formatter -> result


(** {2 Formula} *)

(**
   [push t nlevels]
   push context levels.

   Requires that incremental solving has been enabled via
   {!val:set_option}.

   Assumptions added via this {!val:bitwuzla_assume} are not affected by
       context level changes and are only valid until the next
       {!val:check_sat} call, no matter at which level they were
       assumed.

   @param t The Bitwuzla instance.
   @param nlevels The number of context levels to push.
*)
val push : t -> int -> unit

(**
   [pop t nlevels]
   pop context levels.

   Requires that incremental solving has been enabled via {!val:set_option}.

   Assumptions added via this {!val:assume} are not affected by
       context level changes and are only valid until the next
       {!val:check_sat} call, no matter at which level they were
       assumed.

   @param t The Bitwuzla instance.
   @param nlevels The number of context levels to pop.
*)
val pop : t -> int -> unit

(**
   [mk_assert t term]
   assert formula.

   @param t The Bitwuzla instance.
   @param term The formula to assert.
*)
val mk_assert : t -> term -> unit

(**
   [mk_assume t term]
   assume formula.

   Requires that incremental solving has been enabled via {!val:set_option}.

   Assumptions added via this function are not affected by context level
       changes and are only valid until the next {!val:check_sat} call,
       no matter at which level they were assumed.

   @param t The Bitwuzla instance.
   @param term The formula to assume.
*)
val mk_assume : t -> term -> unit

(**
   [fixate_assumptions t]
   assert all added assumptions.

   @param t The Bitwuzla instance.
*)
val fixate_assumptions : t -> unit

(**
   [reset_assumptions t]
   reset all added assumptions.

   @param t The Bitwuzla instance.
*)
val reset_assumptions : t -> unit

(**
   [dump_formula t format pp]
   print the current input formula.

   Requires that incremental solving is not enabled.

   @param t The Bitwuzla instance.
   @param format The output format for printing the formula. Either
               [`Aiger_ascii] for the AIGER ascii format, [`Aiger_binary]
               for the binary AIGER format, [`Btor] for the BTOR format, or
               [`Smt2] for the SMT-LIB v2 format.
   @param pp The outpout formatter.
*)
val dump_formula : t -> [ `Aiger_ascii | `Aiger_binary | `Btor | `Smt2 ] ->
  Format.formatter -> unit

(** {2 Check} *)

(**
   [simplify t]
   simplify the current input formula.

   Assumptions are not considered for simplification.

   @param t The Bitwuzla instance.

   @return [Sat] if the input formula was simplified to true,
         [Unsat] if it was simplified to false, and
         [Unknown] otherwise.
*)
val simplify : t -> result

(**
   [check_sat t]
   check satisfiability of current input formula.

   An input formula consists of assertions added via {!val:mk_assert}.
   The search for a solution can by guided by making assumptions via
   {!val:mk_assume}.

   Assertions and assumptions are combined via Boolean and.  Multiple
       calls to this function require enabling incremental solving via
       {!val:set_option}.

   @param t The Bitwuzla instance.

   @return {!constructor:Sat} if the input formula is satisfiable and
         {!constructor:Unsat} if it is unsatisfiable, and {!constructor:Unknown}
         when neither satisfiability nor unsatisfiability was determined.
         This can happen when [t] was terminated via a termination callback.
*)
val check_sat : t -> result

(** {2 Sat} *)

(**
   [get_value t term]
   get a term representing the model value of a given term.

   Requires that the last {!val:check_sat} query returned {!constructor:Sat}.

   @param t The Bitwuzla instance.
   @param term The term to query a model value for.

   @return A term representing the model value of term [term].
*)
val get_value : t -> term -> term

(**
   [get_bv_value t term]
   get string representation of the current model value of given bit-vector
   term.

   @param t The Bitwuzla instance.
   @param term The term to query a model value for.

   @return Binary string representation of current model value of term [term].
*)
val get_bv_value : t -> term -> string

(**
   [get_fp_value t term]
   get string of IEEE 754 standard representation of the current model value of
   given floating-point term.

   @param t The Bitwuzla instance.
   @param term The term to query a model value for.

   @return Binary string representations of the sign bit, the exponent
           bit-vector and significand bit-vector values.
*)
val get_fp_value : t -> term -> string * string * string

(**
   [get_rm_value t term]
   get string representation of the current model value of given rounding mode
   term.

   @param t The Bitwuzla instance.
   @param term The rounding mode term to query a model value for.

   @return String representation of rounding mode (RNA, RNE, RTN, RTP, RTZ).
*)
val get_rm_value : t -> term -> string

(**
   [get_array_value t term]
   get the current model value of given array term.

   The string representation of indices and values can be queried via
   {!val:get_bv_value}, {!val:get_fp_value}, and {!val:get_rm_value}.

   @param t The Bitwuzla instance.
   @param term The term to query a model value for.

   @return An array of associations between indices and values.
           The value of all other indices is [Some default] when
           base array is constant array, otherwise, it is [None].
*)
val get_array_value : t -> term -> (term * term) array * term option

(**
   [get_fun_value t term]
   get the current model value of given function term.

   The string representation of arguments and values can be queried via
   {!val:get_bv_value}, {!val:get_fp_value}, and {!val:get_rm_value}.

   @param t The Bitwuzla instance.
   @param term The term to query a model value for.

   @return An array of associations between `arity` arguments and a value.
*)
val get_fun_value : t -> term -> term array array

(**
   [print_model t]
   print a model for the current input formula.

   Requires that the last {!val:check_sat} query returned [Sat].

   @param t The Bitwuzla instance.
   @param format The output format for printing the model. Either ["btor"] for
               the BTOR format, or ["smt2"] for the SMT-LIB v2 format.
   @param file The file to print the model to.
*)
val print_model : t -> [ `Btor | `Smt2 ] -> Format.formatter -> unit


(** {2 Unsat} *)

(**
   [is_unsat_assumption t term]
   determine if an assumption is an unsat assumption.

   Unsat assumptions are assumptions that force an input formula to become
   unsatisfiable. Unsat assumptions handling in Bitwuzla is analogous to
   failed assumptions in MiniSAT.

   Requires that incremental solving has been enabled via
   {!val:set_option}.

   Requires that the last {!val:check_sat} query returned [Unsat].

   @param t The Bitwuzla instance.
   @param term The assumption to check for.

   @return [true] if given assumption is an unsat assumption.
*)
val is_unsat_assumption : t -> term -> bool

(**
   [get_unsat_assumptions t]
   get the set of unsat assumptions.

   Unsat assumptions are assumptions that force an input formula to become
   unsatisfiable. Unsat assumptions handling in Bitwuzla is analogous to
   failed assumptions in MiniSAT.

   Requires that incremental solving has been enabled via {!val:set_option}.

   Requires that the last {!val:check_sat} query returned {!constructor:Unsat}.

   @param t The Bitwuzla instance.

   @return An array with unsat assumptions.
*)
val get_unsat_assumptions : t -> term array

(**
   [get_unsat_core t]
   get the set unsat core (unsat assertions).

   The unsat core consists of the set of assertions that force an input formula
   to become unsatisfiable.

   Requires that the last {!val:check_sat} query returned {!constructor:Unsat}.

   @param t The Bitwuzla instance.

   @return An array with unsat assertions.
*)
val get_unsat_core : t -> term array
