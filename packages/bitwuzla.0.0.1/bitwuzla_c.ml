type bvbase =
  | Bin
  | Dec
  | Hex

let bvbase_to_c = function
  | Hex -> 2
  | Dec -> 1
  | Bin -> 0

type opt =
  | Engine
  | Exit_codes
  | Input_format
  | Incremental
  | Loglevel
  | Output_format
  | Output_number_format
  | Pretty_print
  | Print_dimacs
  | Produce_models
  | Produce_unsat_cores
  | Seed
  | Verbosity
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

let opt_to_c = function
  | Sat_engine_cadical_freeze -> 104
  | Parse_interactive -> 103
  | Ls_share_sat -> 102
  | Declsort_bv_witdh -> 101
  | Check_unsat_assumptions -> 100
  | Check_unconstrained -> 99
  | Check_model -> 98
  | Quant_synth_qi -> 97
  | Quant_synth_limit -> 96
  | Quant_synth_ite_complete -> 95
  | Quant_fixsynth -> 94
  | Quant_synth -> 93
  | Quant_miniscope -> 92
  | Quant_dual_solver -> 91
  | Quant_der -> 90
  | Quant_cer -> 89
  | Aigprop_use_restarts -> 88
  | Aigprop_use_bandit -> 87
  | Aigprop_nprops -> 86
  | Prop_xor -> 85
  | Prop_skip_no_progress -> 84
  | Prop_sext -> 83
  | Prop_use_restarts -> 82
  | Prop_use_inv_lt_concat -> 81
  | Prop_use_bandit -> 80
  | Prop_prob_use_inv_value -> 79
  | Prop_prob_slice_keep_dc -> 78
  | Prop_prob_slice_flip -> 77
  | Prop_prob_random_input -> 76
  | Prop_prob_flip_cond_const -> 75
  | Prop_prob_flip_cond -> 74
  | Prop_prob_eq_flip -> 73
  | Prop_prob_and_flip -> 72
  | Prop_prob_fallback_random_value -> 71
  | Prop_path_sel -> 70
  | Prop_nupdates -> 69
  | Prop_nprops -> 68
  | Prop_no_move_on_conflict -> 67
  | Prop_infer_ineq_bounds -> 66
  | Prop_flip_cond_const_npathsel -> 65
  | Prop_flip_cond_const_delta -> 64
  | Prop_entailed -> 63
  | Prop_const_domains -> 62
  | Prop_const_bits -> 61
  | Prop_ashr -> 60
  | Sls_use_bandit -> 59
  | Sls_use_restarts -> 58
  | Sls_strategy -> 57
  | Sls_nflips -> 56
  | Sls_prob_move_rand_walk -> 55
  | Sls_move_segment -> 54
  | Sls_move_range -> 53
  | Sls_move_rand_walk -> 52
  | Sls_move_rand_range -> 51
  | Sls_move_rand_all -> 50
  | Sls_move_prop_nslss -> 49
  | Sls_move_prop_nprops -> 48
  | Sls_move_prop_force_rw -> 47
  | Sls_move_prop -> 46
  | Sls_move_inc_move_test -> 45
  | Sls_move_gw -> 44
  | Sls_just -> 43
  | Fun_store_lambdas -> 42
  | Fun_presls -> 41
  | Fun_preprop -> 40
  | Fun_just_heuristic -> 39
  | Fun_just -> 38
  | Fun_lazy_synthesize -> 37
  | Fun_eager_lemmas -> 36
  | Fun_dual_prop_qsort -> 35
  | Fun_dual_prop -> 34
  | Rw_sort_exp -> 33
  | Rw_sort_aigvec -> 32
  | Rw_sort_aig -> 31
  | Rw_slt -> 30
  | Rw_simplify_constraints -> 29
  | Rw_normalize_add -> 28
  | Rw_normalize -> 27
  | Rw_level -> 26
  | Rw_extract_arith -> 25
  | Pp_var_subst -> 24
  | Pp_unconstrained_optimization -> 23
  | Pp_skeleton_preproc -> 22
  | Pp_normalize_add -> 21
  | Pp_nondestr_subst -> 20
  | Pp_merge_lambdas -> 19
  | Pp_extract_lambdas -> 18
  | Pp_eliminate_ites -> 17
  | Pp_eliminate_extracts -> 16
  | Pp_beta_reduce -> 15
  | Pp_ackermann -> 14
  | Verbosity -> 13
  | Seed -> 12
  | Produce_unsat_cores -> 10
  | Produce_models -> 9
  | Print_dimacs -> 8
  | Pretty_print -> 7
  | Output_number_format -> 6
  | Output_format -> 5
  | Loglevel -> 4
  | Incremental -> 3
  | Input_format -> 2
  | Exit_codes -> 1
  | Engine -> 0

type result =
  | Sat
  | Unsat
  | Unknown

let result_from_c = function
  | 0 -> Unknown
  | 20 -> Unsat
  | 10 -> Sat
  | _ -> assert false

type roundingmode =
  | Rne
  | Rna
  | Rtn
  | Rtp
  | Rtz

let roundingmode_to_c = function
  | Rtz -> 4
  | Rtp -> 3
  | Rtn -> 2
  | Rna -> 1
  | Rne -> 0

type kind =
  | And
  | Apply
  | Array_select
  | Array_store
  | Bv_add
  | Bv_and
  | Bv_ashr
  | Bv_comp
  | Bv_concat
  | Bv_dec
  | Bv_inc
  | Bv_mul
  | Bv_nand
  | Bv_neg
  | Bv_nor
  | Bv_not
  | Bv_or
  | Bv_redand
  | Bv_redor
  | Bv_redxor
  | Bv_rol
  | Bv_ror
  | Bv_sadd_overflow
  | Bv_sdiv_overflow
  | Bv_sdiv
  | Bv_sge
  | Bv_sgt
  | Bv_shl
  | Bv_shr
  | Bv_sle
  | Bv_slt
  | Bv_smod
  | Bv_smul_overflow
  | Bv_srem
  | Bv_ssub_overflow
  | Bv_sub
  | Bv_uadd_overflow
  | Bv_udiv
  | Bv_uge
  | Bv_ugt
  | Bv_ule
  | Bv_ult
  | Bv_umul_overflow
  | Bv_urem
  | Bv_usub_overflow
  | Bv_xnor
  | Bv_xor
  | Distinct
  | Equal
  | Exists
  | Forall
  | Fp_abs
  | Fp_add
  | Fp_div
  | Fp_eq
  | Fp_fma
  | Fp_fp
  | Fp_geq
  | Fp_gt
  | Fp_is_inf
  | Fp_is_nan
  | Fp_is_neg
  | Fp_is_normal
  | Fp_is_pos
  | Fp_is_subnormal
  | Fp_is_zero
  | Fp_leq
  | Fp_lt
  | Fp_max
  | Fp_min
  | Fp_mul
  | Fp_neg
  | Fp_rem
  | Fp_rti
  | Fp_sqrt
  | Fp_sub
  | Iff
  | Implies
  | Ite
  | Lambda
  | Not
  | Or
  | Xor
  | Bv_extract
  | Bv_repeat
  | Bv_roli
  | Bv_rori
  | Bv_sign_extend
  | Bv_zero_extend
  | Fp_to_fp_from_bv
  | Fp_to_fp_from_fp
  | Fp_to_fp_from_sbv
  | Fp_to_fp_from_ubv
  | Fp_to_sbv
  | Fp_to_ubv

let kind_to_c = function
  | Fp_to_ubv -> 94
  | Fp_to_sbv -> 93
  | Fp_to_fp_from_ubv -> 92
  | Fp_to_fp_from_sbv -> 91
  | Fp_to_fp_from_fp -> 90
  | Fp_to_fp_from_bv -> 89
  | Bv_zero_extend -> 88
  | Bv_sign_extend -> 87
  | Bv_rori -> 86
  | Bv_roli -> 85
  | Bv_repeat -> 84
  | Bv_extract -> 83
  | Xor -> 82
  | Or -> 81
  | Not -> 80
  | Lambda -> 79
  | Ite -> 78
  | Implies -> 77
  | Iff -> 76
  | Fp_sub -> 75
  | Fp_sqrt -> 74
  | Fp_rti -> 73
  | Fp_rem -> 72
  | Fp_neg -> 71
  | Fp_mul -> 70
  | Fp_min -> 69
  | Fp_max -> 68
  | Fp_lt -> 67
  | Fp_leq -> 66
  | Fp_is_zero -> 65
  | Fp_is_subnormal -> 64
  | Fp_is_pos -> 63
  | Fp_is_normal -> 62
  | Fp_is_neg -> 61
  | Fp_is_nan -> 60
  | Fp_is_inf -> 59
  | Fp_gt -> 58
  | Fp_geq -> 57
  | Fp_fp -> 56
  | Fp_fma -> 55
  | Fp_eq -> 54
  | Fp_div -> 53
  | Fp_add -> 52
  | Fp_abs -> 51
  | Forall -> 50
  | Exists -> 49
  | Equal -> 48
  | Distinct -> 47
  | Bv_xor -> 46
  | Bv_xnor -> 45
  | Bv_usub_overflow -> 44
  | Bv_urem -> 43
  | Bv_umul_overflow -> 42
  | Bv_ult -> 41
  | Bv_ule -> 40
  | Bv_ugt -> 39
  | Bv_uge -> 38
  | Bv_udiv -> 37
  | Bv_uadd_overflow -> 36
  | Bv_sub -> 35
  | Bv_ssub_overflow -> 34
  | Bv_srem -> 33
  | Bv_smul_overflow -> 32
  | Bv_smod -> 31
  | Bv_slt -> 30
  | Bv_sle -> 29
  | Bv_shr -> 28
  | Bv_shl -> 27
  | Bv_sgt -> 26
  | Bv_sge -> 25
  | Bv_sdiv -> 24
  | Bv_sdiv_overflow -> 23
  | Bv_sadd_overflow -> 22
  | Bv_ror -> 21
  | Bv_rol -> 20
  | Bv_redxor -> 19
  | Bv_redor -> 18
  | Bv_redand -> 17
  | Bv_or -> 16
  | Bv_not -> 15
  | Bv_nor -> 14
  | Bv_neg -> 13
  | Bv_nand -> 12
  | Bv_mul -> 11
  | Bv_inc -> 10
  | Bv_dec -> 9
  | Bv_concat -> 8
  | Bv_comp -> 7
  | Bv_ashr -> 6
  | Bv_and -> 5
  | Bv_add -> 4
  | Array_store -> 3
  | Array_select -> 2
  | Apply -> 1
  | And -> 0

let kind_from_c = function
  | 94 -> Fp_to_ubv
  | 93 -> Fp_to_sbv
  | 92 -> Fp_to_fp_from_ubv
  | 91 -> Fp_to_fp_from_sbv
  | 90 -> Fp_to_fp_from_fp
  | 89 -> Fp_to_fp_from_bv
  | 88 -> Bv_zero_extend
  | 87 -> Bv_sign_extend
  | 86 -> Bv_rori
  | 85 -> Bv_roli
  | 84 -> Bv_repeat
  | 83 -> Bv_extract
  | 82 -> Xor
  | 81 -> Or
  | 80 -> Not
  | 79 -> Lambda
  | 78 -> Ite
  | 77 -> Implies
  | 76 -> Iff
  | 75 -> Fp_sub
  | 74 -> Fp_sqrt
  | 73 -> Fp_rti
  | 72 -> Fp_rem
  | 71 -> Fp_neg
  | 70 -> Fp_mul
  | 69 -> Fp_min
  | 68 -> Fp_max
  | 67 -> Fp_lt
  | 66 -> Fp_leq
  | 65 -> Fp_is_zero
  | 64 -> Fp_is_subnormal
  | 63 -> Fp_is_pos
  | 62 -> Fp_is_normal
  | 61 -> Fp_is_neg
  | 60 -> Fp_is_nan
  | 59 -> Fp_is_inf
  | 58 -> Fp_gt
  | 57 -> Fp_geq
  | 56 -> Fp_fp
  | 55 -> Fp_fma
  | 54 -> Fp_eq
  | 53 -> Fp_div
  | 52 -> Fp_add
  | 51 -> Fp_abs
  | 50 -> Forall
  | 49 -> Exists
  | 48 -> Equal
  | 47 -> Distinct
  | 46 -> Bv_xor
  | 45 -> Bv_xnor
  | 44 -> Bv_usub_overflow
  | 43 -> Bv_urem
  | 42 -> Bv_umul_overflow
  | 41 -> Bv_ult
  | 40 -> Bv_ule
  | 39 -> Bv_ugt
  | 38 -> Bv_uge
  | 37 -> Bv_udiv
  | 36 -> Bv_uadd_overflow
  | 35 -> Bv_sub
  | 34 -> Bv_ssub_overflow
  | 33 -> Bv_srem
  | 32 -> Bv_smul_overflow
  | 31 -> Bv_smod
  | 30 -> Bv_slt
  | 29 -> Bv_sle
  | 28 -> Bv_shr
  | 27 -> Bv_shl
  | 26 -> Bv_sgt
  | 25 -> Bv_sge
  | 24 -> Bv_sdiv
  | 23 -> Bv_sdiv_overflow
  | 22 -> Bv_sadd_overflow
  | 21 -> Bv_ror
  | 20 -> Bv_rol
  | 19 -> Bv_redxor
  | 18 -> Bv_redor
  | 17 -> Bv_redand
  | 16 -> Bv_or
  | 15 -> Bv_not
  | 14 -> Bv_nor
  | 13 -> Bv_neg
  | 12 -> Bv_nand
  | 11 -> Bv_mul
  | 10 -> Bv_inc
  | 9 -> Bv_dec
  | 8 -> Bv_concat
  | 7 -> Bv_comp
  | 6 -> Bv_ashr
  | 5 -> Bv_and
  | 4 -> Bv_add
  | 3 -> Array_store
  | 2 -> Array_select
  | 1 -> Apply
  | 0 -> And
  | _ -> assert false

let format_to_c = function
  | `Btor         -> 0
  | `Btor2        -> 1
  | `Smt2         -> 2
  | `Aiger_ascii  -> 3
  | `Aiger_binary -> 4

type t
type sort [@@immediate]
type term [@@immediate]
type 'a cookie = unit

external init : unit -> unit
  = "ocaml_bitwuzla_init"

external create : unit -> t
  = "ocaml_bitwuzla_new"
external delete : t -> unit
  = "ocaml_bitwuzla_delete"
external reset : t -> unit
  = "ocaml_bitwuzla_reset"

external copyright : t -> string
  = "ocaml_bitwuzla_copyright"
external version : t -> string
  = "ocaml_bitwuzla_version"

external terminate : t -> bool
  = "ocaml_bitwuzla_terminate"
external set_termination_callback : t -> ('a -> int) * 'a -> unit
  = "ocaml_bitwuzla_set_termination_callback"
let set_termination_callback t f a = set_termination_callback t (f, a)
external get_termination_callback_state : unit -> 'a
  = "ocaml_bitwuzla_get_termination_callback_state"

external set_option : t -> (int [@untagged]) -> int -> unit
  = "ocaml_bitwuzla_set_option" "native_bitwuzla_set_option"
let set_option t o v = set_option t (opt_to_c o) v
external set_option_str : t -> (int [@untagged]) -> string -> unit
  = "ocaml_bitwuzla_set_option_str" "native_bitwuzla_set_option_str"
let set_option_str t o v = set_option_str t (opt_to_c o) v
external get_option : t -> (int [@untagged]) -> int
  = "ocaml_bitwuzla_get_option" "native_bitwuzla_get_option"
let get_option t o = get_option t (opt_to_c o)

external mk_array_sort : t  -> sort -> sort -> sort
  = "ocaml_bitwuzla_mk_array_sort"
external mk_bool_sort : t -> sort
  = "ocaml_bitwuzla_mk_bool_sort"
external mk_bv_sort : t -> int -> sort
  = "ocaml_bitwuzla_mk_bv_sort"
external mk_fp_sort : t -> int -> int -> sort
  = "ocaml_bitwuzla_mk_fp_sort"
external mk_fun_sort : t -> sort array -> sort -> sort
  = "ocaml_bitwuzla_mk_fun_sort"
external mk_rm_sort : t -> sort
  = "ocaml_bitwuzla_mk_rm_sort"

external mk_true : t -> term
  = "ocaml_bitwuzla_mk_true"
external mk_false : t -> term
  = "ocaml_bitwuzla_mk_false"

external mk_bv_zero : t -> sort -> term
  = "ocaml_bitwuzla_mk_bv_zero"
external mk_bv_one : t -> sort -> term
  = "ocaml_bitwuzla_mk_bv_one"
external mk_bv_ones : t -> sort -> term
  = "ocaml_bitwuzla_mk_bv_ones"
external mk_bv_min_signed : t -> sort -> term
  = "ocaml_bitwuzla_mk_bv_min_signed"
external mk_bv_max_signed : t -> sort -> term
  = "ocaml_bitwuzla_mk_bv_max_signed"

external mk_fp_pos_zero : t -> sort -> term
  = "ocaml_bitwuzla_mk_fp_pos_zero"
external mk_fp_neg_zero : t -> sort -> term
  = "ocaml_bitwuzla_mk_fp_neg_zero"
external mk_fp_pos_inf : t -> sort -> term
  = "ocaml_bitwuzla_mk_fp_pos_inf"
external mk_fp_neg_inf : t -> sort -> term
  = "ocaml_bitwuzla_mk_fp_neg_inf"
external mk_fp_nan : t -> sort -> term
  = "ocaml_bitwuzla_mk_fp_nan"
external mk_bv_value : t -> sort -> string -> (int [@untagged]) -> term
  = "ocaml_bitwuzla_mk_bv_value" "native_bitwuzla_mk_bv_value"
let mk_bv_value t s v b = mk_bv_value t s v @@ bvbase_to_c b
external mk_bv_value_int : t -> sort -> int -> term
  = "ocaml_bitwuzla_mk_bv_value_int"
external mk_fp_value : t -> term -> term -> term -> term
  = "ocaml_bitwuzla_mk_fp_value"
external mk_fp_value_from_real : t -> sort -> term -> string -> term
  = "ocaml_bitwuzla_mk_fp_value_from_real"
external mk_fp_value_from_rational : t -> sort -> term -> string -> string ->
  term
  = "ocaml_bitwuzla_mk_fp_value_from_rational"
external mk_rm_value : t -> (int [@untagged]) -> term
  = "ocaml_bitwuzla_mk_rm_value" "native_bitwuzla_mk_rm_value"
let mk_rm_value t m = mk_rm_value t @@ roundingmode_to_c m

external mk_term1 : t -> (int [@untagged]) -> term -> term
  = "ocaml_bitwuzla_mk_term1" "native_bitwuzla_mk_term1"
let mk_term1 t k e = mk_term1 t (kind_to_c k) e
external mk_term2 : t -> (int [@untagged]) -> term -> term -> term
  = "ocaml_bitwuzla_mk_term2" "native_bitwuzla_mk_term2"
let mk_term2 t k e1 e2 = mk_term2 t (kind_to_c k) e1 e2
external mk_term3 : t -> (int [@untagged]) -> term -> term -> term -> term
  = "ocaml_bitwuzla_mk_term3" "native_bitwuzla_mk_term3"
let mk_term3 t k e1 e2 e3 = mk_term3 t (kind_to_c k) e1 e2 e3
external mk_term : t -> (int [@untagged]) -> term array ->
  term
  = "ocaml_bitwuzla_mk_term" "native_bitwuzla_mk_term"
let mk_term t k a = mk_term t (kind_to_c k) a
external mk_term1_indexed1 : t -> (int [@untagged]) -> term -> int -> term
  = "ocaml_bitwuzla_mk_term1_indexed1" "native_bitwuzla_mk_term1_indexed1"
let mk_term1_indexed1 t k e i = mk_term1_indexed1 t (kind_to_c k) e i
external mk_term1_indexed2 : t -> (int [@untagged]) -> term -> int -> int ->
  term
  = "ocaml_bitwuzla_mk_term1_indexed2" "native_bitwuzla_mk_term1_indexed2"
let mk_term1_indexed2 t k e i1 i2 = mk_term1_indexed2 t (kind_to_c k) e i1 i2
external mk_term2_indexed1 : t -> (int [@untagged]) -> term -> term -> int ->
  term
  = "ocaml_bitwuzla_mk_term2_indexed1" "native_bitwuzla_mk_term2_indexed1"
let mk_term2_indexed1 t k e1 e2 i = mk_term2_indexed1 t (kind_to_c k) e1 e2 i
external mk_term2_indexed2 : t -> kind -> term -> term -> int -> int -> term
  = "ocaml_bitwuzla_mk_term2_indexed2_byte6" "ocaml_bitwuzla_mk_term2_indexed2"
external mk_term_indexed : t -> (int [@untagged]) ->
  term array -> int array -> term
  = "ocaml_bitwuzla_mk_term_indexed" "native_bitwuzla_mk_term_indexed"
let mk_term_indexed t k e i = mk_term_indexed t (kind_to_c k) e i
external mk_const : t -> sort -> string -> term
  = "ocaml_bitwuzla_mk_const"
external mk_const_array : t -> sort -> term -> term
  = "ocaml_bitwuzla_mk_const_array"
external mk_var : t -> sort -> string -> term
  = "ocaml_bitwuzla_mk_var"

external push : t -> int -> unit
  = "ocaml_bitwuzla_push"
external pop : t -> int -> unit
  = "ocaml_bitwuzla_pop"
external mk_assert : t -> term -> unit
  = "ocaml_bitwuzla_assert"
external mk_assume : t -> term -> unit
  = "ocaml_bitwuzla_assume"
external is_unsat_assumption : t -> term -> bool
  = "ocaml_bitwuzla_is_unsat_assumption"
external get_unsat_assumptions : t -> term array
  = "ocaml_bitwuzla_get_unsat_assumptions"

external get_unsat_core : t -> term array
  = "ocaml_bitwuzla_get_unsat_core"
external fixate_assumptions : t -> unit
  = "ocaml_bitwuzla_fixate_assumptions"
external reset_assumptions : t -> unit
  = "ocaml_bitwuzla_reset_assumptions"
external simplify : t -> (int [@untagged])
  = "ocaml_bitwuzla_simplify" "native_bitwuzla_simplify"
let simplify t = result_from_c @@ simplify t
external check_sat : t -> (int [@untagged])
  = "ocaml_bitwuzla_check_sat" "native_bitwuzla_check_sat"
let check_sat t = result_from_c @@ check_sat t

external get_value : t -> term -> term
  = "ocaml_bitwuzla_get_value"
external print_model : t -> (int [@untagged]) -> Format.formatter -> unit
  = "ocaml_bitwuzla_print_model" "native_bitwuzla_print_model"
let print_model t (f : [ `Btor | `Smt2 ]) k = print_model t (format_to_c f) k

external dump_formula : t -> (int [@untagged]) -> Format.formatter -> unit
  = "ocaml_bitwuzla_dump_formula" "native_bitwuzla_dump_formula"
let dump_formula t (f : [ `Aiger_ascii | `Aiger_binary | `Btor | `Smt2 ]) k
  = dump_formula t (format_to_c f) k
external parse : t -> string -> Format.formatter -> (int [@untagged])
  = "ocaml_bitwuzla_parse" "native_bitwuzla_parse"
let parse t p k = result_from_c @@ parse t p k
external parse_format : t -> (int [@untagged]) -> string -> Format.formatter ->
  (int [@untagged])
  = "ocaml_bitwuzla_parse_format" "native_bitwuzla_parse_format"
let parse_format t (f : [ `Btor | `Btor2 | `Smt2 ]) p k
  = result_from_c @@ parse_format t (format_to_c f) p k

external substitute_term : t -> term -> (term * term) array -> term
  = "ocaml_bitwuzla_substitute_term"
external substitute_terms : t -> term array -> (term * term) array -> unit
  = "ocaml_bitwuzla_substitute_terms"

external sort_hash : sort -> int
  = "ocaml_bitwuzla_sort_hash"
external sort_bv_get_size : sort -> int
  = "ocaml_bitwuzla_sort_bv_get_size"
external sort_fp_get_exp_size : sort -> int
  = "ocaml_bitwuzla_sort_fp_get_exp_size"
external sort_fp_get_sig_size : sort -> int
  = "ocaml_bitwuzla_sort_fp_get_sig_size"
external sort_array_get_index : sort -> sort
  = "ocaml_bitwuzla_sort_array_get_index"
external sort_array_get_element : sort -> sort
  = "ocaml_bitwuzla_sort_array_get_element"
external sort_fun_get_domain_sorts : sort -> sort array
  = "ocaml_bitwuzla_sort_fun_get_domain_sorts"
external sort_fun_get_codomain : sort -> sort
  = "ocaml_bitwuzla_sort_fun_get_codomain"
external sort_fun_get_arity : sort -> int
  = "ocaml_bitwuzla_sort_fun_get_arity"
external sort_is_equal : sort -> sort -> bool
  = "ocaml_bitwuzla_sort_is_equal"
external sort_is_array : sort -> bool
  = "ocaml_bitwuzla_sort_is_array"
external sort_is_bv : sort -> bool
  = "ocaml_bitwuzla_sort_is_bv"
external sort_is_fp : sort -> bool
  = "ocaml_bitwuzla_sort_is_fp"
external sort_is_fun : sort -> bool
  = "ocaml_bitwuzla_sort_is_fun"
external sort_is_rm : sort -> bool
  = "ocaml_bitwuzla_sort_is_rm"
external sort_dump : sort -> (int [@untagged]) -> Format.formatter -> unit
  = "ocaml_bitwuzla_sort_dump" "native_bitwuzla_sort_dump"
let sort_dump t (f : [ `Btor | `Smt2 ]) k = sort_dump t (format_to_c f) k

external term_hash : term -> int
  = "ocaml_bitwuzla_term_hash"
external term_get_kind : term -> (int [@untagged])
  = "ocaml_bitwuzla_term_get_kind" "native_bitwuzla_term_get_kind"
let term_get_kind t = kind_from_c @@ term_get_kind t
external term_get_children : term -> term array
  = "ocaml_bitwuzla_term_get_children"
external term_get_indices : term -> int array
  = "ocaml_bitwuzla_term_get_indices"
external term_is_indexed : term -> bool
  = "ocaml_bitwuzla_term_is_indexed"
external term_get_sort :  term -> sort
  = "ocaml_bitwuzla_term_get_sort"
external term_array_get_index_sort : term -> sort
  = "ocaml_bitwuzla_term_array_get_index_sort"
external term_array_get_element_sort : term -> sort
  = "ocaml_bitwuzla_term_array_get_element_sort"
external term_fun_get_domain_sorts : term -> sort array
  = "ocaml_bitwuzla_term_fun_get_domain_sorts"
external term_fun_get_codomain_sort : term -> sort
  = "ocaml_bitwuzla_term_fun_get_codomain_sort"
external term_bv_get_size : term -> int = "ocaml_bitwuzla_term_bv_get_size"
external term_fp_get_exp_size : term -> int
  = "ocaml_bitwuzla_term_fp_get_exp_size"
external term_fp_get_sig_size : term -> int
  = "ocaml_bitwuzla_term_fp_get_sig_size"
external term_fun_get_arity : term -> int = "ocaml_bitwuzla_term_fun_get_arity"
external term_get_symbol : term -> string = "ocaml_bitwuzla_term_get_symbol"
external term_set_symbol : term -> string -> unit
  = "ocaml_bitwuzla_term_set_symbol"
external term_is_equal_sort : term -> term -> bool
  = "ocaml_bitwuzla_term_is_equal_sort"
external term_is_array : term -> bool
  = "ocaml_bitwuzla_term_is_array"
external term_is_const : term -> bool
  = "ocaml_bitwuzla_term_is_const"
external term_is_fun : term -> bool
  = "ocaml_bitwuzla_term_is_fun"
external term_is_var : term -> bool
  = "ocaml_bitwuzla_term_is_var"
external term_is_bound_var : term -> bool
  = "ocaml_bitwuzla_term_is_bound_var"
external term_is_value : term -> bool
  = "ocaml_bitwuzla_term_is_value"
external term_is_bv_value : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value"
external term_is_fp_value : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value"
external term_is_rm_value : term -> bool
  = "ocaml_bitwuzla_term_is_rm_value"
external term_is_bv : term -> bool
  = "ocaml_bitwuzla_term_is_bv"
external term_is_fp : term -> bool
  = "ocaml_bitwuzla_term_is_fp"
external term_is_rm : term -> bool
  = "ocaml_bitwuzla_term_is_rm"
external term_is_bv_value_zero : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value_zero"
external term_is_bv_value_one : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value_one"
external term_is_bv_value_ones : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value_ones"
external term_is_bv_value_min_signed : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value_min_signed"
external term_is_bv_value_max_signed : term -> bool
  = "ocaml_bitwuzla_term_is_bv_value_max_signed"
external term_is_fp_value_pos_zero : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value_pos_zero"
external term_is_fp_value_neg_zero : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value_neg_zero"
external term_is_fp_value_pos_inf : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value_pos_inf"
external term_is_fp_value_neg_inf : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value_neg_inf"
external term_is_fp_value_nan : term -> bool
  = "ocaml_bitwuzla_term_is_fp_value_nan"
external term_is_const_array : term -> bool
  = "ocaml_bitwuzla_term_is_const_array"
external term_dump : term -> (int [@untagged]) -> Format.formatter -> unit
  = "ocaml_bitwuzla_term_dump" "native_bitwuzla_term_dump"
let term_dump t (f : [ `Btor | `Smt2 ]) k = term_dump t (format_to_c f) k

let () =
  Callback.register "Format.pp_print_string" Format.pp_print_string;
  init ()
