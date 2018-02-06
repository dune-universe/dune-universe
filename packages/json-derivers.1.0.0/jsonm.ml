open Import

type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list ]
[@@deriving_inline compare, sexp, hash]
let _ = fun (_ : value)  -> ()
let rec compare_value : value -> value -> int =
  fun a__001_  ->
    fun b__002_  ->
      if Ppx_compare_lib.phys_equal a__001_ b__002_
      then 0
      else
        (match (a__001_, b__002_) with
         | (`Null,`Null) -> 0
         | (`Bool _left__003_,`Bool _right__004_) ->
             compare_bool _left__003_ _right__004_
         | (`Float _left__005_,`Float _right__006_) ->
             compare_float _left__005_ _right__006_
         | (`String _left__007_,`String _right__008_) ->
             compare_string _left__007_ _right__008_
         | (`A _left__009_,`A _right__010_) ->
             compare_list compare_value _left__009_ _right__010_
         | (`O _left__013_,`O _right__014_) ->
             compare_list
               (fun a__015_  ->
                  fun b__016_  ->
                    let (t__017_,t__018_) = a__015_  in
                    let (t__019_,t__020_) = b__016_  in
                    match compare_string t__017_ t__019_ with
                    | 0 -> compare_value t__018_ t__020_
                    | n -> n) _left__013_ _right__014_
         | (x,y) -> Ppx_compare_lib.polymorphic_compare x y)

let _ = compare_value
let rec __value_of_sexp__ : Sexplib.Sexp.t -> value =
  let _tp_loc = "jsonm.ml.value"  in
  function
  | Sexplib.Sexp.Atom atom as _sexp ->
      (match atom with
       | "Null" -> `Null
       | "Bool" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Float" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "String" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "A" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "O" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom atom)::sexp_args) as _sexp ->
      (match atom with
       | "Bool" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = bool_of_sexp v0  in `Bool v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Float" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = float_of_sexp v0  in `Float v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "String" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = string_of_sexp v0  in `String v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "A" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = list_of_sexp value_of_sexp v0  in `A v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "O" as _tag ->
           (match sexp_args with
            | v0::[] ->
                let v0 =
                  list_of_sexp
                    (function
                     | Sexplib.Sexp.List (v0::v1::[]) ->
                         let v0 = string_of_sexp v0

                         and v1 = value_of_sexp v1
                          in (v0, v1)
                     | sexp ->
                         Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
                           2 sexp) v0
                   in
                `O v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Null" -> Sexplib.Conv_error.ptag_no_args _tp_loc _sexp
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.List _)::_) as sexp ->
      Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp
  | Sexplib.Sexp.List [] as sexp ->
      Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp

and value_of_sexp : Sexplib.Sexp.t -> value =
  let _tp_loc = "jsonm.ml.value"  in
  fun sexp  ->
    try __value_of_sexp__ sexp
    with
    | Sexplib.Conv_error.No_variant_match  ->
        Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp

let _ = __value_of_sexp__

and _ = value_of_sexp

let rec sexp_of_value : value -> Sexplib.Sexp.t =
  function
  | `Null -> Sexplib.Sexp.Atom "Null"
  | `Bool v0 -> Sexplib.Sexp.List [Sexplib.Sexp.Atom "Bool"; sexp_of_bool v0]
  | `Float v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "Float"; sexp_of_float v0]
  | `String v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "String"; sexp_of_string v0]
  | `A v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "A"; sexp_of_list sexp_of_value v0]
  | `O v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "O";
        sexp_of_list
          (function
           | (v0,v1) ->
               let v0 = sexp_of_string v0

               and v1 = sexp_of_value v1
                in Sexplib.Sexp.List [v0; v1]) v0]

let _ = sexp_of_value
let rec (hash_fold_value :
  Ppx_hash_lib.Std.Hash.state -> value -> Ppx_hash_lib.Std.Hash.state) =
  fun hsv  ->
    fun arg  ->
      match arg with
      | `Null -> Ppx_hash_lib.Std.Hash.fold_int hsv 870828711
      | `Bool _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 737456202  in
          hash_fold_bool hsv _v
      | `Float _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 365180284  in
          hash_fold_float hsv _v
      | `String _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv (-976970511)  in
          hash_fold_string hsv _v
      | `A _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 65  in
          hash_fold_list hash_fold_value hsv _v
      | `O _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 79  in
          hash_fold_list
            (fun hsv  ->
               fun arg  ->
                 let (e0,e1) = arg  in
                 let hsv = hash_fold_string hsv e0  in
                 let hsv = hash_fold_value hsv e1  in hsv) hsv _v

and (hash_value : value -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_value hsv arg)
     in
  fun x  -> func x

let _ = hash_fold_value

and _ = hash_value

[@@@deriving.end]

type t =
  [ `A of value list
  | `O of (string * value) list ]
[@@deriving_inline compare, sexp, hash]
let _ = fun (_ : t)  -> ()
let compare : t -> t -> int =
  fun a__021_  ->
    fun b__022_  ->
      if Ppx_compare_lib.phys_equal a__021_ b__022_
      then 0
      else
        (match (a__021_, b__022_) with
         | (`A _left__023_,`A _right__024_) ->
             compare_list compare_value _left__023_ _right__024_
         | (`O _left__027_,`O _right__028_) ->
             compare_list
               (fun a__029_  ->
                  fun b__030_  ->
                    let (t__031_,t__032_) = a__029_  in
                    let (t__033_,t__034_) = b__030_  in
                    match compare_string t__031_ t__033_ with
                    | 0 -> compare_value t__032_ t__034_
                    | n -> n) _left__027_ _right__028_
         | (x,y) -> Ppx_compare_lib.polymorphic_compare x y)

let _ = compare
let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "jsonm.ml.t"  in
  function
  | Sexplib.Sexp.Atom atom as _sexp ->
      (match atom with
       | "A" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "O" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom atom)::sexp_args) as _sexp ->
      (match atom with
       | "A" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = list_of_sexp value_of_sexp v0  in `A v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "O" as _tag ->
           (match sexp_args with
            | v0::[] ->
                let v0 =
                  list_of_sexp
                    (function
                     | Sexplib.Sexp.List (v0::v1::[]) ->
                         let v0 = string_of_sexp v0

                         and v1 = value_of_sexp v1
                          in (v0, v1)
                     | sexp ->
                         Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
                           2 sexp) v0
                   in
                `O v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.List _)::_) as sexp ->
      Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp
  | Sexplib.Sexp.List [] as sexp ->
      Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp

let _ = __t_of_sexp__
let t_of_sexp : Sexplib.Sexp.t -> t =
  let _tp_loc = "jsonm.ml.t"  in
  fun sexp  ->
    try __t_of_sexp__ sexp
    with
    | Sexplib.Conv_error.No_variant_match  ->
        Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp

let _ = t_of_sexp
let sexp_of_t : t -> Sexplib.Sexp.t =
  function
  | `A v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "A"; sexp_of_list sexp_of_value v0]
  | `O v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "O";
        sexp_of_list
          (function
           | (v0,v1) ->
               let v0 = sexp_of_string v0

               and v1 = sexp_of_value v1
                in Sexplib.Sexp.List [v0; v1]) v0]

let _ = sexp_of_t
let (hash_fold_t :
  Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  fun hsv  ->
    fun arg  ->
      match arg with
      | `A _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 65  in
          hash_fold_list hash_fold_value hsv _v
      | `O _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 79  in
          hash_fold_list
            (fun hsv  ->
               fun arg  ->
                 let (e0,e1) = arg  in
                 let hsv = hash_fold_string hsv e0  in
                 let hsv = hash_fold_value hsv e1  in hsv) hsv _v

let _ = hash_fold_t
let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_t hsv arg)
     in
  fun x  -> func x
let _ = hash
[@@@deriving.end]
