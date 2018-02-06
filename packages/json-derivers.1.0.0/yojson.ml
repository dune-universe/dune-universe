open Import

type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of t list
  | `Null
  | `String of string
  | `Tuple of t list
  | `Variant of string * t option ]

[@@deriving_inline compare, sexp, hash]
let _ = fun (_ : t)  -> ()
let rec compare : t -> t -> int =
  fun a__001_  ->
    fun b__002_  ->
      if Ppx_compare_lib.phys_equal a__001_ b__002_
      then 0
      else
        (match (a__001_, b__002_) with
         | (`Assoc _left__003_,`Assoc _right__004_) ->
             compare_list
               (fun a__005_  ->
                  fun b__006_  ->
                    let (t__007_,t__008_) = a__005_  in
                    let (t__009_,t__010_) = b__006_  in
                    match compare_string t__007_ t__009_ with
                    | 0 -> compare t__008_ t__010_
                    | n -> n) _left__003_ _right__004_
         | (`Bool _left__011_,`Bool _right__012_) ->
             compare_bool _left__011_ _right__012_
         | (`Float _left__013_,`Float _right__014_) ->
             compare_float _left__013_ _right__014_
         | (`Int _left__015_,`Int _right__016_) ->
             compare_int _left__015_ _right__016_
         | (`Intlit _left__017_,`Intlit _right__018_) ->
             compare_string _left__017_ _right__018_
         | (`List _left__019_,`List _right__020_) ->
             compare_list compare _left__019_ _right__020_
         | (`Null,`Null) -> 0
         | (`String _left__023_,`String _right__024_) ->
             compare_string _left__023_ _right__024_
         | (`Tuple _left__025_,`Tuple _right__026_) ->
             compare_list compare _left__025_ _right__026_
         | (`Variant _left__029_,`Variant _right__030_) ->
             let (t__031_,t__032_) = _left__029_  in
             let (t__033_,t__034_) = _right__030_  in
             (match compare_string t__031_ t__033_ with
              | 0 -> compare_option compare t__032_ t__034_
              | n -> n)
         | (x,y) -> Ppx_compare_lib.polymorphic_compare x y)

let _ = compare
let rec __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "yojson.ml.t"  in
  function
  | Sexplib.Sexp.Atom atom as _sexp ->
      (match atom with
       | "Null" -> `Null
       | "Assoc" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Bool" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Float" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Int" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Intlit" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "List" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "String" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Tuple" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | "Variant" -> Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom atom)::sexp_args) as _sexp ->
      (match atom with
       | "Assoc" as _tag ->
           (match sexp_args with
            | v0::[] ->
                let v0 =
                  list_of_sexp
                    (function
                     | Sexplib.Sexp.List (v0::v1::[]) ->
                         let v0 = string_of_sexp v0

                         and v1 = t_of_sexp v1
                          in (v0, v1)
                     | sexp ->
                         Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
                           2 sexp) v0
                   in
                `Assoc v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
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
       | "Int" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = int_of_sexp v0  in `Int v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Intlit" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = string_of_sexp v0  in `Intlit v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "List" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = list_of_sexp t_of_sexp v0  in `List v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "String" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = string_of_sexp v0  in `String v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Tuple" as _tag ->
           (match sexp_args with
            | v0::[] -> let v0 = list_of_sexp t_of_sexp v0  in `Tuple v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Variant" as _tag ->
           (match sexp_args with
            | v0::[] ->
                let v0 =
                  match v0 with
                  | Sexplib.Sexp.List (v0::v1::[]) ->
                      let v0 = string_of_sexp v0

                      and v1 = option_of_sexp t_of_sexp v1
                       in (v0, v1)
                  | sexp ->
                      Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc 2
                        sexp
                   in
                `Variant v0
            | _ ->
                Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
       | "Null" -> Sexplib.Conv_error.ptag_no_args _tp_loc _sexp
       | _ -> Sexplib.Conv_error.no_variant_match ())
  | Sexplib.Sexp.List ((Sexplib.Sexp.List _)::_) as sexp ->
      Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp
  | Sexplib.Sexp.List [] as sexp ->
      Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp

and t_of_sexp : Sexplib.Sexp.t -> t =
  let _tp_loc = "yojson.ml.t"  in
  fun sexp  ->
    try __t_of_sexp__ sexp
    with
    | Sexplib.Conv_error.No_variant_match  ->
        Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp

let _ = __t_of_sexp__

and _ = t_of_sexp

let rec sexp_of_t : t -> Sexplib.Sexp.t =
  function
  | `Assoc v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "Assoc";
        sexp_of_list
          (function
           | (v0,v1) ->
               let v0 = sexp_of_string v0

               and v1 = sexp_of_t v1
                in Sexplib.Sexp.List [v0; v1]) v0]
  | `Bool v0 -> Sexplib.Sexp.List [Sexplib.Sexp.Atom "Bool"; sexp_of_bool v0]
  | `Float v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "Float"; sexp_of_float v0]
  | `Int v0 -> Sexplib.Sexp.List [Sexplib.Sexp.Atom "Int"; sexp_of_int v0]
  | `Intlit v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "Intlit"; sexp_of_string v0]
  | `List v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "List"; sexp_of_list sexp_of_t v0]
  | `Null -> Sexplib.Sexp.Atom "Null"
  | `String v0 ->
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "String"; sexp_of_string v0]
  | `Tuple v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "Tuple"; sexp_of_list sexp_of_t v0]
  | `Variant v0 ->
      Sexplib.Sexp.List
        [Sexplib.Sexp.Atom "Variant";
        (let (v0,v1) = v0  in
         let v0 = sexp_of_string v0

         and v1 = sexp_of_option sexp_of_t v1
          in Sexplib.Sexp.List [v0; v1])]

let _ = sexp_of_t
let rec (hash_fold_t :
  Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  fun hsv  ->
    fun arg  ->
      match arg with
      | `Assoc _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 963043957  in
          hash_fold_list
            (fun hsv  ->
               fun arg  ->
                 let (e0,e1) = arg  in
                 let hsv = hash_fold_string hsv e0  in
                 let hsv = hash_fold_t hsv e1  in hsv) hsv _v
      | `Bool _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 737456202  in
          hash_fold_bool hsv _v
      | `Float _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 365180284  in
          hash_fold_float hsv _v
      | `Int _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 3654863  in
          hash_fold_int hsv _v
      | `Intlit _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv (-752863768)  in
          hash_fold_string hsv _v
      | `List _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 848054398  in
          hash_fold_list hash_fold_t hsv _v
      | `Null -> Ppx_hash_lib.Std.Hash.fold_int hsv 870828711
      | `String _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv (-976970511)  in
          hash_fold_string hsv _v
      | `Tuple _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 726928360  in
          hash_fold_list hash_fold_t hsv _v
      | `Variant _v ->
          let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 708012133  in
          let (e0,e1) = _v  in
          let hsv = hash_fold_string hsv e0  in
          let hsv = hash_fold_option hash_fold_t hsv e1  in hsv

and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_t hsv arg)
     in
  fun x  -> func x

let _ = hash_fold_t

and _ = hash

[@@@deriving.end]
