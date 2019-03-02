open Base
open Ppxlib
open Ast_builder.Default

let extension_name = "csv"
let unsupported_type_error_msg ~name =
  Printf.sprintf "The type %s is not natively supported in the csv camlp4 extension" name

let useless_merge_recursive _log ~field_name:_ ~tp:_ ast = ast

let edot ~loc path_opt id =
  pexp_ident ~loc
    (Located.mk ~loc
     (match path_opt with
       | None   -> Longident.Lident id
       | Some p -> Longident.Ldot (p, id)))

(** Generate the list of fields contained in a flattened record type *)
module Rev_headers =
  Ppx_conv_func.Of_simple (struct
    let unsupported_type_error_msg = unsupported_type_error_msg
    let conversion_name = extension_name
    let function_name = function
      | None -> "rev_csv_header'"
      | Some param -> Printf.sprintf "rev_csv_header_of_%s'" param

    let atoms loc ~field_name =
      [%expr fun acc _ -> [%e estring ~loc field_name] :: acc]

    let merge_recursive = useless_merge_recursive

    let recursive loc ~field_name ~type_name:_ ~path =
      let tns = function_name None in
      let recursive = edot ~loc path tns in
      let is_csv_atom  = edot ~loc path "is_csv_atom" in
      [%expr
        fun acc _ ->
          if [%e is_csv_atom] then
            [%e estring ~loc field_name] :: acc
          else
            [%e recursive] acc () ()
      ]
  end)

(* Generate the specification of the headers as a tree. This is useful to generate headers
consisting of multiple rows, each field grouping those below. *)
module Spec_of_headers =
  Ppx_conv_func.Of_simple (struct
    let unsupported_type_error_msg = unsupported_type_error_msg
    let conversion_name = extension_name
    let function_name = function
      | None ->  "rev_csv_header_spec'"
      | Some param -> Printf.sprintf "rev_csv_header_spec_of_%s'" param

    let atoms loc ~field_name =
      [%expr fun acc _ -> (Csvfields.Csv.Spec.Leaf [%e estring ~loc field_name]) :: acc]

    let merge_recursive = useless_merge_recursive

    let recursive loc ~field_name ~type_name:_ ~path =
      let tns = function_name None in
      let recursive = edot ~loc path tns in
      let is_csv_atom = edot ~loc path "is_csv_atom" in
      [%expr
        fun acc _ ->
          if [%e is_csv_atom] then
            (Csvfields.Csv.Spec.Leaf [%e estring ~loc field_name]) :: acc
          else
            (Csvfields.Csv.Spec.Tree ([%e estring ~loc field_name],
                                      [%e recursive] [] () ()))
            :: acc
      ]
  end)

(** Generate the some type using a csv row (a list of strings) *)
module Type_of_csv_row =
  Ppx_conv_func.Of_complete (struct
    let unsupported_type_error_msg = unsupported_type_error_msg
    let conversion_name = extension_name
    let function_name = function
      | None -> failwith "Csv conversion of_row requires some name"
      | Some param -> Printf.sprintf "%s_of_row'" param

    let unit loc ~field_name:_      = [%expr  Csvfields.Csv.unit_of_row ]
    let bool loc ~field_name:_      = [%expr  Csvfields.Csv.bool_of_row ]
    let string loc ~field_name:_    = [%expr  Csvfields.Csv.string_of_row ]
    let char loc ~field_name:_      = [%expr  Csvfields.Csv.char_of_row ]
    let int loc ~field_name:_       = [%expr  Csvfields.Csv.int_of_row ]
    let float loc ~field_name:_     = [%expr  Csvfields.Csv.float_of_row ]
    let int32 loc ~field_name:_     = [%expr  Csvfields.Csv.int32_of_row ]
    let int64 loc ~field_name:_     = [%expr  Csvfields.Csv.int64_of_row ]
    let nativeint loc ~field_name:_ = [%expr  Csvfields.Csv.nativeint_of_row ]
    let big_int loc ~field_name:_   = [%expr  Csvfields.Csv.big_int_of_row ]
    let nat loc ~field_name:_       = [%expr  Csvfields.Csv.nat_of_row ]
    let num loc ~field_name:_       = [%expr  Csvfields.Csv.num_of_row ]
    let ratio loc ~field_name:_     = [%expr  Csvfields.Csv.ratio_of_row ]
    let list loc ~field_name:_      = Ppx_conv_func.raise_unsupported ~loc "list"
    let array loc ~field_name:_     = Ppx_conv_func.raise_unsupported ~loc "list"
    let option loc ~field_name:_    = Ppx_conv_func.raise_unsupported ~loc "option"
    let lazy_t loc ~field_name:_    = Ppx_conv_func.raise_unsupported ~loc "lazy_t"
    let ref loc ~field_name:_       = Ppx_conv_func.raise_unsupported ~loc "ref"
    let merge_recursive = useless_merge_recursive
    let recursive loc ~field_name:_ ~type_name ~path =
      let tns = function_name (Some type_name) in
      edot ~loc path tns
  end)

module type B = sig
  val writer   : Location.t -> arg_label * expression
  val is_first : Location.t -> arg_label * expression
  val is_last  : Location.t -> arg_label * expression
end

module Make_row_of (S : B) = struct
  let unsupported_type_error_msg = unsupported_type_error_msg
  let conversion_name = extension_name

  let function_name = function
    | None -> failwith "Csv conversion write_row_of_ requires some name"
    | Some param -> Printf.sprintf "write_row_of_%s'" param

  let add_arguments expr loc =
    pexp_apply ~loc expr
      [ S.is_first loc
      ; S.is_last  loc
      ; S.writer loc
      ]
  ;;
  let unit loc ~field_name:_ =
    add_arguments ([%expr  Csvfields.Csv.row_of_unit ]) loc
  let bool loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_bool ] loc
  let string loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_string ] loc
  let char loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_char ] loc
  let int loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_int ] loc
  let float loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_float ] loc
  let int32 loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_int32 ] loc
  let int64 loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_int64 ] loc
  let nativeint loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_nativeint ] loc
  let big_int loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_big_int ] loc
  let nat loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_nat ] loc
  let num loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_num ] loc
  let ratio loc ~field_name:_ =
    add_arguments [%expr  Csvfields.Csv.row_of_ratio ] loc
  let merge_recursive = useless_merge_recursive
  let recursive loc ~field_name:_ ~type_name ~path =
    let tns = function_name (Some type_name) in
    add_arguments (edot ~loc path tns) loc
  let list loc ~field_name:_ = Ppx_conv_func.raise_unsupported ~loc "list"
  let array loc ~field_name:_ = Ppx_conv_func.raise_unsupported ~loc "array"
  let option loc ~field_name:_ = Ppx_conv_func.raise_unsupported ~loc "option"
  let lazy_t loc ~field_name:_ = Ppx_conv_func.raise_unsupported ~loc "lazy_t"
  let ref loc ~field_name:_ = Ppx_conv_func.raise_unsupported ~loc "ref"
end

let falseexpr loc = [%expr false]

module Unique_row_of =
  Ppx_conv_func.Of_complete (Make_row_of (struct
    let writer   loc = (Labelled "writer"  , [%expr writer   ])
    let is_first loc = (Labelled "is_first", [%expr is_first ])
    let is_last  loc = (Labelled "is_last" , [%expr is_last  ])
end))

module First_row_of =
  Ppx_conv_func.Of_complete (Make_row_of (struct
    let writer   loc = (Labelled "writer"  , [%expr writer   ])
    let is_first loc = (Labelled "is_first", [%expr is_first ])
    let is_last  loc = (Labelled "is_last" , falseexpr loc    )
  end))

module Middle_row_of =
  Ppx_conv_func.Of_complete (Make_row_of (struct
    let writer   loc = (Labelled "writer"  , [%expr writer ])
    let is_first loc = (Labelled "is_first", falseexpr loc  )
    let is_last  loc = (Labelled "is_last" , falseexpr loc  )
  end))

module Last_row_of =
  Ppx_conv_func.Of_complete (Make_row_of (struct
    let writer   loc = (Labelled "writer"  , [%expr writer ])
    let is_first loc = (Labelled "is_first", falseexpr loc  )
    let is_last  loc = (Labelled "is_last" , [%expr is_last  ])
  end))

let csv_record_sig loc ~record_name =
  let st =
    psig_include ~loc
      (include_infos ~loc
         (pmty_with ~loc (pmty_ident ~loc (Located.lident ~loc "Csvfields.Csv.Csvable"))
            [ Pwith_typesubst
                (Located.lident ~loc "t",
                 type_declaration ~loc ~name:(Located.mk ~loc "t")
                   ~params:[]
                   ~manifest:(Some
                                (ptyp_constr ~loc (Located.lident ~loc record_name) []))
                   ~cstrs:[]
                   ~kind:Ptype_abstract
                   ~private_:Public
                )
            ]))
  in
  [st]
;;

let rev_csv_header' ~lds loc =
  let name = [%pat? rev_csv_header' ] in
  let conversion_of_type = Rev_headers.conversion_of_type in
  Ppx_conv_func.Gen_struct.generate_using_fold
    ~pass_acc:true ~pass_anonymous:true ~conversion_of_type ~name ~lds loc

let rev_csv_header_spec' ~lds loc =
  let name = [%pat?  rev_csv_header_spec' ] in
  let conversion_of_type = Spec_of_headers.conversion_of_type in
  Ppx_conv_func.Gen_struct.generate_using_fold
  ~pass_acc:true ~pass_anonymous:true ~conversion_of_type ~name ~lds loc

let row_of_t' ~record_name ~lds loc =
  let init = [%expr  Fields.Direct.iter t ] in
  let body =
    Ppx_conv_func.Gen_struct.make_body ~lds ~init loc
      ~unique_f:Unique_row_of.conversion_of_type
      ~first_f:First_row_of.conversion_of_type
      ~last_f:Last_row_of.conversion_of_type
      Middle_row_of.conversion_of_type
  in
  let anonymous = Ppx_conv_func.Gen_struct.anonymous loc in
  let func =
    [%expr
      fun ~is_first ~is_last ~writer [%p anonymous] [%p anonymous] t ->
        [%e body]
    ]
  in
  let name = pvar ~loc ("write_row_of_" ^ record_name ^ "'") in
  [%stri let [%p name] = [%e func] ]

let t_of_row' ~record_name ~lds loc =
  let init = [%expr  Fields.make_creator strings ] in
  let body =
    let f = Type_of_csv_row.conversion_of_type in
    Ppx_conv_func.Gen_struct.make_body ~lds ~init loc f
  in
  let func =
    Ppx_conv_func.lambda loc
      [ Ppx_conv_func.Gen_struct.anonymous loc; [%pat?  strings ] ]
      body
  in
  let name = pvar ~loc (record_name ^ "_of_row'") in
  [%stri let [%p name] = [%e func] ]

let csv_record ~tps:_ ~record_name loc lds =
  let t_of_row' = t_of_row' ~record_name ~lds loc in
  let is_csv_atom = [%stri let is_csv_atom = false ] in
  let row_of_t' = row_of_t' ~record_name ~lds loc in
  let rev_csv_header' = rev_csv_header' ~lds loc in
  let rev_csv_header_spec' = rev_csv_header_spec' ~lds loc in
  let t =
    if String.(<>) record_name "t" then
      [%str type t = [%t ptyp_constr ~loc (Located.lident ~loc record_name) [] ] ]
    else
      [%str
         type _t = t
         type t = _t
      ]
  in
  let with_constraints =
    [ Pwith_typesubst
        (Located.lident ~loc "t",
         type_declaration ~loc ~name:(Located.mk ~loc "t")
           ~manifest:(Some (ptyp_constr ~loc (Located.lident ~loc record_name) []))
           ~kind:Ptype_abstract
           ~private_:Public
           ~params:[]
           ~cstrs:[]
        )
    ]
  in
  let applied_functor =
    pmod_apply ~loc
      (pmod_ident ~loc (Located.lident ~loc "Csvfields.Csv.Record"))
      (pmod_structure ~loc
         (t @
          [ is_csv_atom
          ; rev_csv_header'
          ; rev_csv_header_spec'
          ; t_of_row'
          ; row_of_t'
          ]))
  in
  let st =
    pstr_include ~loc
      (include_infos ~loc
         (pmod_constraint ~loc
            applied_functor
            (pmty_with ~loc
               (pmty_ident ~loc (Located.lident ~loc "Csvfields.Csv.Csvable"))
               with_constraints)))
  in
  [st]
;;

let csv =
  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.empty
      (Ppx_conv_func.Gen_struct.generate
         ~extension_name
         ~record:csv_record)
      ~deps:[Ppx_fields_conv.fields]
  in
  let sig_type_decl =
    Deriving.Generator.make
      Deriving.Args.empty
      (Ppx_conv_func.Gen_sig.generate
         ~extension_name
         ~nil:   (fun ~tps:_ ~record_name loc     -> csv_record_sig loc ~record_name)
         ~record:(fun ~tps:_ ~record_name loc _   -> csv_record_sig loc ~record_name)
      )
  in
  Deriving.add
    extension_name
    ~str_type_decl
    ~sig_type_decl
;;
