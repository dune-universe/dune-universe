module Types =
  struct
    module Stdlib = struct include Stdlib let ref = GT.ref end
    (* TODO: ise ppx_import here *)
    type type_expr = Types.type_expr = {
      mutable desc : Types.type_desc;
      mutable level : int;
      mutable scope : int option;
      id : int;
    }[@@deriving gt]
    and row_desc = Types.row_desc =
      {
      row_fields: (Asttypes.label * row_field) list ;
      row_more: type_expr ;
      row_bound: unit ;
      row_closed: bool ;
      row_fixed: bool ;
      row_name: (Path.t * type_expr list) option }[@@deriving gt]
    and type_desc = Types.type_desc =
      | Tvar of string option 
      | Tarrow of Asttypes.arg_label * type_expr * type_expr * commutable 
      | Ttuple of type_expr list 
      | Tconstr of Path.t * type_expr list * abbrev_memo Pervasives.ref 
      | Tobject of type_expr * (Path.t * type_expr list) option
      Pervasives.ref 
      | Tfield of string * field_kind * type_expr * type_expr 
      | Tnil 
      | Tlink of type_expr 
      | Tsubst of type_expr 
      | Tvariant of row_desc 
      | Tunivar of string option 
      | Tpoly of type_expr * type_expr list 
      | Tpackage of Path.t * Longident.t list * type_expr list [@@deriving
                                                                 gt]
    and row_field = Types.row_field =
      | Rpresent of type_expr option 
      | Reither of bool * type_expr list * bool * row_field option
      Pervasives.ref 
      | Rabsent [@@deriving gt]
    and abbrev_memo = Types.abbrev_memo =
      | Mnil 
      | Mcons of Asttypes.private_flag * Path.t * type_expr * type_expr *
      abbrev_memo 
      | Mlink of abbrev_memo Pervasives.ref [@@deriving gt]
    and field_kind = Types.field_kind =
      | Fvar of field_kind option Pervasives.ref 
      | Fpresent 
      | Fabsent [@@deriving gt]
    and commutable = Types.commutable =
      | Cok 
      | Cunknown 
      | Clink of commutable Pervasives.ref [@@deriving gt]
  end
