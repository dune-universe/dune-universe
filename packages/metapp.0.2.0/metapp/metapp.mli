(** {1 String constant destructor} *)

type string_constant = {
    s : string;
    loc : Location.t;
    delim : string option;
  }

val destruct_string_constant : Parsetree.constant -> string_constant option

(** {1 Coercions} *)

val int_of_expression : Parsetree.expression -> int

val string_of_expression : Parsetree.expression -> string_constant

val string_of_arbitrary_expression : Parsetree.expression -> string

val bool_of_expression : Parsetree.expression -> bool

val pair_of_expression :
    Parsetree.expression -> Parsetree.expression * Parsetree.expression

val list_of_expression : Parsetree.expression -> Parsetree.expression list

val list_of_tuple : Parsetree.expression -> Parsetree.expression list

val structure_of_expression : Parsetree.expression -> Parsetree.structure

val lid_of_str : Ast_helper.str -> Ast_helper.lid

val sequence : Parsetree.expression list -> Parsetree.expression

(** {1 Payload construction and extraction} *)

val int_of_payload : Parsetree.payload -> int

val payload_of_int : int -> Parsetree.payload

val string_of_payload : Parsetree.payload -> string

val bool_of_payload : Parsetree.payload -> bool

(** {1 Location management} *)

val mkloc : 'a -> 'a Location.loc

val map_loc : ('a -> 'b) -> 'a Location.loc -> 'b Location.loc

val with_loc : ('a -> 'b) -> 'a Location.loc -> 'b

(** {1 Longident } *)

type 'a comparer = 'a -> 'a -> int

module Longident : sig
  type t = Longident.t

  val compare : t comparer

  val equal : t -> t -> bool

  val hash : t -> int

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val make : ?prefix : t -> string -> t

  val concat : Longident.t -> t -> Longident.t

  val of_module_expr_opt : Parsetree.module_expr -> Longident.t option

  val of_expression_opt : Parsetree.expression -> Longident.t option

  val of_payload_opt : Parsetree.payload -> Longident.t option

  val of_payload : Parsetree.payload -> Longident.t
end

val mklid : ?prefix : Longident.t -> string -> Ast_helper.lid

(** {1 Constructing function application} *)

val nolabel : 'a -> Asttypes.arg_label * 'a

val nolabels : 'a list -> (Asttypes.arg_label * 'a) list

val apply :
    ?attrs : Parsetree.attributes -> Parsetree.expression ->
      ?labels : (string * Parsetree.expression) list ->
        Parsetree.expression list -> Parsetree.expression

(** {1 Generic signature for visitable nodes} *)

type 'a iterator_item = Ast_iterator.iterator -> 'a -> unit

type 'a mapper_item = Ast_mapper.mapper -> 'a -> 'a

type ('cell, 'contents) accessor = {
    get : 'cell -> 'contents;
    set : 'contents -> 'cell -> 'cell;
  }

module type VisitableS = sig
  type t

  val to_loc : t -> Location.t

  val iterator : (Ast_iterator.iterator, t iterator_item) accessor

  val mapper : (Ast_mapper.mapper, t mapper_item) accessor
end

(** {1 Generic signature for extensible nodes} *)

module type ExtensibleS = sig
  include VisitableS

  val extension : ?attrs:Parsetree.attributes -> Parsetree.extension -> t

  val destruct_extension : t -> Parsetree.extension option
end

module type PayloadS = sig
  type t

  val of_payload : Parsetree.payload -> t

  val to_payload : t -> Parsetree.payload
end

module type ItemS = sig
  include ExtensibleS

  include PayloadS with type t := t

  val of_list : t list -> t
end

module Cty : ExtensibleS with type t = Parsetree.class_type

module Ctf : ExtensibleS with type t = Parsetree.class_type_field

module Cl : ExtensibleS with type t = Parsetree.class_expr

module Cf : ExtensibleS with type t = Parsetree.class_field

module Stri : ItemS with type t := Parsetree.structure_item

module Str : sig
  include VisitableS with type t = Parsetree.structure

  include PayloadS with type t := Parsetree.structure
end

module Sigi : ItemS with type t := Parsetree.signature_item

module Sig : sig
  include VisitableS with type t = Parsetree.signature

  include PayloadS with type t := Parsetree.signature
end

(** {1 Module expressions} *)

[%%meta if Sys.ocaml_version >= "4.10.0" then [%sigi:
  type functor_parameter = Parsetree.functor_parameter =
    | Unit
    | Named of string option Location.loc * Parsetree.module_type]
else [%sigi:
  type functor_parameter =
    | Unit
    | Named of string option Location.loc * Parsetree.module_type]]

module type FunctorS = sig
  type t

  val functor_ :
      ?loc:Location.t -> ?attrs:Parsetree.attributes -> functor_parameter ->
        t -> t

  val destruct_functor : t -> (functor_parameter * t) option
end

module type ModS = sig
  include ExtensibleS

  include FunctorS with type t := t
end

module Mod : ModS with type t = Parsetree.module_expr

(** {1 Module types} *)

module Mty : ModS with type t = Parsetree.module_type

module Types : sig
  (** {1 Signature type destruction} *)

  [%%meta if Sys.ocaml_version >= "4.08.0" then [%sigi:
    type visibility = Types.visibility =
      | Exported
      | Hidden]
  else [%sigi:
    type visibility =
      | Exported
      | Hidden]]

  module Sigi : sig
    type sig_type = {
        id : Ident.t;
        decl : Types.type_declaration;
        rec_status : Types.rec_status;
        visibility : visibility;
      }

    val sig_type : sig_type -> Types.signature_item

    val destruct_sig_type : Types.signature_item -> sig_type option
  end


  (** {1 Module types in Types} *)

  [%%meta if Sys.ocaml_version >= "4.10.0" then [%sigi:
    type functor_parameter = Types.functor_parameter =
      | Unit
      | Named of Ident.t option * Types.module_type]
  else [%sigi:
    type functor_parameter =
      | Unit
      | Named of Ident.t option * Types.module_type]]

  module Mty : sig
    val functor_ : functor_parameter -> Types.module_type -> Types.module_type

    val destruct_functor :
        Types.module_type -> (functor_parameter * Types.module_type) option

    val destruct_alias : Types.module_type -> Path.t option
  end
end

(** {1 Generic signature for expressions and patterns} *)

module type ValueS = sig
  include ExtensibleS

  val var : ?attrs:Parsetree.attributes -> string -> t

  val of_constant : ?attrs:Parsetree.attributes -> Parsetree.constant -> t

  val of_bytes : ?attrs:Parsetree.attributes -> bytes -> t

  val force_tuple : ?attrs:Parsetree.attributes -> t list -> t

  val force_construct :
      ?attrs:Parsetree.attributes -> Ast_helper.lid -> t option -> t

  val array : ?attrs:Parsetree.attributes -> t list -> t

  val record : ?attrs:Parsetree.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Parsetree.attributes -> string -> t option -> t

  val lazy_ :  ?attrs:Parsetree.attributes -> t -> t

  val choice :
      (unit -> Parsetree.expression) -> (unit -> Parsetree.pattern) -> t

  include PayloadS with type t := t

  val of_int : ?attrs:Parsetree.attributes -> int -> t

  val of_string : ?attrs:Parsetree.attributes -> string -> t

  val of_char : ?attrs:Parsetree.attributes -> char -> t

  val of_unit : ?attrs:Parsetree.attributes -> unit -> t

  val of_bool : ?attrs:Parsetree.attributes -> bool -> t

  val of_float : ?attrs:Parsetree.attributes -> float -> t

  val of_int32 : ?attrs:Parsetree.attributes -> int32 -> t

  val of_int64 : ?attrs:Parsetree.attributes -> int64 -> t

  val of_nativeint : ?attrs:Parsetree.attributes -> nativeint -> t

  val none : ?attrs:Parsetree.attributes -> unit -> t

  val some : ?attrs:Parsetree.attributes -> t -> t

  val option : ?attrs:Parsetree.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Parsetree.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Parsetree.attributes -> t list -> t

  val nil : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t -> t -> t

  val list :
      ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t list -> t
end

module Pat : ValueS with type t = Parsetree.pattern

type value = {
    exp : Parsetree.expression;
    pat : Parsetree.pattern;
  }

module Value : ValueS with type t = value

(** {1 Attribute management} *)

module Attr : sig
  val mk : Ast_helper.str -> Parsetree.payload -> Parsetree.attribute

  val name : Parsetree.attribute -> Ast_helper.str

  val payload : Parsetree.attribute -> Parsetree.payload

  val to_loc : Parsetree.attribute -> Location.t

  val find : string -> Parsetree.attributes -> Parsetree.attribute option

  val chop :
      string -> Parsetree.attributes ->
        (Parsetree.attribute * Parsetree.attributes) option

  val get_derivers : Parsetree.attributes -> Parsetree.expression list option

  val has_deriver :
      string -> Parsetree.attributes ->
        (Asttypes.arg_label * Parsetree.expression) list option
end

(** {1 Module binding and declaration} *)

[%%meta Metapp_preutils.Sigi.of_list (
  if Sys.ocaml_version >= "4.10.0" then [%sig:
    type module_name = string option

    external module_name_of_string_option : string option -> module_name =
      "%identity"

    external string_option_of_module_name : module_name -> string option =
      "%identity"]
  else [%sig:
    type module_name = string

    val module_name_of_string_option : string option -> module_name

    val string_option_of_module_name : module_name -> string option
  ])]

module Md : sig
  val mk :
      ?loc:Location.t -> ?attrs:Parsetree.attributes ->
      string option Location.loc -> Parsetree.module_type ->
      Parsetree.module_declaration
end

module Mb : sig
  val mk :
      ?loc:Location.t -> ?attrs:Parsetree.attributes ->
      string option Location.loc -> Parsetree.module_expr ->
      Parsetree.module_binding
end

(** {1 Mapper for [[@if bool]] notation} *)

val filter : Ast_mapper.mapper

(** {1 Type construction} *)

module Typ : sig
  include ExtensibleS with type t = Parsetree.core_type

  include PayloadS with type t := Parsetree.core_type

  val poly : Ast_helper.str list -> Parsetree.core_type -> Parsetree.core_type

  val poly_name :
    [%meta if Sys.ocaml_version >= "4.05.0" then [%t:
      Ast_helper.str -> string]
    else [%t:
      string -> string]]
end

(** {1 Type declarations} *)

module Type : sig
  val has_deriver :
      string -> Parsetree.type_declaration list ->
        (Asttypes.arg_label * Parsetree.expression) list option
end

(** {1 Open} *)

module Opn : sig
  [%%meta if Sys.ocaml_version >= "4.08.0" then
    [%sigi: type 'a t = 'a Parsetree.open_infos]
  else [%sigi:
    type 'a t = {
      popen_expr : 'a;
      popen_override : Asttypes.override_flag;
      popen_loc : Location.t;
      popen_attributes : Parsetree.attributes;
    }]]
end

(** {1 Expressions} *)

module Exp : sig
  include ValueS with type t = Parsetree.expression

  val ident :
      ?loc:Location.t -> ?attrs:Parsetree.attributes -> Longident.t ->
        Parsetree.expression

  val ident_of_str :
      ?attrs:Parsetree.attributes -> Ast_helper.str ->
        Parsetree.expression

  val send :
      ?loc:Location.t -> ?attrs:Parsetree.attributes -> Parsetree.expression ->
        Ast_helper.str -> Parsetree.expression

  val newtype :
      ?loc:Location.t -> ?attrs:Parsetree.attributes -> Ast_helper.str ->
        Parsetree.expression -> Parsetree.expression

  val open_ :
      ?loc:Location.t -> ?attrs:Parsetree.attributes ->
        Parsetree.module_expr Opn.t -> Parsetree.expression ->
        Parsetree.expression

  val destruct_open :
      Parsetree.expression ->
        (Parsetree.module_expr Opn.t * Parsetree.expression) option

  val tuple_of_payload : Parsetree.payload -> Parsetree.expression list
end

(** {1 Row fields} *)

(** The module `Rf` has been introduced in [Ast_helper] with OCaml 4.08.0
    (as [row_field] type switched to a record representation).
    Moreover, even the current [Ast_helper.Rf.inherit_] (OCaml 4.10.0) misses
    the [?attrs] flag. *)
module Rf : sig
  type desc =
    | Rtag of Asttypes.label Location.loc * bool * Parsetree.core_type list
    | Rinherit of Parsetree.core_type

  val to_loc : Parsetree.row_field -> Location.t

  val to_attributes : Parsetree.row_field -> Parsetree.attributes

  val destruct : Parsetree.row_field -> desc

  val tag :
    ?loc:Location.t -> ?attrs:Parsetree.attributes ->
    Asttypes.label Location.loc -> bool -> Parsetree.core_type list ->
    Parsetree.row_field

  val inherit_ :
    ?loc:Location.t -> ?attrs:Parsetree.attributes -> Parsetree.core_type ->
    Parsetree.row_field
end

(** {1 Object fields} *)

module Of : sig
  [%%meta if Sys.ocaml_version >= "4.06.0" then [%sigi:
    type t = Parsetree.object_field]
  else if Sys.ocaml_version >= "4.05.0" then [%sigi:
    type t =
      Asttypes.label Location.loc * Parsetree.attributes * Parsetree.core_type]
  else [%sigi:
    type t = Asttypes.label * Parsetree.attributes * Parsetree.core_type]]

  type desc =
    | Otag of Asttypes.label Location.loc * Parsetree.core_type
    | Oinherit of Parsetree.core_type

  val to_loc : t -> Location.t

  val to_attributes : t -> Parsetree.attributes

  val destruct : t -> desc

  val tag : ?loc:Location.t -> ?attrs:Parsetree.attributes ->
    Asttypes.label Location.loc -> Parsetree.core_type -> t

  val inherit_ : ?loc:Location.t -> ?attrs:Parsetree.attributes ->
    Parsetree.core_type -> t
end

(** {1 With constraint} *)

module With : sig
  val typesubst :
      ?t:Ast_helper.lid -> Parsetree.type_declaration ->
        Parsetree.with_constraint

  val destruct_typesubst :
      Parsetree.with_constraint ->
        (Ast_helper.lid * Parsetree.type_declaration) option

  val modsubst :
      Ast_helper.lid -> Ast_helper.lid -> Parsetree.with_constraint

  val destruct_modsubst :
      Parsetree.with_constraint -> (Ast_helper.lid * Ast_helper.lid) option
end

(** {1 General purpose functions} *)

val compare_pair : 'a comparer -> 'b comparer -> ('a * 'b) comparer

val compare_list : 'a comparer -> 'a list comparer

val update : ('a -> 'b * 'a) ->  'a ref -> 'b

val mutate : ('a -> 'a) -> 'a ref -> unit

val extract_first : ('a -> 'b option) -> 'a list -> ('b * 'a list) option

(** Indexed accumulator to build an array. *)
module Accu : sig
  type 'a t
  (** An accumulator of type ['a t] can accumulate values of type ['a] to
      build an ['a array]. *)

  val empty : 'a t
  (** The empty accumulator. *)

  val add : 'a -> 'a t -> int * 'a t
  (** [add v a] returns [(i, a')] where [a'] is the accumulator [a] followed by
      the value [v]. [i] is the index of [v] in [a'] (and is equal to the length
      of [a]). *)

  val length : 'a t -> int
  (** [length a] returns the length of the accumulator [a] (the number of
      elements). *)

  val to_array : 'a t -> 'a array
  (** [to_array a] returns the array containing all the elements of [a]. *)
end
