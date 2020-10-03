(** {1 String constant destructor} *)

type string_constant = {
    s : string;
    loc : Location.t;
    delim : string option;
  }

val destruct_string_constant : Ppxlib.constant -> string_constant option

(** {1 Coercions} *)

val int_of_expression : Ppxlib.expression -> int

val string_of_expression : Ppxlib.expression -> string_constant

val string_of_arbitrary_expression : Ppxlib.expression -> string

val bool_of_expression : Ppxlib.expression -> bool

val pair_of_expression :
    Ppxlib.expression -> Ppxlib.expression * Ppxlib.expression

val list_of_expression : Ppxlib.expression -> Ppxlib.expression list

val list_of_tuple : Ppxlib.expression -> Ppxlib.expression list

val structure_of_expression : Ppxlib.expression -> Ppxlib.structure

val lid_of_str : Ast_helper.str -> Ast_helper.lid

val sequence : Ppxlib.expression list -> Ppxlib.expression

(** {1 Payload construction and extraction} *)

val int_of_payload : Ppxlib.payload -> int

val payload_of_int : int -> Ppxlib.payload

val string_of_payload : Ppxlib.payload -> string

val bool_of_payload : Ppxlib.payload -> bool

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

  val of_module_expr_opt : Ppxlib.module_expr -> Longident.t option

  val of_expression_opt : Ppxlib.expression -> Longident.t option

  val of_payload_opt : Ppxlib.payload -> Longident.t option

  val of_payload : Ppxlib.payload -> Longident.t
end

val mklid : ?prefix : Longident.t -> string -> Ast_helper.lid

(** {1 Constructing function application} *)

val nolabel : 'a -> Ppxlib.Asttypes.arg_label * 'a

val nolabels : 'a list -> (Ppxlib.Asttypes.arg_label * 'a) list

val apply :
    ?attrs : Ppxlib.attributes -> Ppxlib.expression ->
      ?labels : (string * Ppxlib.expression) list ->
        ?optional : (string * Ppxlib.expression) list ->
          Ppxlib.expression list -> Ppxlib.expression

(** {1 Generic signature for visitable nodes} *)

type 'a iter = 'a -> unit

type 'a map = 'a -> 'a

module type VisitableS = sig
  type t

  val to_loc : t -> Location.t

  val iter : #Ppxlib.Ast_traverse.iter -> t iter

  val map : #Ppxlib.Ast_traverse.map -> t map
end

(** {1 Generic signature for extensible nodes} *)

type destruct_extension = Ppxlib.extension * Ppxlib.attributes

module type ExtensibleS = sig
  include VisitableS

  val extension : ?attrs:Ppxlib.attributes -> Ppxlib.extension -> t

  val destruct_extension : t -> destruct_extension option
end

module type PayloadS = sig
  type t

  val of_payload : Ppxlib.payload -> t

  val to_payload : t -> Ppxlib.payload
end

module type ItemS = sig
  include ExtensibleS

  include PayloadS with type t := t

  val of_list : t list -> t
end

module Cty : ExtensibleS with type t = Ppxlib.class_type

module Ctf : ExtensibleS with type t = Ppxlib.class_type_field

module Cl : ExtensibleS with type t = Ppxlib.class_expr

module Cf : ExtensibleS with type t = Ppxlib.class_field

module Stri : ItemS with type t := Ppxlib.structure_item

module Str : sig
  include VisitableS with type t = Ppxlib.structure

  include PayloadS with type t := Ppxlib.structure
end

module Sigi : ItemS with type t := Ppxlib.signature_item

module Sig : sig
  include VisitableS with type t = Ppxlib.signature

  include PayloadS with type t := Ppxlib.signature
end

(** {1 Module expressions} *)

type functor_parameter = Ppxlib.functor_parameter =
  | Unit
  | Named of string option Location.loc * Ppxlib.module_type

module type FunctorS = sig
  type t

  val functor_ :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes -> functor_parameter ->
        t -> t

  val destruct_functor : t -> (functor_parameter * t) option
end

module type ModS = sig
  include ExtensibleS

  include FunctorS with type t := t
end

module Mod : ModS with type t = Ppxlib.module_expr

(** {1 Module types} *)

module Mty : ModS with type t = Ppxlib.module_type

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

  val var : ?attrs:Ppxlib.attributes -> string -> t

  val of_constant : ?attrs:Ppxlib.attributes -> Ppxlib.constant -> t

  val of_bytes : ?attrs:Ppxlib.attributes -> bytes -> t

  val force_tuple : ?attrs:Ppxlib.attributes -> t list -> t

  val force_construct :
      ?attrs:Ppxlib.attributes -> Ast_helper.lid -> t option -> t

  val array : ?attrs:Ppxlib.attributes -> t list -> t

  val record : ?attrs:Ppxlib.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Ppxlib.attributes -> string -> t option -> t

  val lazy_ :  ?attrs:Ppxlib.attributes -> t -> t

  val choice :
      (unit -> Ppxlib.expression) -> (unit -> Ppxlib.pattern) -> t

  include PayloadS with type t := t

  val of_int : ?attrs:Ppxlib.attributes -> int -> t

  val of_string : ?attrs:Ppxlib.attributes -> string -> t

  val of_char : ?attrs:Ppxlib.attributes -> char -> t

  val of_unit : ?attrs:Ppxlib.attributes -> unit -> t

  val of_bool : ?attrs:Ppxlib.attributes -> bool -> t

  val of_float : ?attrs:Ppxlib.attributes -> float -> t

  val of_int32 : ?attrs:Ppxlib.attributes -> int32 -> t

  val of_int64 : ?attrs:Ppxlib.attributes -> int64 -> t

  val of_nativeint : ?attrs:Ppxlib.attributes -> nativeint -> t

  val none : ?attrs:Ppxlib.attributes -> unit -> t

  val some : ?attrs:Ppxlib.attributes -> t -> t

  val option : ?attrs:Ppxlib.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Ppxlib.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Ppxlib.attributes -> t list -> t

  val nil : ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> t -> t -> t

  val list :
      ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> t list -> t
end

module Pat : ValueS with type t = Ppxlib.pattern

type value = {
    exp : Ppxlib.expression;
    pat : Ppxlib.pattern;
  }

module Value : ValueS with type t = value

(** {1 Attribute management} *)

module Attr : sig
  val mk : Ast_helper.str -> Ppxlib.payload -> Ppxlib.attribute

  val name : Ppxlib.attribute -> Ast_helper.str

  val payload : Ppxlib.attribute -> Ppxlib.payload

  val to_loc : Ppxlib.attribute -> Location.t

  val find : string -> Ppxlib.attributes -> Ppxlib.attribute option

  val chop :
      string -> Ppxlib.attributes ->
        (Ppxlib.attribute * Ppxlib.attributes) option

  val get_derivers : Ppxlib.attributes -> Ppxlib.expression list option

  val has_deriver :
      string -> Ppxlib.attributes ->
        (Ppxlib.Asttypes.arg_label * Ppxlib.expression) list option
end

(** {1 Module binding and declaration} *)

type module_name = string option

external module_name_of_string_option : string option -> module_name =
  "%identity"

external string_option_of_module_name : module_name -> string option =
  "%identity"

module Md : sig
  val mk :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
      string option Location.loc -> Ppxlib.module_type ->
      Ppxlib.module_declaration
end

module Mb : sig
  val mk :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
      string option Location.loc -> Ppxlib.module_expr ->
      Ppxlib.module_binding
end

(** {1 Mapper for [[@if bool]] notation} *)

class filter : Ppxlib.Ast_traverse.map

(** {1 Type construction} *)

module Typ : sig
  include ExtensibleS with type t = Ppxlib.core_type

  include PayloadS with type t := Ppxlib.core_type

  val poly : Ast_helper.str list -> Ppxlib.core_type -> Ppxlib.core_type

  val poly_name : Ast_helper.str -> string
end

(** {1 Type declarations} *)

module Type : sig
  val has_deriver :
      string -> Ppxlib.type_declaration list ->
        (Ppxlib.Asttypes.arg_label * Ppxlib.expression) list option
end

(** {1 Open} *)

module Opn : sig
  type 'a t = 'a Ppxlib.open_infos
end

(** {1 Expressions} *)

module Exp : sig
  include ValueS with type t = Ppxlib.expression

  val ident :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes -> Longident.t ->
        Ppxlib.expression

  val ident_of_str :
      ?attrs:Ppxlib.attributes -> Ast_helper.str ->
        Ppxlib.expression

  val send :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes -> Ppxlib.expression ->
        Ast_helper.str -> Ppxlib.expression

  val newtype :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes -> Ast_helper.str ->
        Ppxlib.expression -> Ppxlib.expression

  val open_ :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
        Ppxlib.module_expr Opn.t -> Ppxlib.expression ->
        Ppxlib.expression

  val destruct_open :
      Ppxlib.expression ->
        (Ppxlib.module_expr Opn.t * Ppxlib.expression) option

  val tuple_of_payload : Ppxlib.payload -> Ppxlib.expression list
end

(** {1 Row fields} *)

(** The module `Rf` has been introduced in [Ast_helper] with OCaml 4.08.0
    (as [row_field] type switched to a record representation).
    Moreover, even the current [Ast_helper.Rf.inherit_] (OCaml 4.10.0) misses
    the [?attrs] flag. *)
module Rf : sig
  type desc =
    | Rtag of Ppxlib.Asttypes.label Location.loc * bool * Ppxlib.core_type list
    | Rinherit of Ppxlib.core_type

  val to_loc : Ppxlib.row_field -> Location.t

  val to_attributes : Ppxlib.row_field -> Ppxlib.attributes

  val destruct : Ppxlib.row_field -> desc

  val tag :
    ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
    Ppxlib.Asttypes.label Location.loc -> bool -> Ppxlib.core_type list ->
    Ppxlib.row_field

  val inherit_ :
    ?loc:Location.t -> ?attrs:Ppxlib.attributes -> Ppxlib.core_type ->
    Ppxlib.row_field
end

(** {1 Object fields} *)

module Of : sig
  type t = Ppxlib.object_field

  type desc =
    | Otag of Ppxlib.Asttypes.label Location.loc * Ppxlib.core_type
    | Oinherit of Ppxlib.core_type

  val to_loc : t -> Location.t

  val to_attributes : t -> Ppxlib.attributes

  val destruct : t -> desc

  val tag : ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
    Ppxlib.Asttypes.label Location.loc -> Ppxlib.core_type -> t

  val inherit_ : ?loc:Location.t -> ?attrs:Ppxlib.attributes ->
    Ppxlib.core_type -> t
end

(** {1 With constraint} *)

module With : sig
  val typesubst :
      ?t:Ast_helper.lid -> Ppxlib.type_declaration ->
        Ppxlib.with_constraint

  val destruct_typesubst :
      Ppxlib.with_constraint ->
        (Ast_helper.lid * Ppxlib.type_declaration) option

  val modsubst :
      Ast_helper.lid -> Ast_helper.lid -> Ppxlib.with_constraint

  val destruct_modsubst :
      Ppxlib.with_constraint -> (Ast_helper.lid * Ast_helper.lid) option
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
