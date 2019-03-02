open Ppxlib

(* DO NOT USE THIS LIBRARY IN NEW CODE!!!

   This was ported from the old camlp4 world but it doesn't follow our ppx guidelines and
   will disappear at some point.
*)

val raise_unsupported : loc:Location.t -> string -> 'a

(** A simple processor that takes a field name and generates an expression for the
contents of that field *)
type simple_processor =
  Location.t
  -> field_name:string
  -> expression

(** The recursive counter-part to the previous. Additional arguments are the type name and
the a path to make recursive calls *)
type recursive_processor =
  Location.t
  -> field_name:string
  -> type_name:string
  -> path:Longident.t option
  -> expression

(** Module types to specify an extension that does something with a record type *)

module type Simple = sig
  (** The simplest of all the extensions can only see atoms (leafs in the conversion type,
  for example a string), and records or recursive types that need to be recursively
  processed *)
  val conversion_name : string
  val function_name : string option -> string
  val merge_recursive :
    Location.t -> field_name:string -> tp:core_type -> expression -> expression
  val unsupported_type_error_msg : name:string -> string
  val atoms : simple_processor
  val recursive : recursive_processor
end

module type Complete = sig
  (** A more complex extension may have a different behavior depending on the contents of
  the field. *)
  val conversion_name : string
  val function_name : string option -> string
  val merge_recursive :
    Location.t -> field_name:string -> tp:core_type -> expression -> expression
  val unsupported_type_error_msg : name:string -> string
  val unit      : simple_processor
  val bool      : simple_processor
  val string    : simple_processor
  val char      : simple_processor
  val int       : simple_processor
  val float     : simple_processor
  val int32     : simple_processor
  val int64     : simple_processor
  val nativeint : simple_processor
  val big_int   : simple_processor
  val nat       : simple_processor
  val num       : simple_processor
  val ratio     : simple_processor
  val list      : simple_processor
  val array     : simple_processor
  val option    : simple_processor
  val ref       : simple_processor
  val lazy_t    : simple_processor
  val recursive : recursive_processor
end

module type Complete_list = sig
  include Complete
  val prepend : Location.t -> expression -> expression
end

module type Matcher = sig
  (** A module that converts a type into another expression that can be used in camlp4
  extensions needs to have the following functions. *)

  val conversion :
    Location.t
    -> field_name:string
    -> id:Longident.t Location.loc
    -> expression

  val conversion_of_type :
    Location.t
    -> field_name:string
    -> field_ty:core_type
    -> expression
end

(** Three basic functors to generate the Matcher from a simple extension specification *)
module Of_simple (S : Simple) : Matcher
module Of_complete (S : Complete) : Matcher
module Of_list (S : Complete_list) : Matcher

val lambda :
  Location.t ->
  pattern list ->
  expression ->
  expression

module Gen_sig : sig
  val generate :
    extension_name:string
    -> nil:(
      tps:core_type list
      -> record_name:string
      -> Location.t
      -> 'new_record_sig)
    -> record:(
      tps:core_type list
      -> record_name:string
      -> Location.t
      -> label_declaration list
      -> 'new_record_sig)
    -> loc:Location.t
    -> path:string
    -> (rec_flag * type_declaration list)
    -> 'new_record_sig
end

module Gen_struct : sig
  val fields :
    label_declaration list ->
      (string * [> `Immutable | `Mutable ] * core_type) list

  val make_body :
    ?unique_f:(Location.t ->
              field_name:string ->
              field_ty:core_type -> expression) ->
    ?first_f:(Location.t ->
              field_name:string ->
              field_ty:core_type -> expression) ->
    ?last_f:(Location.t ->
            field_name:string ->
            field_ty:core_type -> expression) ->
    lds:label_declaration list ->
    init:expression ->
    Location.t ->
    (Location.t ->
    field_name:string ->
    field_ty:core_type -> expression) ->
    expression

  (** Create the anonymous pattern *)
  val anonymous :
    Location.t -> pattern

  val generate_using_fold :
    ?wrap_body:(expression -> expression) ->
    pass_acc:bool ->
    pass_anonymous:bool ->
    conversion_of_type:(
      Location.t ->
      field_name:string ->
      field_ty:core_type ->
      expression) ->
    name:pattern ->
    lds:label_declaration list ->
    Location.t ->
    structure_item

  val generate :
    extension_name:string
    -> record:(
        tps:core_type list
        -> record_name:string
        -> Location.t
        -> label_declaration list
        -> 'new_record)
    -> loc:Location.t
    -> path:string
    -> (rec_flag * type_declaration list)
    -> 'new_record


end
