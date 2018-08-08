open Ast_405

(** Extensions of Ast_helper *)
open Asttypes
open Docstrings
open Ast_helper
open Parsetree

val ghost : Location.t -> Location.t
(** Make the location ghost *)
  
val at : ?loc:Location.t -> 'a -> 'a Location.loc
(** Attach the location. 
    If the location is not given, [Ast_helper.default_loc] is used. 
*)

val (!@) : 'a -> 'a Location.loc
(** [!@ x] is equivalent with [at x] *)

val lid : ?loc:Location.t -> string -> Longident.t Location.loc
(** Build a [Longident.t] with a location. 
    If the location is not given, [Ast_helper.default_loc] is used. 
*)
  
val with_loc : Location.t -> (unit -> 'a) -> 'a
(** Execute a function with setting [Ast_helper.default_loc] *)
  
val with_gloc : Location.t -> (unit -> 'a) -> 'a
(** Execute a function with setting [Ast_helper.default_loc], but with ghosting *)

module Name : sig
  val make_unique : string -> string
  (** Make a unique name by suffixing an incrementing id number *)
end

(** Extension of AST creator modules 

    For all the [?loc:Location.t] labels, [Ast_helper.default_loc]
    is used when omitted.
*)
  
(** Type expressions *)
module Typ : sig
  val new_var : string option (*+ prefix *) -> core_type
  (** Create a fresh type variable *)
    
  val ref_ :
    ?loc:Location.t ->
    ?attrs:attrs -> core_type -> core_type
  (** Create a type AST [t ref] *)
    
  val option :
    ?loc:Location.t ->
    ?attrs:attrs -> core_type -> core_type
  (** Create a type AST [t option] *)

  (** Imported from Ast_helper *)
    
  val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
  val attr: core_type -> attribute -> core_type
  val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
  val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
  val arrow: ?loc:loc -> ?attrs:attrs ->  arg_label -> core_type -> core_type
             -> core_type
  val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
  val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
  val object_: ?loc:loc -> ?attrs:attrs ->
                (str * attributes * core_type) list -> closed_flag ->
                core_type
  val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
  val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
  val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
               -> label list option -> core_type
  val poly: ?loc:loc -> ?attrs:attrs -> str list -> core_type -> core_type
  val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
               -> core_type
  val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type
  val force_poly: core_type -> core_type
end
  
(** Expressions *)
module Exp : sig
  val var :
    ?loc:Location.t ->
    ?attrs:attrs -> string -> expression
  (** Create a variable AST of the given name like "x" *)
    
  val id :
      ?loc:Location.t ->
      ?attrs:attrs -> string -> expression
  (** Create an longident AST of the given name, such as "Printf.printf". *)

  val lident :
      ?loc:Location.t ->
      ?attrs:attrs -> Longident.t -> expression
  (** Create an longident AST. *)

  val unit : unit -> expression
    
  val string :
      ?loc:Location.t ->
      ?attrs:attrs -> string -> expression
  (** Create a string AST *)
    
  val int :
      ?loc:Location.t ->
      ?attrs:attrs -> int -> expression
  (** Create an int AST *)

  val bool :
      ?loc:Location.t ->
      ?attrs:attrs -> bool -> expression
  (** Create a bool AST *)

  val option :
      ?loc:Location.t ->
      ?attrs:attrs ->
      expression option -> expression
  (** Create [Some e] or [None] *)
    
  val parse : string -> expression
  (** Parse the given string as an expression and returns the corresponding AST *)
    
  val object' :
      ?loc:Location.t ->
      ?attrs:attrs ->
      class_field list -> expression
  (** Create [object method m1 = e1 .. method mn = en end]. *)
    
  val seqs : expression list -> expression
  (** Create [e1; ..; en] *)
    
  val ignore_ : expression -> expression
  (** Create [ignore e] *)
    
  val assert_false : unit -> expression
  (** Create [assert false] *)

  val open_: ?loc:loc -> ?attrs:attrs -> ?override:bool -> lid -> expression -> expression
  (** [override]'s default is [false] *)
    
  (** Imported from Ast_helper *)

  val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
  val attr: expression -> attribute -> expression

  val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
  val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
  val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
            -> expression -> expression
  val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option -> pattern
            -> expression -> expression
  val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
  val apply: ?loc:loc -> ?attrs:attrs -> expression
             -> (arg_label * expression) list -> expression
  val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
              -> expression
  val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
  val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
  val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                 -> expression
  val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
               -> expression
  val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
              -> expression option -> expression
  val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
  val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                -> expression
  val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
  val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression option -> expression
  val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                -> expression
  val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
              -> expression
  val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
            -> direction_flag -> expression -> expression
  val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
              -> core_type -> expression
  val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                   -> expression
  val send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
  val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
  val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
  val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                -> expression
  val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                 -> expression
  val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression

  val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
  val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option -> expression
  val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
  val newtype: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
  val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
  val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
  val case: pattern -> ?guard:expression -> expression -> case

  val list : ?loc:loc -> ?attrs:attrs -> expression list -> expression

  val with_desc : expression -> expression_desc -> expression
  (** Replace pexp_desc *)

  val is_simple_ext : string -> expression -> bool
  (** [is_simple_ext <str> e] Returns [true] if [e] is [%<str>]. *)
end
  
(** Patterns *)
module Pat : sig
  val var' :
      ?loc:Location.t ->
      ?attrs:attrs -> string -> pattern
  (** Create a pattern variable. *)

  val unit : unit -> pattern

  val of_expr :
      expression ->
      [> `Error of expression | `Ok of pattern ]
  (** Translate an expression as a pattern.
      For example, [of_expr (Expr.parse "(x, y)")] returns an AST for [(x, y)].
  *)


  (** Imported from Ast_helper *)

  val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
  val attr:pattern -> attribute -> pattern
  val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
  val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
  val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
  val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
  val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
  val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
  val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
  val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
  val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
              -> pattern
  val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
  val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
  val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
  val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
  val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
  val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
  val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
  val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
end

module ExpPat : sig
  val var :
    ?loc:Location.t ->
    ?attrs:attrs ->
    string -> expression * pattern
  (** Create an expression and a pattern of the same variable. *)    
end
  
(** Class fields *)
module Cf : sig
  val method_concrete :
      ?loc:Location.t ->
      ?attrs:attrs ->
      str ->
      ?priv:bool ->
      ?override:bool -> expression -> class_field
  (** Create a non virtual (concrete) method 
      [override]'s default is [false]
  *)
    
  val method_virtual :
      ?loc:Location.t ->
      ?attrs:attrs ->
      str ->
      ?priv:bool -> core_type -> class_field
  (** Create a virtual method *)

  val inherit_: ?loc:loc -> ?attrs:attrs -> ?override:bool -> class_expr -> str option -> class_field
  (** [override]'s default is [false] *)
    
  val concrete: ?override:bool -> expression -> class_field_kind
  (** [override]'s default is [false] *)
    
  (** Imported from Ast_helper *)

  val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc -> class_field
  val attr: class_field -> attribute -> class_field

  val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag -> class_field_kind -> class_field
  val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag -> class_field_kind -> class_field
  val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_field
  val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
  val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
  val attribute: ?loc:loc -> attribute -> class_field
  val text: text -> class_field list

  val virtual_: core_type -> class_field_kind
end

(** Class structures *)
module Cstr : sig
  val mk  : pattern -> class_field list -> class_structure

  val mk' : ?self:pattern -> class_field list -> class_structure
  (** Create a class structure. *)
end

(** Module expressions *)
module Mod : sig
  val ident' : ?loc:Location.t -> Longident.t -> module_expr
  (** Create a module expression of the given longident *)  

  (** Imported from Ast_helper *)

  val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
  val attr: module_expr -> attribute -> module_expr
  val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
  val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
  val functor_: ?loc:loc -> ?attrs:attrs ->
    str -> module_type option -> module_expr -> module_expr
  val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr -> module_expr
  val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type -> module_expr
  val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
  val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
end

(** Value declarations *)
module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list -> str -> core_type -> value_description
  end

(** Type declarations *)
module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * variance) list -> ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str -> constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> str -> core_type -> label_declaration
  end

(** Type extensions *)
module Te:
  sig
    val mk: ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * variance) list -> ?priv:private_flag ->
      lid -> extension_constructor list -> type_extension

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str -> extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

(** {2 Module language} *)

(** Module type expressions *)
module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      str -> module_type option -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type -> with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

(** Signature items *)
module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> ?rec_flag: rec_flag -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> extension_constructor -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

(** Structure items *)
module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> ?rec_flag:rec_flag -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> extension_constructor -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_description -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

(** Module declarations *)
module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_type -> module_declaration
  end

(** Module type declarations *)
module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

(** Module bindings *)
module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_expr -> module_binding
  end

(* Opens *)
module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:bool -> lid -> open_description
    (** [override]'s default is [false] *)
  end

(* Includes *)
module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

(** Value bindings *)

module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      pattern -> expression -> value_binding
  end


(** {2 Class language} *)

(** Class type expressions *)
module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
  end

(** Class type fields *)
module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag -> virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag -> virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

(** Class expressions *)
module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option -> pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr -> (arg_label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list -> class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type -> class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
  end

(** Classes *)
module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag -> ?params:(core_type * variance) list ->
      str -> 'a -> 'a class_infos
  end

(** Class signatures *)
module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

(** Attributes *)
module Attr : sig
  val ocaml : ?loc:loc -> string -> expression -> attribute
end
  
val ocaml_warning : ?loc:loc -> string -> attribute
(** Create an attribute like [@@ocaml.warning <string>] *)

