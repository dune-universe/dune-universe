open Longident
let mknoloc = Location.mknoloc
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open VisitorsList
open VisitorsAnalysis
open VisitorsCompatibility

(* This module offers helper functions for code generation. *)

(* -------------------------------------------------------------------------- *)

(* Type abbreviations. *)

type variable = string
type datacon = string
type label = string
type classe = string
type methode = string
type tyvar = string

type variables = variable list
type tyvars = tyvar list
type core_types = core_type list
type patterns = pattern list
type expressions = expression list

(* -------------------------------------------------------------------------- *)

(* We should in principle ensure that our code makes sense even if the
   standard names that we rely upon are shadowed by the user. *)

(* This is made slightly difficult by the fact that the name [Pervasives]
   has been deprecated in favor of [Stdlib] in OCaml 4.07. *)

(* One viable approach would be to define the names that we need in the
   library [VisitorsRuntime], then refer to this library in the generated
   code. *)

(* One problem is that defining an alias for the standard operator (&&)
   causes it to become strict instead of lazy! So we cannot define an
   alias for it. *)

(* Let's just cross our fingers and assume that the user won't shadow
   the standard names that we need. *)

let pervasive (x : string) : Longident.t =
  Lident x

(* We normally place an improbable prefix in front of our private (local)
   variables, so as to make sure that we do not shadow user variables that
   are used in [@build] code fragments. *)

(* When producing code for inclusion in the documentation, we remove this
   prefix, just so that things look pretty. We rely on an undocumented
   environment variable to toggle this behavior. *)

let improbable (x : string) : string =
  try
    let _ = Sys.getenv "VISITORS_BUILDING_DOCUMENTATION" in
    x
  with Not_found ->
    "_visitors_" ^ x

(* -------------------------------------------------------------------------- *)

(* Types. *)

let ty_var (alpha : tyvar) : core_type =
  Typ.var alpha

let ty_vars (alphas : tyvars) : core_types =
  List.map ty_var alphas

let ty_any =
  Typ.any()

let ty_unit =
  tconstr "unit" []

(* For [ty_arrow], see [VisitorsCompatibility]. *)

let ty_arrows : core_types -> core_type -> core_type =
  List.fold_right ty_arrow

(* [decl_type decl] turns a declaration of the type ['a foo] into a the type
   ['a foo]. *)

let decl_type (decl : type_declaration) : core_type =
  tconstr decl.ptype_name.txt (ty_vars (decl_params decl))

(* -------------------------------------------------------------------------- *)

(* [unit] produces a unit constant. [tuple] produces a tuple. [record]
   produces a record. These functions already exist; we redefine them without
   any optional arguments so as avoid OCaml's warning 48 (implicit elimination
   of optional arguments). *)

let unit() =
  unit()

let tuple es =
  tuple es

let record les =
  record les

(* -------------------------------------------------------------------------- *)

(* [number i thing] constructs an English description of "[i] thing(s)". *)

let number i s =
  match i with
  | 0 ->
      Printf.sprintf "zero %s" s
  | 1 ->
      Printf.sprintf "one %s" s
  | _ ->
      Printf.sprintf "%d %ss" i s

(* -------------------------------------------------------------------------- *)

(* [eident] converts a (possibly-qualified) identifier to an expression. *)

let eident (id : Longident.t) : expression =
  Exp.ident (mknoloc id)

(* -------------------------------------------------------------------------- *)

(* [pvars] converts a list of variables to a list of patterns. *)

let pvars (xs : variables) : patterns =
  List.map (fun x -> pvar x) xs

(* [evars] converts a list of variables to a list of expressions. *)

let evars (xs : variables) : expressions =
  List.map (fun x -> evar x) xs

(* [pvarss] converts a matrix of variables to a matrix of patterns. *)

let pvarss (xss : variables list) : patterns list =
  List.map pvars xss

(* [evarss] converts a matrix of variables to a matrix of expressions. *)

let evarss (xss : variables list) : expressions list =
  List.map evars xss

(* -------------------------------------------------------------------------- *)

(* [wildcards] converts a list of anything to a list of wildcard patterns. *)

let wildcards xs =
  List.map (fun _ -> Pat.any()) xs

(* -------------------------------------------------------------------------- *)

(* [plambda p e] constructs a function [fun p -> e]. *)

(* For [plambda], see [VisitorsCompatibility]. *)

(* [lambda x e] constructs a function [fun x -> e]. *)

let lambda (x : variable) (e : expression) : expression =
  plambda (pvar x) e

(* [plambdas ps e] constructs a multi-argument function [fun ps -> e]. *)

let plambdas (ps : patterns) (e : expression) : expression =
  List.fold_right plambda ps e

(* [lambdas xs e] constructs a multi-argument function [fun xs -> e]. *)

let lambdas (xs : variables) (e : expression) : expression =
  List.fold_right lambda xs e

(* -------------------------------------------------------------------------- *)

(* [app] works like [Ast_convenience.app] (which it shadows), except it avoids
   constructing nested applications of the form [(f x) y], transforming them
   instead into a single application [f x y]. The difference is probably just
   cosmetic. *)

let app (e : expression) (es2 : expressions) : expression =
  match e.pexp_desc with
  | Pexp_apply (e1, les1) ->
      let les2 = List.map (fun e -> Label.nolabel, e) es2 in
      { e with pexp_desc = Pexp_apply (e1, les1 @ les2) }
  | _ ->
      app e es2

(* -------------------------------------------------------------------------- *)

(* [sequence es] constructs a sequence of the expressions [es]. *)

let sequence (es : expressions) : expression =
  (* Using [fold_right1] instead of [List.fold_right] allows us to get
     rid of a final [()] constant at the end of the sequence. Cosmetic. *)
  fold_right1
    (fun e accu -> Exp.sequence e accu)
    es
    (unit())

(* -------------------------------------------------------------------------- *)

(* [vblet1 vb e] constructs a single [let] binding. *)

let vblet1 (vb : value_binding) (e : expression) : expression =
  Exp.let_ Nonrecursive [vb] e

(* [let1 x e1 e2] constructs a single [let] binding. *)

let let1 (x : variable) (e1 : expression) (e2 : expression) : expression =
  vblet1 (Vb.mk (pvar x) e1) e2

(* [let1p x y e1 e2] constructs a single [let] binding of a pair. *)

let let1p (x, y : variable * variable) (e1 : expression) (e2 : expression) : expression =
  vblet1 (Vb.mk (ptuple [pvar x; pvar y]) e1) e2

(* [vbletn vbs e] constructs a series of nested [let] bindings. *)

let vbletn (vbs : value_binding list) (e : expression) : expression =
  List.fold_right vblet1 vbs e

(* [letn xs es e] constructs a series of nested [let] bindings. *)

let letn (xs : variables) (es : expressions) (e : expression) =
  List.fold_right2 let1 xs es e

(* [letnp xs ys es e] constructs a series of nested [let] bindings of pairs. *)

let letnp (xs : variables) (ys : variables) (es : expressions) (e : expression) =
  List.fold_right2 let1p (List.combine xs ys) es e

(* -------------------------------------------------------------------------- *)

(* [access x label] constructs a record access expression [x.label]. *)

let access (x : variable) (label : label) : expression =
  Exp.field (evar x) (mknoloc (Lident label))

(* [accesses labels xs] constructs a matrix of record access expressions of
   the form [x.label]. There is a row for every [label] and a column for every
   [x]. *)

let accesses (xs : variables) (labels : label list) : expressions list =
  List.map (fun label -> List.map (fun x -> access x label) xs) labels

(* -------------------------------------------------------------------------- *)

(* [ptuple] is [Ast_convenience.ptuple], deprived of its optional arguments. *)

let ptuple (ps : patterns) : pattern =
  ptuple ps

(* [ptuples] is [map ptuple]. *)

let ptuples (pss : patterns list) : patterns =
  List.map ptuple pss

(* -------------------------------------------------------------------------- *)

(* The Boolean expressions [false] and [true]. *)

let efalse : expression =
  Exp.construct (mknoloc (Lident "false")) None

let etrue : expression =
  Exp.construct (mknoloc (Lident "true")) None

(* -------------------------------------------------------------------------- *)

(* [conjunction es] constructs a Boolean conjunction of the expressions [es]. *)

let conjunction : expression =
  eident (pervasive "&&")

let conjunction e1 e2 =
  app conjunction [e1; e2]

let conjunction (es : expressions) : expression =
  fold_right1 conjunction es etrue

(* -------------------------------------------------------------------------- *)

(* [eassertfalse] is the expression [assert false]. *)

let eassertfalse : expression =
  Exp.assert_ efalse

(* -------------------------------------------------------------------------- *)

(* [eforce e] is the expression [Lazy.force e]. *)

let eforce : expression =
  eident (Longident.parse "Lazy.force")
    (* danger: the module name [Lazy] must not be shadowed. *)

let eforce (e : expression) : expression =
  app eforce [e]

(* -------------------------------------------------------------------------- *)

(* [eqphy e1 e2] is the expression [e1 == e2]. *)

let eqphy : expression =
  eident (pervasive "==")

let eqphy (e1 : expression) (e2 : expression) : expression =
  app eqphy [e1; e2]

(* [eqphys es1 es2] is the conjunction of the expressions [e1 == e2]. *)

let eqphys (es1 : expressions) (es2 : expressions) : expression =
  assert (List.length es1 = List.length es2);
  conjunction (List.map2 eqphy es1 es2)

(* -------------------------------------------------------------------------- *)

(* [efail s] generates a call to [VisitorsRuntime.fail]. The parameter [s] is
   a string, which could represent the place where a failure occurred, or the
   reason why a failure occurred. As of now, it is unused. *)

let efail : expression =
  eident (Ldot (Lident "VisitorsRuntime", "fail"))
    (* danger: the module name [VisitorsRuntime] must not be shadowed. *)

let efail (_ : string) : expression =
  app efail [ unit() ]

(* -------------------------------------------------------------------------- *)

(* [include_ e] constructs an [include] declaration. *)

let include_ (e : module_expr) : structure_item =
  Str.include_ {
    pincl_mod = e;
    pincl_loc = Location.none;
    pincl_attributes = [];
  }

(* -------------------------------------------------------------------------- *)

(* [with_warnings w items] wraps the structure items [items] in such a way
   that the warning directive [w] is applied to these items. Technically, this
   is done by emitting [include struct [@@@ocaml.warning <w>] <items> end]. *)

let with_warnings (w : string) (items : structure_item list) : structure_item =
  include_ (Mod.structure (
     floating "ocaml.warning" [ Str.eval (Exp.constant (const_string w)) ]
  :: items
  ))

(* -------------------------------------------------------------------------- *)

(* [class1 concrete ancestors params name self fields] builds a class
   declaration and packages it as a structure item. (This implies that it
   cannot be recursive with other class declarations). *)

let class1
  (concrete : bool)
  (params : (core_type * variance) list)
  (name : classe)
  (self : pattern)
  (fields : class_field list)
  : structure_item =
  Str.class_ [{
    pci_virt = if concrete then Concrete else Virtual;
    pci_params = params;
    pci_name = mknoloc name;
    pci_expr = Cl.structure (Cstr.mk self fields);
    pci_loc = !default_loc;
    pci_attributes = [];
  }]

(* -------------------------------------------------------------------------- *)

(* [inherit_ c tys] builds an [inherit] clause, where the superclass is [c]
   and its actual type parameters are [tys]. No [super] identifier is bound. *)

let inherit_ (c : Longident.t) (tys : core_types) : class_field =
  Cf.inherit_ Fresh (Cl.constr (mknoloc c) tys) None

(* -------------------------------------------------------------------------- *)

(* An algebraic data type of the methods that we generate. These include
   concrete methods (with code) and virtual methods (without code). They may
   be public or private. The method type is optional. If omitted, then
   it is inferred by OCaml. If present, it can be a polymorphic type. *)

type meth =
  Meth of private_flag * methode * expression option * core_type option

let concrete_method p m e oty =
  Meth (p, m, Some e, oty)

let virtual_method p m oty =
  Meth (p, m, None, oty)

(* -------------------------------------------------------------------------- *)

(* Converting a method description to OCaml abstract syntax. *)

let oe2cfk (oe : expression option) (oty : core_type option) : class_field_kind =
  match oe, oty with
  | Some e, Some _ ->
      Cf.concrete Fresh (Exp.poly e oty)
  | Some e, None ->
      Cf.concrete Fresh e
  | None, Some ty ->
      Cf.virtual_ ty
  | None, None ->
      Cf.virtual_ ty_any

let meth2cf (Meth (p, m, oe, oty)) : class_field =
  Cf.method_ (mknoloc m) p (oe2cfk oe oty)

(* -------------------------------------------------------------------------- *)

(* [method_name] extracts a method name out of a method description. *)

let method_name (Meth (_, m, _, _)) : string =
  m

(* -------------------------------------------------------------------------- *)

(* [is_virtual] tests whether a method description represents a virtual
   method. *)

let is_virtual (Meth (_, _, oe, _)) : bool =
  oe = None

(* -------------------------------------------------------------------------- *)

(* [send o m es] produces a call to the method [o#m] with arguments [es]. *)

let send (o : variable) (m : methode) (es : expressions) : expression =
  app (exp_send (evar o) m) es

(* -------------------------------------------------------------------------- *)

(* An algebraic data type of the ``hoisted expressions'' that we generate. *)

(* A ``hoisted expression'' is evaluated at most once after the object is
   allocated. Its value is stored in an instance field. We allow such an
   expression to reference [self], as long as it does not actually invoke any
   methods. *)

type hoisted =
  Hoisted of string     (* the name of the instance field *)
           * expression (* the hoisted expression *)

(* -------------------------------------------------------------------------- *)

(* Converting a hoisted field description to OCaml abstract syntax. *)

(* We generate a mutable field declaration, followed with an initialization:

     val mutable x =  lazy (assert false)
     initializer x <- lazy e

   We must do this in two steps because the expression [e] might contain
   references to [self], which are invalid in a field declaration, whereas
   they are allowed in an initializer.

   The potential danger in this idiom lies in forcing [x] before the
   initializer has finished running, leading to an assertion failure.
   This should not happen if [e] does not perform any method calls
   or read any fields. *)

let hoisted2cf (Hoisted (x, e)) : class_field list =
  [
    Cf.val_ (mknoloc x) (Mutable) (Cf.concrete Fresh (Exp.lazy_ eassertfalse));
    Cf.initializer_ (Exp.setinstvar (mknoloc x) (Exp.lazy_ e))
  ]

(* -------------------------------------------------------------------------- *)

(* A facility for generating a class. *)

module ClassFieldStore (X : sig end) : sig

  (* [generate meth] adds [meth] to the list of methods. *)
  val generate: meth -> unit

  (* [hoist e] causes the expression [e] to be hoisted, that is, computed
     once after the object is allocated. The result of evaluating [e] is
     stored in a field. The call [hoist e] returns an expression which
     reads this field. *)
  val hoist: expression -> expression

  (* [dump concrete ancestors params self c] returns a class definition. *)
  val dump:
    bool ->
    Longident.t list ->
    (core_type * variance) list ->
    pattern ->
    classe ->
    structure_item

end = struct

  let meths : meth list ref =
    ref []

  let generate meth =
    meths := meth :: !meths

  let dump () : class_field list =
    let methods = List.rev !meths in
    (* Move all of the virtual methods up front. If two virtual methods have
       the same name, keep only one of them. This is useful because we allow
       a virtual method declaration to be generated several times. In fact,
       OCaml supports this, but it looks tidier if we remove duplicates. *)
    let virtual_methods, concrete_methods = List.partition is_virtual methods in
    let cmp meth1 meth2 = compare (method_name meth1) (method_name meth2) in
    let virtual_methods = VisitorsList.weed cmp virtual_methods in
    let methods = virtual_methods @ concrete_methods in
    List.map meth2cf methods

  let hoisted : hoisted list ref =
    ref []

  let fresh : unit -> int =
    let c = ref 0 in
    fun () ->
      let x = !c in
      c := x + 1;
      x

  let hoist (e : expression) : expression =
    let x = Printf.sprintf "h%d" (fresh()) in
    hoisted := Hoisted (x, e) :: !hoisted;
    eforce (evar x)

  let dump concrete ancestors params self c : structure_item =
    class1 concrete params c self (
      (* [inherit] clauses. *)
      (* We ARBITRARILY assume that every ancestor class is parameterized
         with ONE type parameter. *)
      List.map (fun c -> inherit_ c [ ty_any ]) ancestors @
      (* Hoisted expressions. *)
      List.flatten (List.map hoisted2cf (List.rev !hoisted)) @
      (* Methods. *)
      dump()
    )

end

(* -------------------------------------------------------------------------- *)

(* A facility for emitting preprocessor warnings. *)

(* Warnings must be emitted under the form of [ppwarning] attributes, placed
   in the generated code. This is not very convenient; we must store these
   warnings, waiting for a convenient time to emit them. *)

module WarningStore (X : sig end) : sig

  (* [warning loc format ...] emits a warning. *)
  val warning: loc -> ('a, unit, string, unit) format4 -> 'a

  (* [warnings()] returns a list of all warnings emitted so far. *)
  val warnings: unit -> structure

end = struct

  let warnings : attribute list ref =
    ref []

  let warning loc msg =
    warnings := Ast_mapper.attribute_of_warning loc msg :: !warnings

  let warning loc format =
    Printf.ksprintf (warning loc) format

  let warnings () =
    let ws = !warnings in
    warnings := [];
    List.map (fun a -> Str.attribute a) (List.rev ws)

end
