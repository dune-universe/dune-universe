open VisitorsString
open VisitorsList
open Longident
open List
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving
open VisitorsPlugin
open VisitorsCompatibility
open VisitorsAnalysis
open VisitorsGeneration
open VisitorsSettings

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Setup (X : SETTINGS) = struct

let arity =
  X.arity

(* -------------------------------------------------------------------------- *)

(* If the [public] option is absent, then every method is public. If it is
   present, then every method is private, unless its name occurs in the list
   [X.public]. *)

let visibility m =
  match X.public with
  | None ->
      Public
  | Some ms ->
      if List.mem m ms then Public else Private

(* The following brings [generate] and [dump] into scope. *)

include ClassFieldStore(struct end)

let annotation (ty : core_type) : core_type option =
  (* A type annotation is generated only in [polymorphic] mode. *)
  if X.polymorphic then Some ty else None

let generate_concrete_method m e ty =
  generate (concrete_method (visibility m) m e (annotation ty))

let generate_virtual_method m ty =
  generate (virtual_method (visibility m) m (annotation ty))

(* -------------------------------------------------------------------------- *)

(* The following brings [warning] and [warnings] into scope. *)

include WarningStore(struct end)

(* [datacon_opacity_warning cd] emits a warning (if necessary) about the
   following issue. One should not write "A of int[@opaque]". Instead, one
   should write "A of (int[@opaque])". In the case of records fields, we fix
   this silently, by moving the attribute from the record field to the type,
   but in the case of data constructors with multiple fields, it is preferable
   to be strict. *)

let datacon_opacity_warning (cd : constructor_declaration) : unit =
  if opacity cd.pcd_attributes = Opaque then
    warning cd.pcd_loc
      "%s: @opaque, attached to a data constructor, is ignored.\n\
       It should be attached to a type. Please use parentheses."
      plugin

(* [sum_build_warning decl] emits emits a warning (if necessary) about the
   following issue. One should not attach a [@@build] attribute to a sum type.
   Instead, one should attach a [@build] attribute to every data constructor.
   Note that one can attach a [@@build] attribute to a record type. *)

let sum_build_warning (decl : type_declaration) : unit =
  if build decl.ptype_attributes <> None then
    warning decl.ptype_loc
      "%s: @@build, attached to a sum type, is ignored.\n\
       Instead, @build should be attached to each data constructor."
      plugin

(* -------------------------------------------------------------------------- *)

(* Shared glue code for detecting and warning against name clashes. *)

type 'a wrapper =
  'a -> 'a

type tycon_visitor_method =
  Location.t * attributes * Longident.t -> methode

let protect_tycon_visitor_method : tycon_visitor_method wrapper =
  fun tycon_visitor_method ->
    let format : (_, _, _, _) format4 =
      "%s: name clash: the types %s and %s\n\
       both have visitor methods named %s.\n\
       Please consider using [@@name] at type declaration sites\n\
       or [@name] at type reference sites."
    in
    let id = print_longident in
    protect tycon_visitor_method
      (fun (_, _, x) (_, _, y) -> x = y)
      (fun (_, _, x) (loc, _, y) m -> warning loc format plugin (id x) (id y) m)

type datacon_descending_method =
  constructor_declaration -> methode

let protect_datacon_descending_method : datacon_descending_method wrapper =
  fun datacon_descending_method ->
    let format : (_, _, _, _) format4 =
      "%s: name clash: the data constructors %s and %s\n\
       both have visitor methods named %s.\n\
       Please consider using [@name] at data constructor declaration sites."
    in
    let id cd = cd.pcd_name.txt in
    protect datacon_descending_method
      (fun cd1 cd2 -> cd1 == cd2)
      (fun cd1 cd2 m -> warning cd2.pcd_loc format plugin (id cd1) (id cd2) m)

(* -------------------------------------------------------------------------- *)

(* We support parameterized type declarations. We require them to be regular.
   That is, for instance, if a type ['a term] is being defined, then every
   use of [_ term] in the definition should be ['a term]; it cannot be, say,
   [int term] or [('a * 'a) term]. *)

(* To enforce this, we check that, in every use of a local type constructor,
   the actual type parameters coincide with the formal type parameters. *)

(* This check is imposed only on [mono] type variables. For [poly] type
   variables, irregularity is allowed. *)

(* The purpose of this check is to avoid an incomprehensible type error in
   the generated code. *)

let check_regularity loc tycon (formals : tyvars) (actuals : core_types) =
  (* Check that the numbers of parameters match. *)
  if length formals <> length actuals then
    raise_errorf ~loc
      "%s: the type constructor %s expects %s,\n\
       but is applied to %s."
      plugin tycon
      (number (length formals) "type parameter")
      (number (length actuals) "type parameter");
  (* Check that the parameters match. *)
  if not X.irregular && not (
    fold_left2 (fun ok formal actual ->
      ok && (X.poly formal || actual.ptyp_desc = Ptyp_var formal)
    ) true formals actuals
  ) then
    raise_errorf ~loc "%s: the type constructor %s is irregular." plugin tycon

(* -------------------------------------------------------------------------- *)

(* Public naming conventions. *)

(* The names of the methods associated with the type [foo] are normally based
   on (derived from) the name [foo].

   This base name can be overriden by the user via an attribute. For a local
   type, a [@@name] attribute must be attached to the type declaration. For a
   nonlocal type, a [@name] attribute must be attached to every reference to
   this type.

   The [@name] attribute can be misused: e.g., one can mistakenly use
   different visitor method names for different occurrences of a single type.
   We currently do not attempt to detect this situation.

   The prefix that is prepended to the base name can be controlled via the
   settings [visit_prefix], [build_prefix], and [fail_prefix]. *)

let tycon_modified_name (attrs : attributes) (tycon : tycon) : tycon =
  maybe (name attrs) tycon

(* Similarly, the base name of the methods associated with a data constructor
   can be altered via a [@name] attribute, which must be attached to the data
   constructor declaration. *)

let datacon_modified_name (cd : constructor_declaration) : datacon =
  maybe (name cd.pcd_attributes) cd.pcd_name.txt

(* For every type constructor [tycon], there is a visitor method, also called
   a descending method, as it is invoked when going down into the tree. *)

(* The name of this method is normally [visit_foo] if the type is named [foo]
   or [A.foo]. (A qualified name must denote a nonlocal type.) *)

(* This convention can cause name clashes, as the types [foo] and [A.foo]
   receive visitor methods by the same name. We warn if this happens.

   A name clash can also be caused by incorrect use of the [@@name] or
   [@name] attributes. We also warn if this happens. *)

(* Step 1 -- the raw convention. *)

let tycon_visitor_method : tycon_visitor_method =
  fun (_, attrs, tycon) ->
    X.visit_prefix ^ tycon_modified_name attrs (Longident.last tycon)

(* Step 2 -- protect against name clashes. *)

let tycon_visitor_method =
  protect_tycon_visitor_method tycon_visitor_method

(* Step 3 -- define auxiliary functions that are easier to use. *)

let local_tycon_visitor_method (decl : type_declaration) : methode =
  tycon_visitor_method (decl.ptype_loc, decl.ptype_attributes, Lident decl.ptype_name.txt)

let nonlocal_tycon_visitor_method (ty : core_type) : methode =
  match ty.ptyp_desc with
  | Ptyp_constr (tycon, _) ->
      tycon_visitor_method (ty.ptyp_loc, ty.ptyp_attributes, tycon.txt)
  | _ ->
      assert false

(* For every local record type constructor [tycon], there is an ascending
   method, which is invoked on the way up, in order to re-build some data
   structure. This method is virtual and exists only when the scheme is
   [fold]. *)

(* The name of this method is normally [build_foo] if the type is named [foo]. *)

let tycon_ascending_method (decl : type_declaration) : methode =
  X.build_prefix ^ tycon_modified_name decl.ptype_attributes decl.ptype_name.txt

(* [mono] type variables have a virtual visitor method. We include a quote in
   the method name so as to ensure the absence of collisions. *)

let tyvar_visitor_method (alpha : tyvar) : methode =
  sprintf "%s'%s" X.visit_prefix alpha

(* For every data constructor [datacon], there is a descending visitor method,
   which is invoked on the way down, when this data constructor is discovered. *)

(* The name of this method is normally [visit_Foo] if the data constructor is
   named [Foo]. *)

let datacon_descending_method (cd : constructor_declaration) : methode =
  X.visit_prefix ^ datacon_modified_name cd

let datacon_descending_method =
  protect_datacon_descending_method datacon_descending_method

(* For every data constructor [datacon], there is a ascending visitor method,
   which is invoked on the way up, in order to re-build some data structure.
   This method is virtual and exists only when the scheme is [fold]. *)

let datacon_ascending_method (cd : constructor_declaration) : methode =
  X.build_prefix ^ datacon_modified_name cd

(* At arity 2, for every sum type constructor [tycon] which has at least two
   data constructors, there is a failure method, which is invoked when the
   left-hand and right-hand arguments do not exhibit the same tags. *)

(* The name of this method is normally [fail_foo] if the type is named [foo]. *)

let failure_method (decl : type_declaration) : methode =
  X.fail_prefix ^ tycon_modified_name decl.ptype_attributes decl.ptype_name.txt

(* When [scheme] is [Reduce], we need a monoid, that is, a unit [zero] and a
   binary operation [plus]. The names [zero] and [plus] are fixed. We assume
   that there exist virtual methods by these names. It is up to the user to
   provide these methods via inheritance, that is, via the [ancestors]
   option. *)

let zero =
  "zero"

let plus =
  "plus"

(* -------------------------------------------------------------------------- *)

(* Private naming conventions. *)

(* These conventions must be set up so as to avoid collisions within each name
   space separately: e.g., variables, methods, type variables, and so on. *)

(* We use improbable variable names, because it is possible for user code to
   be placed in the scope of these variables. (This is the case when the user
   provides [@build] annotations.) As a couple exceptions, the names [self]
   and [env] are not made improbable, and we document the existence of these
   variables, which can be used in [@build] annotations. *)

(* In a class, the variable [self] refers to self.
   The type variable [ty_self] denotes its type. *)

let self : variable =
  "self"

let ty_self : core_type =
  Typ.var "self"

let pself : pattern =
  Pat.constraint_ (pvar self) ty_self

(* The variable [env] refers to the environment that is carried down into
   recursive calls. *)

let env : variable =
  "env"

(* We sometimes need two (or more) copies of a variable: one copy for each
   index [j] ranging in the interval [0..arity). *)

let copy (j : int) (x : string) : string =
  assert (0 <= j && j < arity);
  if arity = 1 then
    (* No alteration required. *)
    x
  else
    sprintf "%s_%d" x j

(* The variables [component i j] denote tuple components. The index [i]
   ranges over tuple components; the index [j] ranges in [0..arity). *)

let component (i : int) (j : int) : variable =
  improbable (copy j (sprintf "c%d" i))

let components (i : int) : variables =
  map (component i) (interval 0 arity)

let componentss (xs : _ list) : variables list =
  mapi (fun i _ -> components i) xs

(* The variable [thing j] denotes an input value. *)

let thing (j : int) : variable =
  improbable (copy j "this")

let things : variables =
  map thing (interval 0 arity)

(* The variable [this] is used only in the generation of [endo] visitors. *)

let this =
  thing 0

(* The variables [field label j] denote record fields. *)

let field (label : label) (j : int) : variable =
  improbable (copy j (sprintf "f%s" label))

let fields (label : label) : variables =
  map (field label) (interval 0 arity)

let fieldss (labels : label list) : variables list =
  map fields labels

(* The variables [result i] denote results of recursive calls. *)

let result (i : int) : variable =
  improbable (sprintf "r%d" i)

let results (xs : _ list) : variables =
  mapi (fun i _ -> result i) xs

(* The variables [summary i] denote results of recursive calls.
   When the scheme is [MapReduce], each recursive call produces
   a pair; we use [result i] and [summary i] as the names of the
   pair components. *)

let summary (i : int) : variable =
  improbable (sprintf "s%d" i)

let summaries (xs : _ list) : variables =
  mapi (fun i _ -> summary i) xs

(* Reserved names of type variables. We forbid the user from using these
   names, and do not let them be renamed by the function [variant] below. *)

let reserved : tyvars =
  [ "s"; "env" ]

let reserved_ty_var (alpha : tyvar) : core_type =
  assert (mem alpha reserved);
  ty_var alpha

(* Naming conventions for type variables in type annotations. If ['a]
   is a type variable named by the user, we use ['a_i], where [i] is
   in [0..arity]. Indices [i] in the interval [0..arity) are used for
   the arguments of a visitor method. The index [arity] is used for
   the result of a visitor method. *)

(* If [scheme] is [Endo], then the argument and result must have the
   same type, so we do not introduce a variation in the type variables. *)

let variant (i : int) (alpha : tyvar) : tyvar =
  assert (0 <= i && i <= arity);
  if X.scheme = Endo || mem alpha reserved then
    alpha
  else
    sprintf "%s_%d" alpha i

let vary_type (i : int) (ty : core_type) : core_type =
  rename_type (variant i) ty

(* [ty_monoid] is the type of monoid elements. *)

let ty_monoid =
  reserved_ty_var "s"

(* [ty_env] is the type of the environment. *)

(* What IS the type of the environment? This is a tricky question. Two
   possibilities arise:

   1. One might wish for every visitor method to be polymorphic in the type
   ['env] of the environment. This makes the method more widely applicable,
   but means that the environment effectively cannot be used by the method
   (except by passing it on to its callees). Thus, the method cannot be
   overridden by a custom implementation that actually uses the environment.

   2. One might wish for the environment to have monomorphic type. In that
   case, one should note that there is a priori no reason why the type of
   the environment should be the same in every method. So, we must be
   careful not to use a single type variable ['env]. We must use a distinct
   type variable every time, or (easier but equivalent) use a wildcard.

   How do we let the user specify which behavior is desired? And with what
   granularity? We choose a simple approach: we treat ['env] as polymorphic
   if and only if this (reserved) type variable is declared polymorphic by
   the user. *)

let ty_env =
  if X.poly "env" then
    reserved_ty_var "env"
  else
    ty_any

(* What is the type of a virtual visitor method [visit_'a] in charge of
   dealing with a [mono] type variable ['a]?

   One might think that it must be a monomorphic type, so we can just
   issue a wildcard [_] and let OCaml infer this type.

   Yet, if the user has requested that every method be polymorphic in
   the type ['env] of the environment, then [visit_'a], too must be
   polymorphic in ['env]. (Otherwise, the generated code would be ill-typed.)
   In that case, we must generate ['env . 'env -> _].

   This implies that [visit_'a] cannot use the environment.
   So, it seems somewhat doubtful that this feature is useful.
   Perhaps we could allow the environment to consist of two components
   and to quantify universally over only one of them? *)

let tyvar_visitor_method_type =
  if X.poly "env" then
    typ_poly ["env"] (ty_arrow ty_env ty_any)
  else
    ty_any

(* [poly] type variables have a visitor function. *)

let tyvar_visitor_function (alpha : tyvar) : variable =
  tyvar_visitor_method alpha

(* -------------------------------------------------------------------------- *)

(* Construction of type annotations. *)

(* [result_type scheme ty] is the result type of a visitor method associated
   with the type [ty]. *)

(* If [ty] is of the form [decl_type decl] -- that is, if [ty] is a local type
   constructor -- then this is the result type of the visitor method associated
   with [ty]. *)

let rec result_type scheme (ty : core_type) : core_type =
  match scheme with
  | Iter ->
      (* An [iter] visitor method returns nothing. *)
      ty_unit
  | Map | Endo ->
      (* A [map] or [endo] visitor method for the type [ty] returns a
         value of type [ty]. Note that [ty] can contain type variables. *)
      ty
  | Reduce ->
      (* A [reduce] visitor method returns a monoid element. *)
      ty_monoid
  | MapReduce ->
      (* A [mapreduce] visitor method returns a pair of the results produced
         by a [map] visitor method and by a [reduce] visitor method. *)
      Typ.tuple [ result_type Map ty; result_type Reduce ty ]
  | Fold ->
      (* This is where we have a problem. We would really like to allow the
         user to specify which skeleton should be used here, as we cannot
         guess it. We might allow it in the future. For now, we impose the
         monomorphic skeleton [_], which is not as general as we would like,
         since it requires the method to have monomorphic result type. *)
      ty_any

let result_type =
  result_type X.scheme

(* [decl_result_type decl] is the result type of a visitor method associated
   with the type declaration [decl]. *)

let decl_result_type decl =
  result_type (decl_type decl)

(* A visitor function takes an environment, followed with [arity] arguments,
   and produces a result. Thus, if [argument] and [result] are types, then the
   type of a visitor function has the following shape:

     ty_env ->
     argument_0 -> ... -> argument_{arity-1} ->
     result_{arity}

   where [ty_{i}] denotes a copy of the type [ty] whose type variables have
   been renamed by the renaming [variant i]. *)

(* We generalize the above definition to allow for multiple [arguments]. This
   is used in the visitor methods associated with data constructors. Thus,
   each argument in succession is extended to [arity] arguments. *)

(* We specialize the above definition to the case where the result type
   is [result_type ty]. *)

let visitor_fun_type (arguments : core_types) (ty : core_type) : core_type =
  ty_arrows
    (ty_env :: flatten (hextend arguments arity vary_type))
    (vary_type arity (result_type ty))

(* This special case of [visitor_fun_type] is the normal form of a visitor
   function type: there is one argument of type [ty] (extended to [arity])
   and one result of type [result_type ty]. *)

let simple_visitor_fun_type (ty : core_type) : core_type =
  visitor_fun_type [ty] ty

(* [visitor_method_type decl] is the type of the visitor method associated
   with the type [decl]. This does not account for the visitor parameters
   in charge of dealing with type variables. *)

let visitor_method_type (decl : type_declaration) : core_type =
  simple_visitor_fun_type (decl_type decl)

(* [visitor_param_type alpha] is the type of the visitor function associated
   with the type variable [alpha]. *)

let visitor_param_type (alpha : tyvar) : core_type =
  simple_visitor_fun_type (ty_var alpha)

(* [fold_result_type ty] is the result type of the visitor code generated
   by [visit_type ... ty], when [scheme] is [Fold]. *)

let fold_result_type _ty =
  (* This function is currently unimplemented and unused, because we
     do not allow [polymorphic] to be [true] when [scheme] is [Fold].
     Thus, we do not generate any type annotations for ascending methods. *)
  ty_any

(* [poly_params decl] is the subset of the formal type parameters of [decl]
   which are marked [poly]. For each of these parameters, a visitor function
   should be passed. *)

let poly_params (decl : type_declaration) : tyvars =
  filter X.poly (decl_params decl)

(* [quantify alphas ty] quantifies an appropriate set of type variables in the
   method type [ty]. The parameter [alphas] is usually [poly_params decl],
   although it could in principle be a subset of it, if we can prove that
   some visitor functions are unneeded. We introduce universal quantifiers
   on (suitable variants of) the type variables [alphas] and also possibly
   on the type variable ['env]. *)

let quantify (alphas : tyvars) (ty : core_type) : core_type =
  (* Find out which variants of the type variables [alphas] we should quantify
     over. For the arguments, we need to quantify over the variants in the
     interval [0..arity). For the result, we may need to quantify over the
     variant [arity]. We try and avoid superfluous quantifiers, as that would
     decrease readability. *)
  let alphas =
    match X.scheme with
    | Iter
    | Reduce ->
        (* Just the arguments. The result contains no type variables. *)
        flatten (hextend alphas arity variant)
    | Map
    | MapReduce ->
        (* Arguments and result. *)
        flatten (hextend alphas (arity+1) variant)
    | Endo ->
        (* In this special case, there is just one variant, as the argument
           and result must have the same type. *)
        alphas
    | Fold ->
        (* Polymorphism currently not supported with [Fold]. *)
        []
  in
  (* Then, decide whether ['env] should be universally quantified. *)
  let alphas =
    if X.poly "env" then
      "env" :: alphas
    else
      alphas
  in
  (* Done. *)
  typ_poly alphas ty

(* -------------------------------------------------------------------------- *)

(* [bind rs ss] is a binding construct which, depending on the scheme, binds
   either the variables [rs], or the variables [ss], or both, using pair
   patterns. It is used to bind the results of recursive calls to visitor
   methods. *)

let bind (rs : variables) (ss : variables)
  : expressions -> expression -> expression =
  match X.scheme with
  | Iter
  | Map
  | Endo
  | Fold ->
      letn rs
  | Reduce ->
      letn ss
  | MapReduce ->
      letnp rs ss

(* -------------------------------------------------------------------------- *)

(* [call m es] emits a method call of the form [self#m es]. *)

let call (m : methode) (es : expressions) : expression =
  send self m es

(* -------------------------------------------------------------------------- *)

(* Access to the monoid operations. *)

let monoid_unit () : expression =
  assert (X.scheme = Reduce || X.scheme = MapReduce);
  call zero []

let monoid_law () : expression =
  assert (X.scheme = Reduce || X.scheme = MapReduce);
  call plus []

(* -------------------------------------------------------------------------- *)

(* [reduce es] reduces the expressions [es], that is, it combines them, using
   a monoid, which provides a unit and a binary operation. The reduction is
   performed left-to-right. This could be of importance if the monoid is not
   associative-commutative. *)

let reduce es =
  let unit = monoid_unit()
  and law = monoid_law() in
  fold_left1 (fun e1 e2 -> app law [e1; e2]) unit es

(* -------------------------------------------------------------------------- *)

(* [alias x ps] requires the pattern list [ps] to have length [arity]. If
   [scheme] is [Endo], then it further requires [arity] to be 1. It adds a
   binding of the variable [x], using an [as] pattern, at the top level of the
   pattern. The result is again packaged as a pattern list of length [arity].
   If scheme is not [Endo], then [alias x ps] is just [ps]. *)

let alias (x : variable) (ps : patterns) : patterns =
  assert (length ps = arity);
  match X.scheme with
  | Endo ->
      assert (arity = 1);
      map (fun p ->
        Pat.alias p (Location.mknoloc x)
      ) ps
  | _ ->
      ps

(* If [scheme] is [Endo], then [transmit x xs] is [x :: xs]. Otherwise, it is
   just [xs]. *)

let transmit x xs =
  match X.scheme with
  | Endo ->
      x :: xs
  | _ ->
      xs

(* -------------------------------------------------------------------------- *)

(* [hook m xs ty e] constructs a call of the form [self#m xs], and (as a side
   effect) generates a method [method m xs = e]. The free variables of the
   expression [e] must be (a subset of) [xs]. The type [ty] is the type of
   the method. It is always computed internally, but the type annotation is
   actually printed only in [polymorphic] mode. *)

(* Thus, by default, the expression [hook m xs ty e] behaves in the same way
   as the expression [e]. But a hook, named [m], allows this default to be
   overridden. *)

let hook (m : methode) (xs : variables) (ty : core_type) (e : expression) : expression =
  (* Generate a method. *)
  generate_concrete_method m (lambdas xs e) ty;
  (* Construct a method call. *)
  call m (evars xs)

(* The additional parameter [b] makes hook insertion optional. If [b] is [true],
   a hook is created; otherwise, no hook is created. *)

let hook b m xs ty e =
  if b then hook m xs ty e else e

(* -------------------------------------------------------------------------- *)

(* [vhook m xs ty] constructs a call of the form [self#m xs], and (as a side
   effect) generates a virtual method [method m: ty]. The type [ty] is the type
   of the method. It is always computed internally, but the type annotation is
   actually printed only in [polymorphic] mode. *)

let vhook (m : methode) (xs : variables) (ty : core_type) : expression =
  generate_virtual_method m ty;
  call m (evars xs)

(* -------------------------------------------------------------------------- *)

(* If a data constructor or record carries a [@build] attribute, then the
   OCaml expression carried by this attribute should be used instead of the
   default [builder] function, which rebuilds a data constructor or record.
   This concerns [map], [endo], and [mapreduce] visitors. *)

type builder =
  variables -> expression

let ifbuild (attrs : attributes) (builder : builder) : builder =
  match build attrs with
  | None ->
      builder
  | Some e ->
      fun rs -> app e (evars rs)

(* -------------------------------------------------------------------------- *)

(* The following classes help build the code that forms the ascending part of
   a visitor method, that is, the code found after the recursive calls. *)

(* There are four variants of this code, used in visitor methods for data
   constructors, in visitor methods for records, in visitor functions for
   tuples, and in visitor functions for @opaque types. *)

(* The base class, [ascend], provides as much shared behavior as possible. *)

class virtual ascend (ss : variables) = object (self)

  (* An [iter] visitor returns a unit value. *)
  method ascend_Iter =
    unit()

  (* The behavior of a [map] visitor is defined in subclasses. *)
  method virtual ascend_Map : expression

  (* By default, an [endo] visitor behaves like a [map] visitor. This behavior
  is appropriate at @opaque types. *)
  method ascend_Endo =
    self#ascend_Map

  (* A [reduce] visitor uses [zero] and [plus] to combine the results
     of the recursive calls, which are bound to the variables [ss]. *)
  method ascend_Reduce =
    reduce (evars ss)

  (* A [mapreduce] visitor returns a pair of the results that would be
     returned by a [map] visitor and by a [reduce] visitor. *)
  method ascend_MapReduce =
    tuple [ self#ascend_Map; self#ascend_Reduce ]

  (* By default, a [fold] visitor behaves like a [map] visitor, because we
     have no better choice. This behavior is used  at tuples and at @opaque
     types. *)
  method ascend_Fold =
    self#ascend_Map

  (* Dispatch. *)
  method ascend =
    match X.scheme with
    | Iter      -> self#ascend_Iter
    | Map       -> self#ascend_Map
    | Endo      -> self#ascend_Endo
    | Reduce    -> self#ascend_Reduce
    | MapReduce -> self#ascend_MapReduce
    | Fold      -> self#ascend_Fold

end

(* The subclass [ascend_opaque] defines the desired behavior at @opaque types. *)

(* In an [iter] visitor, we return a unit value, as always. This means that,
   even if [arity] is greater than 1, NO EQUALITY TEST takes place. This
   differs from the behavior of the methods [visit_int], [visit_bool], etc.,
   which perform an equality test. *)

(* In a [map] visitor, we return THE FIRST ARGUMENT of the visitor. At arity
   greater than 1, this is an ARBITRARY choice. It is not clear what else we
   could do. *)

(* In a [reduce] visitor, we return the neutral element, [self#zero]. *)

(* In a [fold] visitor, we keep the default behavior, which is to behave like
   a [map] visitor. *)

class ascend_opaque (xs : variables) = object

  inherit ascend []

  method ascend_Map =
    evar (hd xs) (* [xs] is the vector of arguments; pick the first one *)

end

(* The subclass [ascend_endo] defines the standard behavior of an [endo]
   visitor, which is to perform physical equality tests. *)

(* Its parameters are as follows:

   [this]       (a variable for) the data structure that is visited
   [subjects]   the matrix of arguments to the recursive calls
   [rs], [ss]   (vectors of variables for) the results of the recursive calls *)

class virtual ascend_endo
  (this : variable)
  (subjects : expressions list)
  (rs : variables)
  (ss : variables)
= object (self)

  inherit ascend ss

  (* An [endo] visitor first tests if the arguments of the recursive calls,
     [subjects], are physically equal to the results of these calls, [rs]. If
     that is the case, then it returns the original data structure, [this].
     Otherwise, it reconstructs a new data structure, like a [map] visitor. *)
  method! ascend_Endo =
    (* [subjects] is a matrix of width [arity], and [arity] is [1]. The first
       column of [subjects] is [map hd subjects]. *)
    assert (for_all (fun es -> length es = arity) subjects);
    assert (arity = 1);
    Exp.ifthenelse
      (eqphys (map hd subjects) (evars rs))
      (evar this)
      (Some self#ascend_Map)

end

(* The subclass [ascend_tuple] defines the desired behavior at tuple types. *)

class ascend_tuple this subjects rs ss = object

  inherit ascend_endo this subjects rs ss

  (* A [map] visitor reconstructs a tuple. *)
  method ascend_Map =
    tuple (evars rs)

end

(* The subclass [ascend_algebraic] defines the desired behavior at a sum type
   or record type. Its extra parameters are as follows:

   [builder]  how to reconstruct a data constructor or record
   [decl]     the type declaration under which we are working
   [m]        the name of the virtual ascending method
   [tys]      the types of the components of this data constructor or record *)

class ascend_algebraic this subjects rs ss
  (builder : builder)
  (decl : type_declaration)
  (m : methode)
  (tys : core_types)
= object

  inherit ascend_endo this subjects rs ss

  (* A [map] visitor reconstructs a data structure using the results [rs] of
     the recursive calls. *)
  method ascend_Map =
    builder rs

  (* A [fold] visitor invokes the virtual ascending method [m], with [env] and
     [rs] as arguments. As a side effect, [ascend_Fold] declares this virtual
     method. *)
  method! ascend_Fold =
    vhook m
      (env :: rs)
      (ty_arrows
         (ty_env :: map fold_result_type tys)
         (decl_result_type decl)
      )

end

(* -------------------------------------------------------------------------- *)

(* [visit_type env_in_scope ty] builds a small expression that represents the
   visiting code associated with the OCaml type [ty]. For instance, if [ty] is
   a local type constructor, this could be a call to the visitor method
   associated with this type constructor. *)

(* This expression may refer to the variable [self]. *)

(* If [env_in_scope] is true, then this expression may refer to the variable
   [env]. If [env_in_scope] is false, then this expression should denote a
   function of [env]. The use of [env_in_scope] complicates things slightly,
   but allows us to avoid the production of certain eta-redexes. *)

(* If [ty] carries the attribute [@opaque], then we act as if there is nothing
   to visit. The nature of the type [ty], in that case, plays no role. *)

let rec visit_type (env_in_scope : bool) (ty : core_type) : expression =
  match env_in_scope, opacity ty.ptyp_attributes, ty.ptyp_desc with

  (* A type constructor [tycon] applied to type parameters [tys]. We handle
     the case where [env_in_scope] is false, so we construct a function of
     [env]. *)
  | false,
    NonOpaque,
    Ptyp_constr ({ txt = tycon; _ }, tys) ->
      (* [tycon] is a type constructor, applied to certain types [tys]. *)
      (* We must call the visitor method associated with [tycon],
         applied to the visitor functions associated with SOME of the [tys]. *)
      let m, tys =
        match is_local X.decls tycon with
        | Some decl ->
            let formals = decl_params decl in
            (* [tycon] is a local type constructor, whose formal type parameters
               are [formals]. Among these formal type parameters, some should be
               treated in a monomorphic manner, and some should be treated in a
               polymorphic manner. The function [X.poly], applied to a type
               variable [formal], tells how it should be treated. *)
            (* The regularity check is applied only to [mono] parameters. *)
            check_regularity ty.ptyp_loc (last tycon) formals tys;
            (* The visitor method should be applied to the visitor functions
               associated with the subset of [tys] that corresponds to [poly]
               variables. *)
            local_tycon_visitor_method decl,
            filter2 X.poly formals tys
        | None ->
            (* [tycon] is a nonlocal type constructor. *)
            (* The visitor method should be applied to visitor functions for all
               of the types [tys]. *)
           (* This visitor method is NOT generated by us, so it MUST be
               inherited from an ancestor class; it is up to the user to
               ensure that this method exists. (It may be virtual.) This
               method may be polymorphic, so multiple call sites do not
               pollute one another. *)
            nonlocal_tycon_visitor_method ty,
            tys
      in
      app
        (call m [])
        (map (visit_type false) tys)

  (* A type variable [alpha] must be a formal parameter of the current
     declaration. (Indeed, we do not handle GADTs yet.) Now, two cases
     arise. If [alpha] is [mono], then it is handled by a virtual visitor
     method. If [alpha] is [poly], then it is handled by a visitor function
     which we must have received as an argument. *)
  | false,
    NonOpaque,
    Ptyp_var alpha ->
      if X.poly alpha then
        evar (tyvar_visitor_function alpha)
      else
        vhook (tyvar_visitor_method alpha) [] tyvar_visitor_method_type

  (* A tuple type. We handle the case where [env_in_scope] is true, as it
     is easier. *)
  | true,
    NonOpaque,
    Ptyp_tuple tys ->
      (* Construct a function that takes [arity] tuples as arguments. *)
      (* See [constructor_declaration] for comments. *)
      let xss = componentss tys in
      let subjects = evarss xss in
      let rs = results xss
      and ss = summaries xss in
      let ascend = new ascend_tuple this subjects rs ss in
      plambdas
        (alias this (ptuples (transpose arity (pvarss xss))))
        (bulk rs ss tys subjects ascend)

  (* If [env_in_scope] does not have the desired value, wrap a recursive call
     within an application or abstraction. At most one recursive call takes
     place, so we never produce an eta-redex. *)
  | true, NonOpaque, (Ptyp_constr _ | Ptyp_var _) ->
     app (visit_type false ty) [evar env]
  | false, _, _ ->
     lambda env (visit_type true ty)

  (* If [ty] is marked opaque, then we ignore the structure of [ty] and carry
     out appropriate action, based on the current scheme. *)
  | true, Opaque, _ ->
      (* Construct a function that takes [arity] arguments. *)
      lambdas things (
        let ascend = new ascend_opaque things in
        ascend#ascend
      )

  (* An unsupported construct. *)
  | _, _, Ptyp_any
  | _, _, Ptyp_arrow _
  | _, _, Ptyp_object _
  | _, _, Ptyp_class _
  | _, _, Ptyp_alias _
  | _, _, Ptyp_variant _
  | _, _, Ptyp_poly _
  | _, _, Ptyp_package _
  | _, _, Ptyp_extension _ ->
      unsupported ty

and visit_types tys (ess : expressions list) : expressions =
  (* The matrix [ess] is indexed first by component, then by index [j].
     Thus, to each type [ty], corresponds a row [es] of expressions,
     whose length is [arity]. *)
  assert (is_matrix (length tys) arity ess);
  map2 (fun ty es ->
    app (visit_type true ty) es
  ) tys ess

(* -------------------------------------------------------------------------- *)

(* The expression [bulk rs ss tys subjects ascend] represents the bulk of a
   visitor method or visitor function. It performs the recursive calls, binds
   their results to [rs] and/or [ss], then runs the ascending code. *)

and bulk
  (rs : variables) (ss : variables)
  (tys : core_types)
  (subjects : expressions list)
  (ascend : < ascend: expression; .. >)
=
  bind rs ss
    (visit_types tys subjects)
    (ascend#ascend)

(* -------------------------------------------------------------------------- *)

(* [constructor_declaration] turns a constructor declaration (as found in a
   declaration of a sum type) into a case, that is, a branch in the case
   analysis construct that forms the body of the visitor method for this sum
   type. At the same time, it generates several auxiliary method declarations
   and definitions. *)

let constructor_declaration decl (cd : constructor_declaration) : case =
  datacon_opacity_warning cd;
  let datacon = cd.pcd_name.txt in

  (* This is either a traditional data constructor, whose components are
     anonymous, or a data constructor whose components form an ``inline
     record''. This is a new feature of OCaml 4.03. *)

  (* In order to treat these two cases uniformly, we extract the following
     information.
     [xss]      the names under which the components are known.
                this matrix has [length tys] rows -- one per component --
                and [arity] columns.
     [tys]      the types of the components.
     [pss]      the patterns that bind [xss], on the way down.
                this matrix has [arity] rows.
                it has [length tys] columns in the case of tuples,
                and 1 column in the case of inline records.
     [builder]  a function which, applied to the results [rs] of the
                recursive calls, rebuilds a data constructor, on the way up.
  *)

  let xss, tys, pss, (builder : builder) =
  match data_constructor_variety cd with
    (* A traditional data constructor. *)
    | DataTraditional tys ->
        let xss = componentss tys in
        let pss = transpose arity (pvarss xss) in
        xss, tys, pss, fun rs -> constr datacon (evars rs)
    (* An ``inline record'' data constructor. *)
    | DataInlineRecord (labels, tys) ->
        let xss = fieldss labels in
        let pss = transpose arity (pvarss xss) in
        xss, tys,
        map (fun ps -> [precord ~closed:Closed (combine labels ps)]) pss,
        fun rs -> constr datacon [record (combine labels (evars rs))]
  in
  assert (is_matrix (length tys) arity xss);
  assert (length pss = arity);
  let subjects = evarss xss in

  (* Take a [@build] attribute into account. *)
  let builder = ifbuild cd.pcd_attributes builder in

  (* Find out which type variables [alphas] are formal parameters of this
     declaration and are marked [poly]. We have to universally quantify over
     (variants of) these type variables in the type of the hook, below.
     Furthermore, we forbid these type variables from appearing under [@opaque],
     as that would cause us to generate code whose actual type is less general
     than its expected type. *)
  let alphas = poly_params decl in
  check_poly_under_opaque alphas tys;

  (* Create new names [rs] and [ss] for the results of the recursive calls of
     visitor methods. *)
  let rs = results xss
  and ss = summaries xss in

  (* Construct a case for this data constructor in the visitor method associated
     with this sum type. This case analyzes a tuple of width [arity]. After
     binding the components [xss], we call the descending method associated with
     this data constructor. The arguments of this method are:
     1. visitor functions for [poly] type variables;
     2. [env];
     3. [this] (see below);
     4. [xss].
     In this method, we bind the variables [rs] and/or [ss] to the results of
     the recursive calls to visitor methods, then produce a result (whose nature
     depends on [scheme]). *)

  (* If the variety is [endo] (which implies that [arity] is 1), then we bind
     the variable [this] to the whole memory block. This variable is transmitted
     to the descending method. When the time comes to allocate a new memory
     block, if the components of the new block are physically equal to the
     components of the existing block, then the address of the existing block is
     returned; otherwise a new block is allocated, as in [map]. *)

  let ascend =
    new ascend_algebraic
        this subjects rs ss
        builder decl (datacon_ascending_method cd) tys
  in

  Exp.case
    (ptuple (alias this (map (pconstr datacon) pss)))
    (hook X.data
      (datacon_descending_method cd)
      (map tyvar_visitor_function alphas @ env :: transmit this (flatten xss))
      (quantify alphas (ty_arrows
        (map visitor_param_type alphas)
        (visitor_fun_type (transmit (decl_type decl) tys) (decl_type decl))))
      (bulk rs ss tys subjects ascend)
    )

(* -------------------------------------------------------------------------- *)

(* [visit_decl decl] constructs an expression that represents the visiting
   code associated with the type declaration [decl]. In other words, it is
   the body of the visitor method associated with [decl]. *)

let visit_decl (decl : type_declaration) : expression =

  (* Check that the user does not use a reserved type variable name. *)
  decl_params decl |> iter (fun alpha ->
    if mem alpha reserved then
      let loc = decl.ptype_loc in
      raise_errorf ~loc "%s: the type variable name '%s is reserved."
                   plugin alpha
  );

  (* Bind the values to a vector of variables [xs]. *)
  let xs = things in
  assert (length xs = arity);

  match decl.ptype_kind, decl.ptype_manifest with

  (* A type abbreviation. *)
  | Ptype_abstract, Some ty ->
      visit_type true ty

  (* A record type. *)
  | Ptype_record (lds : label_declaration list), _ ->
      let labels, tys = ld_labels lds, ld_tys (fix lds) in
      (* See [constructor_declaration] for comments. *)
      check_poly_under_opaque (poly_params decl) tys;
      let builder rs = record (combine labels (evars rs)) in
      let builder = ifbuild decl.ptype_attributes builder in
      let subjects = accesses xs labels in
      let rs = results labels
      and ss = summaries labels in
      let ascend =
        new ascend_algebraic
          (hd xs) subjects rs ss
          builder decl (tycon_ascending_method decl) tys
      in
      lambdas xs (bulk rs ss tys subjects ascend)

  (* A sum type. *)
  | Ptype_variant (cds : constructor_declaration list), _ ->
      sum_build_warning decl;
      (* Generate one case per data constructor. Place these cases in a
         [match] construct, which itself is placed in a function body. *)
      (* If [arity] is greater than 1 and if there is more than one data
         constructor, then generate also a default case. In this default
         case, invoke the failure method, which raises an exception. The
         failure method receives [env] and [xs] as arguments. *)
      let default() : case =
        Exp.case
          (ptuple (pvars xs))
          (hook true
            (failure_method decl)
            (env :: xs)
            (quantify (poly_params decl) (visitor_method_type decl))
            (efail (local_tycon_visitor_method decl))
          )
      in
      let complete (cs : case list) : case list =
        if arity = 1 || length cs <= 1 then cs else cs @ [ default() ]
      in
      lambdas xs (
        Exp.match_
          (tuple (evars xs))
          (complete (map (constructor_declaration decl) cds))
      )

  (* Unsupported constructs. *)
  | Ptype_abstract, None ->
      let loc = decl.ptype_loc in
      raise_errorf ~loc "%s: cannot deal with abstract types." plugin

  | Ptype_open, _ ->
      let loc = decl.ptype_loc in
      raise_errorf ~loc "%s: cannot deal with open types." plugin

(* -------------------------------------------------------------------------- *)

(* [type_decl decl] generates the main visitor method associated with the type
   declaration [decl], as well as the necessary auxiliary methods. It returns
   no result. *)

let type_decl (decl : type_declaration) : unit =
  let alphas = poly_params decl in
  generate_concrete_method
    (local_tycon_visitor_method decl)
    (lambdas (map tyvar_visitor_function alphas @ [env]) (visit_decl decl))
    (quantify alphas (ty_arrows (map visitor_param_type alphas) (visitor_method_type decl)))

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] processes the type declarations [decl] and produces a
   list of structure items. It is the main entry point inside the body of
   the functor [Setup]. *)

let type_decls (decls : type_declaration list) : structure =
  (* Analyze the type definitions, and populate our classes with methods. *)
  iter type_decl decls;
  (* Emit our preprocessor warnings (if any). *)
  warnings() @
  (* In the generated code, disable certain warnings, so that the user sees
     no warnings, even if she explicitly enables them. We disable warnings
     26, 27 (unused variables) and 4 (fragile pattern matching; a feature
     intentionally exploited by [iter2] and [map2]). *)
  [ with_warnings "-4-26-27" (
    (* Surround the generated code with floating attributes, which can be
       used as markers to find and review the generated code. We use this
       mechanism to show the generated code in the documentation. *)
    floating "VISITORS.BEGIN" [] ::
      (* Produce a class definition. *)
      (* Our classes are parameterized over the type variable ['env]. They are
         also parameterized over the type variable ['self], with a constraint
         that this is the type of [self]. This trick allows us to omit the types
         of the virtual methods, even if these types include type variables. *)
    dump X.concrete X.ancestors [ ty_self, Invariant ] pself X.name ::
    floating "VISITORS.END" [] ::
    []
  )]

end

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] produces a list of structure items (that is, toplevel
   definitions) associated with the type declarations [decls]. It is the
   main entry point outside of the functor [Setup]. *)

let type_decls ~options ~path:_ (decls : type_declaration list) : structure =
  assert (decls <> []);
  let module Process = Setup(Parse(struct
    let loc = (VisitorsList.last decls).ptype_loc (* an approximation *)
    let options = options
    let decls = decls
  end)) in
  Process.type_decls decls

(* -------------------------------------------------------------------------- *)

(* Register our plugin with [ppx_deriving]. *)

let () =
  register (create plugin ~type_decl_str:type_decls ())
