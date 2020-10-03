(*****************************************************************************)
(** This file is an extension for the Abstract1 module from the apron Library.
    It is not meant to be used as it is but through the instanciated modules
    Abox, Apol and Aoct *)
(*****************************************************************************)

open Apron

(** Abstract domain signature used by the following functor *)
module type ADomain = sig
  (** Abstract element type *)
  type t

  (** Apron manager *)
  val man: t Manager.t
end

(** functor that allows hiding the use of the manager, It also adds
   few utilities to the Abstract1 module *)
module Make(D:ADomain) = struct
  (** This functor implements all the constraint/generator based
     operations over abstract elements. These are generic and stand
     for Boxes, Octagons and Polyhedra.  *)

  (** Conventions :
  - functions ending with _s allow to use/return string instead of variables
  - functions ending with _f allow to use/return float instead of apron scalar
  - functions ending with _fs mix the two
   *)

  module A = Abstract1
  module G = Generatorext
  module T = Tconsext
  module L = Linconsext
  module E = Environmentext

  open A
  type t = D.t A.t

  let man = D.man

  (** Set-theoritic operations *)

  let bottom = bottom man

  let top = top man

  let join = join man

  let meet = meet man

  let widening = widening man

  (** predicates *)

  let is_bottom = is_bottom man

  let is_top = is_top man

  let is_leq = is_leq man

  let is_eq = is_eq man

  (** constraint satisfaction and filter *)

  let sat_lincons = sat_lincons man

  let sat_tcons = sat_tcons man

  let filter_lincons abs l =
    let ear = L.array_make abs.env 1 in
    L.array_set ear 0 l;
    meet_lincons_array man abs ear

  let filter_tcons abs l =
    let ear = T.array_make abs.env 1 in
    T.array_set ear 0 l;
    meet_tcons_array man abs ear

  (** of and to constraints/generator *)

  let to_lincons_array = to_lincons_array man

  let to_tcons_array = to_tcons_array man

  let to_generator_array = to_generator_array man

  let to_lincons_list e = to_lincons_array e |> L.array_to_list

  let to_tcons_list e = to_tcons_array e |> T.array_to_list

  let to_generator_list e = to_generator_array e |> G.array_to_list

  let of_generator_list (e : E.t) (g : Generator1.t list) =
    let open Generator1 in
    let _,lray = List.partition (fun g -> get_typ g = VERTEX) g in
    let ofvertice v =
      E.fold (fun acc var ->
          let c = Texprext.cst e (get_coeff v var) in
          assign_texpr man acc var c None
        ) (top e) e
    in
    let closed = join_array man (Array.of_list (List.map ofvertice g)) in
    Generatorext.array_of_list lray |> add_ray_array man closed

  let of_generator_array (e : E.t) g =
    of_generator_list e (G.array_to_list g)

  let of_lincons_array = of_lincons_array man

  let of_tcons_array = of_tcons_array man

  let of_lincons_list env l = of_lincons_array env (L.array_of_list l)

  let of_tcons_list env l = of_tcons_array env (T.array_of_list l)

  let of_box = of_box man

  (** Environment and variable related operations *)

  let get_environment a = env a

  let change_environment a e = change_environment man a e false

  (*FIXME : What is the purpose of None?*)
  let assign_texpr abs var texpr = assign_texpr man abs var texpr None

  (*FIXME : What is the purpose of None?*)
  let assign_linexpr abs var linexpr = assign_linexpr man abs var linexpr None

  let assign_f abs var f =
    let texpr = Texprext.cst_f f |> Texprext.of_expr abs.env in
    assign_texpr abs var texpr

  let assign_fs abs var f = assign_f abs (Var.of_string var) f

  (* utilities*)
  let add_var abs typ v =
    let e = env abs in
    let ints,reals =
      if typ = Environment.INT then [|v|],[||]
      else [||],[|v|]
    in
    let env = Environment.add e ints reals in
    A.change_environment man abs env false

  let add_var_s abs typ v = add_var abs typ (Var.of_string v)

  let bound_variable abs v = bound_variable man abs v

  let bound_variable_f abs v =
    let open Intervalext in
    bound_variable abs v |> to_float

  let bound_variable_s abs v = bound_variable abs (Var.of_string v)

  let bound_variable_fs abs v = bound_variable_f abs (Var.of_string v)

  let is_bounded_variable abs v = Intervalext.is_bounded (bound_variable abs v)

  let is_bounded_s abs v = is_bounded_variable abs (Var.of_string v)

  let is_bounded abs =
    let env = env abs in
    try
      E.iter (fun v -> if is_bounded_variable abs v |> not then raise Exit) env;
      true
    with Exit -> false

  (** Cross-domain conversion *)
  let to_box abs =
    let env = env abs in
    let abs' = A.change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Box.manager_alloc ()) env

  let to_oct abs =
    let env = env abs in
    to_lincons_array abs |> A.of_lincons_array (Oct.manager_alloc ()) env

  let to_poly abs =
    let env = env abs in
    let abs' = A.change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Polka.manager_alloc_strict ()) env

  (** Printing *)
  let print fmt a =
    (* print fmt a *)
    let constraints = to_lincons_list a in
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
         Linconsext.print) constraints

  (** Projection on 2 dimensions *)
  let proj2D abs v1 v2 =
    let env = env abs in
    let new_env = Environmentext.empty in
    let new_env =
      if Array.exists ((=) v1) (Environmentext.get_ints env) then
        Environmentext.add_int v1 new_env
      else Environmentext.add_real v1 new_env
    in
    let new_env =
      if Array.exists ((=) v2) (Environmentext.get_ints env) then
        Environmentext.add_int v2 new_env
      else Environmentext.add_real v2 new_env
    in
    change_environment abs new_env

  (** Projection on 3 dimensions *)
  let proj3D abs v1 v2 v3 =
    let env = env abs in
    let new_env = Environmentext.empty in
    let new_env =
      if Array.exists ((=) v1) (Environmentext.get_ints env) then
        Environmentext.add_int v1 new_env
      else Environmentext.add_real v1 new_env
    in
    let new_env =
      if Array.exists ((=) v2) (Environmentext.get_ints env) then
        Environmentext.add_int v2 new_env
      else Environmentext.add_real v2 new_env
    in
    let new_env =
      if Array.exists ((=) v3) (Environmentext.get_ints env) then
        Environmentext.add_int v3 new_env
      else Environmentext.add_real v3 new_env
    in change_environment abs new_env

  (** Projection on 2 dimensions with string as variables *)
  let proj2D_s abs v1 v2 =
    proj2D abs (Apron.Var.of_string v1) (Apron.Var.of_string v2)

  (** Projection on 3 dimensions with string as variables *)
  let proj3D_s abs v1 v2 v3 =
    proj3D abs (Apron.Var.of_string v1) (Apron.Var.of_string v2) (Apron.Var.of_string v3)

  (** returns the vertices of an abstract element projected on 2 dimensions *)
  let to_vertices2D abs v1 v2 =
    let gen' = to_generator_array abs in
    let get_coord l = Apron.Linexpr1.(get_coeff l v1, get_coeff l v2) in
    Array.init
      (Generatorext.array_length gen')
	    (fun i -> get_coord
	                (Generatorext.(get_linexpr1 (array_get gen' i))))

    |> Array.to_list
    |> List.rev_map (fun(a,b)-> (Coeffext.to_float a, Coeffext.to_float b))

  (** returns the vertices of an abstract element projected on 2 dimensions *)
  let to_vertices2D_s abs v1 v2 =
    to_vertices2D abs (Apron.Var.of_string v1) (Apron.Var.of_string v2)

end
