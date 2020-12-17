(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

module Ocaml_ast_mapper = Ast_mapper
open Ppxlib
open StdLabels
open Ast_helper
open Asttypes
open Parsetree

let nolabel = Nolabel

exception Syntax_error of Location.Error.t

let make_exception ~loc ~sub str = Syntax_error (Location.Error.make ~loc ~sub str)

let raise_errorf ~loc fmt =
  Printf.ksprintf (fun str -> make_exception ~loc ~sub:[] str |> raise) fmt

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl ->
    Some
      (List.fold_left
         ~f:(fun p s -> Longident.Ldot (p, s))
         ~init:(Longident.Lident hd)
         tl)

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s ~pos ~len:(dot - pos) :: split_at_dots s (dot + 1)
  with Not_found -> [ String.sub s ~pos ~len:(String.length s - pos) ]

let parse_lid s =
  let components = split_at_dots s 0 in
  let assert_lid =
    String.iteri ~f:(fun i c ->
        match i, c with
        | 0, ('a' .. 'z' | '_') -> ()
        | 0, _ -> assert false
        | _, ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') -> ()
        | _ -> assert false)
  in
  let assert_uid =
    String.iteri ~f:(fun i c ->
        match i, c with
        | 0, 'A' .. 'Z' -> ()
        | 0, _ -> assert false
        | _, ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') -> ()
        | _ -> assert false)
  in
  let rec check = function
    | [] -> assert false
    | "" :: _ -> assert false
    | [ s ] -> assert_lid s
    | modul :: rest ->
      assert_uid modul;
      check rest
  in
  check components;
  match unflatten components with
  | None -> assert false
  | Some v -> v

let mkloc txt loc = { txt; loc }

let mknoloc txt = { txt; loc = Location.none }

let lid ?(loc = !default_loc) s = mkloc (parse_lid s) loc

let mkloc_opt ?(loc = !default_loc) x = mkloc x loc

let unit ?loc ?attrs () =
  Exp.construct ?loc ?attrs (mkloc_opt ?loc (Longident.Lident "()")) None

let tuple ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | [ x ] -> x
  | xs -> Exp.tuple ?loc ?attrs xs

let tuple_type ?loc ?attrs = function
  | [] -> Typ.constr ?loc ?attrs (lid "unit") []
  | [ x ] -> x
  | xs -> Typ.tuple ?loc ?attrs xs

let tuple_pat ?loc ?attrs = function
  | [] -> Pat.any ?loc ?attrs ()
  | [ x ] -> x
  | xs -> Pat.tuple ?loc ?attrs xs

let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const.string s)

(** Check if an expression is an identifier and returns it.
    Raise a Location.error if it's not.
*)
let exp_to_string = function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident s; _ }; _ } -> s
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident s; _ }, None); _ }
    when String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z' ->
    "_" ^ s
  | { pexp_loc; _ } ->
    raise_errorf
      ~loc:pexp_loc
      "Javascript methods or attributes can only be simple identifiers."

let typ s = Typ.constr (lid s) []

(** arg1 -> arg2 -> ... -> ret *)
let arrows args ret =
  List.fold_right args ~init:ret ~f:(fun (l, ty) fun_ -> Typ.arrow l ty fun_)

let wrapper = ref None

let make_str ?loc s =
  match loc with
  | None -> mknoloc s
  | Some loc -> mkloc s loc

(* [merlin_hide] tells merlin to not look at a node, or at any of its
   descendants.  *)
let merlin_hide =
  { attr_name = { txt = "merlin.hide"; loc = Location.none }
  ; attr_payload = PStr []
  ; attr_loc = Location.none
  }

module Js : sig
  val type_ :
    ?loc:Ast_helper.loc -> string -> Parsetree.core_type list -> Parsetree.core_type

  val unsafe :
    ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression

  val fun_ :
    ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression

  val js_dot : string -> string
end = struct
  let js_dot name =
    match !wrapper with
    | None -> "." ^ name
    | Some m -> m ^ "." ^ name

  let js_unsafe_dot name = js_dot ("Unsafe." ^ name)

  let type_ ?loc s args = Typ.constr ?loc (lid ?loc (js_dot s)) args

  let apply_ ~where ?loc s args =
    let args = List.map ~f:(fun x -> nolabel, x) args in
    Exp.(apply ?loc (ident ?loc (lid ?loc (where s))) args)

  let unsafe = apply_ ~where:js_unsafe_dot

  let fun_ = apply_ ~where:js_dot
end

let unescape lab =
  if lab = ""
  then lab
  else
    let lab =
      if lab.[0] = '_' then String.sub lab ~pos:1 ~len:(String.length lab - 1) else lab
    in
    try
      let i = String.rindex lab '_' in
      if i = 0 then raise Not_found;
      String.sub lab ~pos:0 ~len:i
    with Not_found -> lab

let app_arg e = nolabel, e

let inject_arg e = Js.unsafe "inject" [ e ]

let inject_args args = Exp.array (List.map ~f:(fun e -> Js.unsafe "inject" [ e ]) args)

module Arg = struct
  type t = { label : arg_label; names : string list }

  let count = ref 0

  let make ?(label = nolabel) n =
    { label;
      names = List.init ~len:n ~f:(fun _ ->
          let c = !count in
          incr count; "t" ^ string_of_int c) }

  let make1 ?label () = make ?label 1

  let label arg = arg.label

  let names arg = arg.names
  let name arg = match arg.names with
    | [ x ] -> x
    | _ -> assert false

  let types arg = List.map ~f:typ (names arg)
  let typ arg = match names arg with
    | [ x ] -> typ x
    | _ -> assert false

  let args l = List.map ~f:(fun x -> label x, tuple_type (types x)) l
  let args1 l = List.map ~f:(fun x -> label x, typ x) l
end

let js_dot_t_the_first_arg args =
  match args with
  | [] -> assert false
  | x :: xs -> (Arg.label x, Js.type_ "t" [ Arg.typ x ]) :: Arg.args1 xs

(* uplift    : type of the unused value - ties all types together
   downlift  : types of individual components (arguments and result)
*)

let invoker ?(extra_types = []) uplift downlift body (arguments : Arg.t list) =
  let default_loc' = !default_loc in
  default_loc := Location.none;
  let res = "res" in
  let typ_res = typ res in
  let twrap = uplift arguments typ_res in
  let tfunc_args, tfunc_res = downlift arguments typ_res in
  (* Build the main body *)
  let ebody =
    let args = List.map ~f:(fun args -> List.map ~f:(fun d -> Exp.ident (lid d)) (Arg.names args)) arguments in
    body args
  in
  let annotated_ebody = Exp.constraint_ ebody tfunc_res in
  (* Build the function.
     The last arguments is just used to tie all types together.
     It's unused in the implementation.
     {[ fun (t1 : type_of_t1) (t2 : type_of_t2) (_ : uplift_type) -> e]}
  *)
  let labels_and_pats =
    List.map ~f:(fun args ->
        let label = Arg.label args in
        let patts = List.map ~f:(fun d -> Pat.var (mknoloc d)) (Arg.names args) in
        label, patts) arguments
  in
  let make_fun (label, pats) (label', typ) expr =
    assert (label' = label);
    Exp.fun_ label None (Pat.constraint_ (tuple_pat pats) typ) expr
  in
  let invoker =
    List.fold_right2
      labels_and_pats
      tfunc_args
      ~f:make_fun
      ~init:(make_fun (nolabel, [Pat.any ()]) (nolabel, twrap) annotated_ebody)
  in
  (* Introduce all local types:
     {[ fun (type res t0 t1 ..) arg1 arg2 -> e ]}
  *)
  let local_types =
    List.map ~f:make_str
      (res :: extra_types @ (List.flatten @@ List.map ~f:Arg.names arguments))
  in
  let result = List.fold_right local_types ~init:invoker ~f:Exp.newtype in
  default_loc := default_loc';
  result

let open_t loc = Js.type_ ~loc "t" [ Typ.object_ ~loc [] Open ]

(* {[ obj##meth x y ]} generates
   {[
     (
       fun (type res a2 a0 a1) ->
       fun (a2 : a2 Js.t)  ->
       fun (a0 : a0)  ->
       fun (a1 : a1)  ->
       fun (_ : a2 -> a0 -> a1 -> res Js.meth)  ->
         (Js.Unsafe.meth_call a2 "meth"
            [|(Js.Unsafe.inject a0);
              (Js.Unsafe.inject a1)
            |] : res)
     )
       (obj : < .. > Js.t)
       x
       y
       (fun x  -> x#meth)
   ]} *)
let method_call ~loc ~apply_loc obj (meth, meth_loc) args =
  let gloc = { loc with Location.loc_ghost = true } in
  let obj =
    let gloc = { obj.pexp_loc with loc_ghost = true } in
    Exp.constraint_ ~attrs:[ merlin_hide ] ~loc:gloc obj (open_t gloc)
  in
  let invoker =
    invoker
      (fun args tres -> arrows (Arg.args1 args) (Js.type_ "meth" [ tres ]))
      (fun args tres -> js_dot_t_the_first_arg args, tres)
      (fun eargs ->
         let eargs = List.flatten eargs in
         match eargs with
         | eobj :: eargs ->
           let eargs = inject_args eargs in
           Js.unsafe "meth_call" [ eobj; str (unescape meth); eargs ]
         | _ -> assert false)
      (Arg.make1 () :: List.map args ~f:(fun (label, _) -> Arg.make1 ~label ()))
  in
  Exp.apply
    ~loc:apply_loc
    { invoker with pexp_attributes = [ merlin_hide ] }
    ((app_arg obj :: args)
     @ [ app_arg
           (Exp.fun_
              ~loc:gloc
              nolabel
              None
              (Pat.var ~loc:gloc (mknoloc "x"))
              (Exp.send
                 ~loc
                 (Exp.ident ~loc:obj.pexp_loc (lid ~loc:obj.pexp_loc "x"))
                 (make_str ~loc:meth_loc meth)))
       ])

(* {[ obj##.prop ]} generates
   {[
     (
       fun (type res a0) ->
       fun (a0 : a0 Js.t)  ->
       fun (_ : a0 -> < get :res  ;.. > Js.gen_prop)  ->
         (Js.Unsafe.get a0 "prop" : res)
     )
       (obj : < .. > Js.t)
       (fun x -> x#prop)
   ]} *)
let prop_get ~loc obj prop =
  let gloc = { obj.pexp_loc with Location.loc_ghost = true } in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun args tres ->
         let loc = !default_loc in
         arrows
           (Arg.args1 args)
           (Js.type_ "gen_prop" [ [%type: < get : [%t tres] ; .. > ] ]))
      (fun args tres -> js_dot_t_the_first_arg args, tres)
      (fun eargs ->
         match eargs with
         | [ [only_arg] ] -> Js.unsafe "get" [ only_arg; str (unescape prop) ]
         | _ -> assert false)
      [ Arg.make1 () ]
  in
  Exp.apply
    invoker
    [ app_arg obj
    ; app_arg
        (Exp.fun_
           ~loc:gloc
           nolabel
           None
           (Pat.var ~loc:gloc (mknoloc "x"))
           (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) (make_str ~loc prop)))
    ]

(* {[ obj##?prop f ]} generates
   {[
     (
       fun (type res a0 a1) ->
       fun (a0 : a0 Js.t)  ->
       fun (a1 : a1 -> res)  ->
       fun (_ : a0 -> < get : a1 optdef ;.. > Js.gen_prop)  ->
         (try Js.Optdef.map (Js.Unsafe.get a0 "prop") a1  with _ -> undefined : res optdef)
     )
       (obj : < .. > Js.t)
       f
       (fun x -> x#prop)
   ]} *)
let prop_try ~loc obj prop f =
  let gloc = { obj.pexp_loc with Location.loc_ghost = true } in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun args _tres ->
         match args with
         | [ obj_arg; f_arg ] ->
           arrows
             (Arg.args1 [obj_arg])
             (Js.type_ "gen_prop" [ [%type: < get : [%t Js.type_ "optdef" [Arg.typ f_arg]] ; .. > ] ])
         | _ -> assert false)
      (fun args tres ->
         (match args with
          | [ obj_arg; f_arg ] -> [
              Arg.label obj_arg, Js.type_ "t" [Arg.typ obj_arg];
              Arg.label f_arg, Typ.arrow Nolabel (Arg.typ f_arg) tres ]
            | _ -> assert false),
         Js.type_ "optdef" [tres])
      (fun eargs ->
         match eargs with
         | [ [obj]; [f] ] ->
           Exp.try_
             (Js.fun_ "Optdef.map" [ Js.unsafe "get" [ obj; str (unescape prop) ]; f]) [
             Exp.case (Pat.any ()) (Exp.ident (lid "undefined")) ]
         | _ -> assert false)
      [ Arg.make1 (); Arg.make1 () ]
  in
  Exp.apply invoker [
    app_arg obj; app_arg f;
    app_arg @@
    (Exp.fun_ ~loc:gloc nolabel  None (Pat.var ~loc:gloc (mknoloc "x"))
       (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) (make_str ~loc prop))) ]

(* {[ obj##.prop := expr ]} generates
   {[
     (
       fun (type res a1  a0) ->
       fun (a1 : a1 Js.t)  ->
       fun (a0 : a0)  ->
       fun (_ : a1 -> < set :a0 -> unit  ;.. > Js.gen_prop)  ->
         (Js.Unsafe.set a1 "prop" (Js.Unsafe.inject a0) : unit)
     )
       (obj : < .. > Js.t)
       expr
       (fun x -> x#prop)
   ]} *)
let prop_set ~loc ~prop_loc obj prop value =
  let gloc = { obj.pexp_loc with Location.loc_ghost = true } in
  let obj =
    { (Exp.constraint_ ~loc:gloc obj (open_t gloc)) with
      pexp_attributes = [ merlin_hide ]
    }
  in
  let invoker =
    invoker
      (fun args _tres ->
         match args with
         | [ obj; arg ] ->
           let loc = !default_loc in
           assert (Arg.label obj = nolabel);
           assert (Arg.label arg = nolabel);
           arrows
             [ nolabel, Arg.typ obj ]
             (Js.type_ "gen_prop" [ [%type: < set : [%t Arg.typ arg] -> unit ; .. > ] ])
         | _ -> assert false)
      (fun args _tres ->
         let loc = !default_loc in
         js_dot_t_the_first_arg args, [%type: unit])
      (function
        | [ [ obj ]; [ arg ] ] -> Js.unsafe "set" [ obj; str (unescape prop); inject_arg arg ]
        | _ -> assert false)
      [ Arg.make1 (); Arg.make1 () ]
  in
  Exp.apply
    invoker
    [ app_arg obj
    ; app_arg value
    ; app_arg
        (Exp.fun_
           ~loc:{ loc with loc_ghost = true }
           nolabel
           None
           (Pat.var ~loc:gloc (mknoloc "x"))
           (Exp.send
              ~loc:prop_loc
              (Exp.ident ~loc:obj.pexp_loc (lid ~loc:gloc "x"))
              (make_str ~loc prop)))
    ]

(* {[ new%js constr x y ]} generates
   {[
     (
       fun (type res a2 a0 a1) ->
       fun (a2 : (a0 -> a1 -> res Js.t) Js.constr)  ->
       fun (a0 : a0)  ->
       fun (a1 : a1)  ->
       fun (_ : unit)  ->
         (Js.Unsafe.new_obj a2
            [|(Js.Unsafe.inject a0);
              (Js.Unsafe.inject a1)
            |] : res Js.t)
     )
       constr x y ()
   ]}
*)

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let invoker =
    invoker
      (fun _args _tres ->
         let loc = !default_loc in
         [%type: unit])
      (fun args tres ->
         let tres = Js.type_ "t" [ tres ] in
         match args with
         | [] -> assert false
         | unit :: args ->
           assert (Arg.label unit = nolabel);
           let args = Arg.args1 args in
           (nolabel, Js.type_ "constr" [ arrows args tres ]) :: args, tres)
      (function
        | [ constr ] :: args -> Js.unsafe "new_obj" [ constr; inject_args (List.flatten args) ]
        | _ -> assert false)
      (Arg.make1 () :: List.map args ~f:(fun (label, _) -> Arg.make1 ~label ()))
  in
  let gloc = { constr.loc with loc_ghost = true } in
  Exp.apply
    invoker
    ((app_arg (Exp.ident ~loc:constr.loc constr) :: args)
     @ [ app_arg (unit ~loc:gloc ()) ])

module S = Map.Make (String)

(** We remove Pexp_poly as it should never be in the parsetree except after a method call.
*)
let format_meth body =
  match body.pexp_desc with
  | Pexp_poly (e, _) -> e
  | _ -> body

(** Ensure basic sanity rules about fields of a literal object:
    - No duplicated declaration
    - Only relevant declarations (val and method, for now).
*)

module Prop_kind = struct
  type t =
    [ `Readonly
    | `Writeonly
    | `Readwrite
    | `Optdef
    | `Case
    ]

  let prop_type constr ty =
    let constr =
      match constr with
      | `Readonly -> "readonly_prop"
      | `Writeonly -> "writeonly_prop"
      | `Readwrite -> "prop"
      | `Optdef -> "optdef_prop"
      | `Case -> "case_prop"
    in match ty with
    | [ ty ] -> Js.type_ constr [ ty ]
    | l -> tuple_type (List.map ~f:(fun ty -> Js.type_ constr [ ty ]) l)

  let wrap_arg_type constr ty =
    match constr, ty with
    | `Readonly, [ ty ] | `Writeonly, [ ty ] | `Readwrite,  [ ty ] -> ty
    | `Optdef, [ ty ] | `Case, [ ty ] -> Js.type_ "optdef" [ ty ]
    | `Case, l -> tuple_type (List.map ~f:(fun ty -> Js.type_ "optdef" [ ty ]) l)
    | _ -> raise_errorf ~loc:!default_loc "Several types for non case prop"
end

type field_desc =
  | Meth of
      string Asttypes.loc
      * Asttypes.private_flag
      * Asttypes.override_flag
      * Parsetree.expression
      * Arg.t list
  | Val of
      string Asttypes.loc * Prop_kind.t * Asttypes.override_flag * Parsetree.expression
  | Cases of
      string Asttypes.loc list * Parsetree.expression list

let filter_map f l =
  let l =
    List.fold_left l ~init:[] ~f:(fun acc x ->
        match f x with
        | Some x -> x :: acc
        | None -> acc)
  in
  List.rev l

let preprocess_literal_object ?(allow_overload=false) mappper fields :
  [ `Fields of field_desc list | `Error of _ ] =
  let check_name id row names =
    let txt = unescape id.txt in
    if S.mem txt names
    then
      let v = S.find txt names in
      match allow_overload, v, row with
      | true, Val (id', k', _, expr'), Val (_, k, _bang, expr) when
          (k = `Case || k = `Readonly) && (k' = `Case || k' = `Readonly) ->
        let v = Cases ([id'; id], [expr'; expr]) in
        S.add txt v names
      | true, Cases (ids, body), Val (_, k, _bang, expr) when k = `Case ->
        let v = Cases (ids @ [id], body @ [expr]) in
        S.add txt v names
      | _, Val (id', _, _, _), _ | _, Meth (id', _, _, _, _), _ ->
        (* We point out both definitions in locations (more convenient for the user). *)
        let details id =
          if id.txt <> txt then Printf.sprintf " (normalized to %S)" txt else ""
        in
        let sub =
          [ id'.loc, Printf.sprintf "Duplicated val or method %S%s." id'.txt (details id') ]
        in
        make_exception
          ~loc:id.loc
          ~sub
          (Printf.sprintf "Duplicated val or method %S%s." id.txt (details id))
        |> raise
      | _ -> raise_errorf ~loc:id.loc "No ids accumulated"
    else S.add txt row names
  in
  let drop_prefix ~prefix s =
    let prefix_len = String.length prefix in
    if String.length s > prefix_len && String.sub s ~pos:0 ~len:prefix_len = prefix
    then true, String.sub s ~pos:prefix_len ~len:(String.length s - prefix_len)
    else false, s
  in
  let parse_attribute x =
    match drop_prefix ~prefix:"jsoo." x with
    | _, "optdef" -> Some `Optdef
    | _, "writeonly" -> Some `Writeonly
    | _, "readonly" -> Some `Readonly
    | _, "readwrite" -> Some `Readwrite
    | _, "case" -> Some `Case
    | false, _ -> None
    | true, _ -> Some (`Unkown x)
  in
  let jsoo_attributes =
    filter_map (fun { attr_name = { txt; _ }; attr_payload = _; attr_loc = _ } ->
        parse_attribute txt)
  in
  let f names exp =
    match exp.pcf_desc with
    | Pcf_val (id, mut, Cfk_concrete (bang, body)) ->
      let body = mappper body in
      let kind =
        match mut, jsoo_attributes exp.pcf_attributes with
        | Immutable, [] -> `Readonly
        | Mutable, [] -> `Readwrite
        | Immutable, [ `Readonly ] -> `Readonly
        | Immutable, [ `Case ] -> `Case
        | (Immutable | Mutable), [ `Optdef ] -> `Optdef
        | (Immutable | Mutable), [ `Writeonly ] -> `Writeonly
        | (Immutable | Mutable), [ `Readwrite ] -> `Readwrite
        | (Immutable | Mutable), [ `Unkown s ] ->
          raise_errorf ~loc:exp.pcf_loc "Unkown jsoo attribute ([@@%s])." s
        | Mutable, [ `Readonly ] | Mutable, [ `Case ] ->
          raise_errorf ~loc:exp.pcf_loc "A mutable field cannot be readonly."
        | _, _ :: _ :: _ -> raise_errorf ~loc:exp.pcf_loc "Too many attributes."
      in
      check_name id (Val (id, kind, bang, body)) names
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
      let body = format_meth (mappper body) in
      let rec create_meth_ty exp =
        match exp.pexp_desc with
        | Pexp_fun (label, _, _, body) -> Arg.make1 ~label () :: create_meth_ty body
        | _ -> []
      in
      let fun_ty = create_meth_ty body in
      check_name id (Meth (id, priv, bang, body, fun_ty)) names
    | _ ->
      raise_errorf
        ~loc:exp.pcf_loc
        "This field is not valid inside a js literal object."
  in
  try `Fields (snd @@ List.split @@ S.bindings @@ List.fold_left fields ~init:S.empty ~f)
  with Syntax_error error -> `Error (Location.Error.to_extension error)

(* {[ object%js (self)
     val readonlyprop = e1
     val prop = e2
     method meth x = e3
   end ]} generates
   {[
     (
       fun (type res a6 a7 a8 a9) ->
       fun (a7 : a7)  ->
       fun (a8 : a8)  ->
       fun (a9 : res Js.t -> a6 -> a9)  ->
       fun
         (_ :
            res Js.t ->
          a7 Js.readonly_prop ->
          a8 Js.prop ->
          (res Js.t -> a6 -> a9 Js.meth) ->

          res)
         ->
           (Js.Unsafe.obj
              [|("readonlyprop", (Js.Unsafe.inject a7));
                ("prop", (Js.Unsafe.inject a8));
                ("meth", (Js.Unsafe.inject (Js.wrap_meth_callback a9)))
              |] : res Js.t)
     )
       e1
       e2
       (fun self -> fun x  -> e3)
       (fun self readonlyprop prop meth ->
          object
            method readonlyprop = readonlyprop
            method prop = prop
            method meth = meth self
          end)
   ]} *)
let literal_object self_id (fields : field_desc list) =
  let gloc = { !default_loc with Location.loc_ghost = true } in
  let pattern = function
    | Val (id, _, _, _) -> Pat.var ~loc:id.loc id
    | Meth (id, _, _, _, _) -> Pat.var ~loc:id.loc id
    | Cases (ids, _) -> tuple_pat ~loc:gloc (List.map ~f:(fun id -> Pat.var ~loc:id.loc id) ids)
  in
  let body = function
    | Val (_, _, _, body) -> body
    | Meth (_, _, _, body, _) ->
      Exp.fun_ ~loc:{ body.pexp_loc with loc_ghost = true } Nolabel None self_id body
    | Cases (_, body) -> tuple ~loc:!Ast_helper.default_loc body
  in
  let extra_types =
    List.concat
      (List.map fields ~f:(function
           | Val _ -> []
           | Meth (_, _, _, _, l) -> List.map ~f:Arg.name l
           | Cases _ -> []))
  in
  let invoker =
    invoker
      ~extra_types
      (fun args tres ->
         let args = List.map2 ~f:(fun f desc ->
             let ret_ty = Arg.types desc in
             let label = Arg.label desc in
             match f with
             | Val (_, constr, _, _) -> label, Prop_kind.prop_type constr ret_ty
             | Cases _ -> label, Prop_kind.prop_type `Case ret_ty
             | Meth (_, _, _, _, args) ->
               ( label
               , arrows
                   ((nolabel, Js.type_ "t" [ tres ]) :: Arg.args1 args)
                   (Js.type_ "meth" ret_ty) )) fields args in
         arrows ((nolabel, Js.type_ "t" [ tres ]) :: args) tres)
      (fun args tres ->
         let args =
           List.map2 ~f:(fun f desc ->
               let ret_ty = Arg.types desc in
               let label = Arg.label desc in
               match f, ret_ty with
               | Val (_, constr, _, _), _ -> label, Prop_kind.wrap_arg_type constr ret_ty
               | Cases _, _ -> label, Prop_kind.wrap_arg_type `Case ret_ty
               | Meth (_, _, _, _, args), [ ret_ty ] ->
                 label, arrows ((nolabel, Js.type_ "t" [ tres ]) :: Arg.args1 args) ret_ty
               | _ -> raise_errorf ~loc:gloc "several types for method types")
             fields args
         in
         args, Js.type_ "t" [ tres ])
      (fun args ->
         Js.unsafe "obj" [
           Exp.array @@
           List.map2 ~f:(fun f arg -> match f, arg with
               | Val (id, _, _, _), [ arg ] -> tuple [ str (unescape id.txt); inject_arg arg ]
               | Meth (id, _, _, _, _), [ arg ] ->
                 tuple [ str (unescape id.txt); inject_arg @@ Js.fun_ "wrap_meth_callback" [ arg ] ]
               | Cases ((id :: _), _), arg ->
                 tuple [ str (unescape id.txt);
                         inject_arg @@ Js.fun_ "choose_case" [
                           Exp.apply (Exp.ident (mknoloc (Longident.parse "Array.to_list")))
                             [ Nolabel, Exp.array @@ List.map ~f:inject_arg arg ] ] ]
               | _ -> raise_errorf ~loc:gloc "Empty cases"
             ) fields args
         ])
      (List.map fields ~f:(function
           | Val _ | Meth _ -> Arg.make 1
           | Cases (ids, _) -> Arg.make @@ List.length ids))
  in
  let self = "self" in
  let fake_object =
    Exp.object_
      { pcstr_self = Pat.any ~loc:gloc ()
      ; pcstr_fields =
          List.flatten @@ List.map fields ~f:(fun f ->
              let fs = match f with
                | Val (id, _, _, _) -> [ id, Exp.ident ~loc:id.loc (lid ~loc:Location.none id.txt) ]
                | Meth (id, _, _, _, _) ->
                  [ id, Exp.apply (Exp.ident ~loc:id.loc (lid ~loc:Location.none id.txt))
                      [ nolabel, Exp.ident (lid ~loc:Location.none self) ] ]
                | Cases (ids, _) ->
                  List.map ~f:(fun id -> id, Exp.ident ~loc:id.loc (lid ~loc:Location.none id.txt)) ids
              in
              List.map ~f:(fun (id, expr) ->
                  { pcf_loc = id.loc; pcf_attributes = [];
                    pcf_desc = Pcf_method (id, Public, Cfk_concrete (Fresh, expr)) }
                ) fs)
      }
  in
  Exp.apply
    invoker
    (List.map fields ~f:(fun f -> app_arg (body f))
     @ [ app_arg
           { (Exp.fun_ ~loc:gloc nolabel None (Pat.var ~loc:gloc (mknoloc self)) @@
              List.fold_right
                fields
                ~init:fake_object
                ~f:(fun f fun_ ->
                    Exp.fun_ ~loc:gloc nolabel None (pattern f) fun_))
             with
               pexp_attributes = [ merlin_hide ]
           }
       ])

let transform =
  object (self)
    inherit Ast_traverse.map as super

    method! expression expr =
      let prev_default_loc = !default_loc in
      default_loc := expr.pexp_loc;
      let { pexp_attributes; _ } = expr in
      let new_expr =
        match expr with
        (* obj##.var *)
        | [%expr [%e? obj] ##. [%e? meth]] ->
          let obj = self#expression obj in
          let prop = exp_to_string meth in
          let new_expr = prop_get ~loc:meth.pexp_loc obj prop in
          self#expression { new_expr with pexp_attributes }
        (* obj##?var f *)
        | [%expr [%e? obj] ##? [%e? { pexp_desc = Pexp_apply (meth, [f]); _ }]]
        | { pexp_desc = Pexp_apply (([%expr [%e? obj] ##? [%e? meth]]), [f]); _} ->
          let obj = self#expression obj in
          let prop = exp_to_string meth in
          let new_expr = prop_try ~loc:meth.pexp_loc obj prop (snd f) in
          self#expression { new_expr with pexp_attributes }
        (* obj##.var := value *)
        | [%expr [%e? [%expr [%e? obj] ##. [%e? meth]] as prop] := [%e? value]] ->
          let obj = self#expression obj in
          let value = self#expression value in
          let prop_loc = prop.pexp_loc in
          let prop = exp_to_string meth in
          let new_expr = prop_set ~loc:meth.pexp_loc ~prop_loc obj prop value in
          self#expression { new_expr with pexp_attributes }
        (* obj##(meth arg1 arg2) .. *)
        | [%expr [%e? obj] ## [%e? { pexp_desc = Pexp_apply (meth, args); _ }]] ->
          let meth_str = exp_to_string meth in
          let obj = self#expression obj in
          let args = List.map args ~f:(fun (s, e) -> s, self#expression e) in
          let new_expr =
            let loc =
              (* The method call "obj ## meth" node doesn't really exist. *)
              { expr.pexp_loc with loc_ghost = true }
            in

            method_call ~loc ~apply_loc:expr.pexp_loc obj (meth_str, meth.pexp_loc) args
          in
          self#expression { new_expr with pexp_attributes }
        (* obj##meth arg1 arg2 .. *)
        | { pexp_desc = Pexp_apply (([%expr [%e? obj] ## [%e? meth]] as prop), args)
          ; pexp_loc
          ; _
          } ->
          let meth_str = exp_to_string meth in
          let obj = self#expression obj in
          let args = List.map args ~f:(fun (s, e) -> s, self#expression e) in
          let new_expr =
            method_call
              ~loc:prop.pexp_loc
              ~apply_loc:pexp_loc
              obj
              (meth_str, meth.pexp_loc)
              args
          in
          self#expression { new_expr with pexp_attributes }
        (* obj##meth *)
        | [%expr [%e? obj] ## [%e? meth]] as expr ->
          let obj = self#expression obj in
          let meth_str = exp_to_string meth in
          let new_expr =
            method_call
              ~loc:expr.pexp_loc
              ~apply_loc:expr.pexp_loc
              obj
              (meth_str, meth.pexp_loc)
              []
          in
          self#expression { new_expr with pexp_attributes }
        (* new%js constr] *)
        | [%expr [%js [%e? { pexp_desc = Pexp_new constr; _ }]]] ->
          let new_expr = new_object constr [] in
          self#expression { new_expr with pexp_attributes }
        (* new%js constr arg1 arg2 ..)] *)
        | { pexp_desc =
              Pexp_apply ([%expr [%js [%e? { pexp_desc = Pexp_new constr; _ }]]], args)
          ; _
          } ->
          let args = List.map args ~f:(fun (s, e) -> s, self#expression e) in
          let new_expr = new_object constr args in
          self#expression { new_expr with pexp_attributes }
        (* object%js ... end *)
        | [%expr [%js [%e? { pexp_desc = Pexp_object class_struct; _ }]]] ->
          let self_id, allow_overload = match class_struct.pcstr_self.ppat_desc with
            | Ppat_tuple (self_id :: {ppat_desc = Ppat_construct ({txt=Lident "true"; _}, _); _} :: _) -> self_id, true
            | _ -> class_struct.pcstr_self, false in
          let fields =
            preprocess_literal_object ~allow_overload self#expression class_struct.pcstr_fields
          in
          let new_expr =
            match fields with
            | `Fields fields -> literal_object self_id fields
            | `Error e -> Exp.extension e
          in
          self#expression { new_expr with pexp_attributes }
        | _ -> super#expression expr
      in
      default_loc := prev_default_loc;
      new_expr
  end

let () = Driver.register_transformation "ppx_jsoo" ~impl:transform#structure

let mapper =
  let expr _ exp =
    Ppxlib_ast.Selected_ast.of_ocaml Expression exp
    |> transform#expression
    |> Ppxlib_ast.Selected_ast.to_ocaml Expression
  in
  { Ocaml_ast_mapper.default_mapper with expr }

let () = wrapper := Some "Ezjs_min"
