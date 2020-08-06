open Core
open Webidl.Data
open Common

let keywords = [ "type"; "module" ] |> String.Set.of_list

module Which = struct
  type t =
    | Ml
    | Mli

  let hat = function
    | Ml -> " = struct"
    | Mli -> " : sig"
  ;;
end

module Name : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create_type : string -> t
  val create_module : string -> t
  val create_value : string -> t
  val to_string : t -> string
end = struct
  include String

  let create_module s =
    if not (Char.is_uppercase s.[0])
    then raise_s [%message "bad module name" s];
    s
  ;;

  let create_value s =
    if Char.is_uppercase s.[0]
    then "_" ^ s
    else if Set.mem keywords s
    then s ^ "_"
    else s
  ;;

  let create_type s = if Set.mem keywords s then s ^ "_" else s
  let to_string t = t
end

exception Not_implemented

module Errors = struct
  let t : Error.t list ref = ref []
  let add e = t := e :: !t
  let add_s s = t := Error.create_s s :: !t

  let get () =
    let result = !t in
    t := [];
    List.rev result
  ;;
end

let try_with_error ~name f =
  match Or_error.try_with ~backtrace:true f with
  | Ok result -> Some result
  | Error e ->
    Error.tag e ~tag:(Name.to_string name) |> Errors.add;
    None
;;

module Type = struct
  open Webidl_syntax.Ast

  module Array = struct
    type t =
      | ArrayBuffer
      | DataView
      | Int8Array
      | Int16Array
      | Int32Array
      | Uint8Array
      | Uint16Array
      | Uint32Array
      | Float32Array
      | Float64Array
    [@@deriving sexp]

    let to_string t =
      let s = sexp_of_t t |> Sexp.to_string |> Bytes.of_string in
      Bytes.set s 0 (Char.lowercase (Bytes.get s 0));
      Bytes.to_string s
    ;;
  end

  type t =
    | Unit
    | Any
    | Bool
    | String
    | Float
    | Int
    | Array of Array.t
    | Optdef of t
    | Promise of t
    | Sequence of t
    | Union of t list
    | Ident of Name.t
  [@@deriving sexp]

  let map_children t ~f =
    match t with
    | Unit | Any | Bool | String | Float | Int | Ident _ | Array _ -> []
    | Promise t | Sequence t | Optdef t -> [ f t ]
    | Union ts -> List.map ts ~f
  ;;

  let rec deps = function
    | Ident name -> Name.Set.singleton name
    | t -> map_children t ~f:deps |> Name.Set.union_list
  ;;

  let rec to_string = function
    | Unit -> "unit"
    | Any -> "any"
    | Bool -> "bool Js.t"
    | Int -> "int"
    | Float -> "float"
    | String -> "js_string Js.t"
    | Array t -> sprintf !"%{Array} Js.t" t
    | Optdef t -> sprintf !"%{to_string} optdef" t
    | Promise t -> sprintf !"(%{to_string}) Promise.t Js.t" t
    | Ident name ->
      let s = Name.to_string name in
      if Char.is_uppercase s.[0] then sprintf "%s.t" s else s
    | Sequence t -> sprintf !"(%{to_string}) js_array Js.t" t
    | Union ts ->
      let rec loop = function
        | [] -> raise_s [%message "union of zero types"]
        | [ t ] -> to_string t
        | t :: ts -> sprintf !"(%{to_string}, %s) Union.t Js.t" t (loop ts)
      in
      loop ts
  ;;

  let to_string t =
    try to_string t with
    | Not_implemented -> raise_s [%message "no to_string" (t : t)]
  ;;

  let rec primitive_exn : primitive -> t = function
    | `Boolean -> Bool
    | `Float | `Double -> Float
    | `Short | `Long | `LongLong | `Unsigned (`Short | `Long | `LongLong)
      -> Int
    | `Byte | `Octet | `Unrestricted (`Float | `Double) ->
      raise Not_implemented

  and string_type_exn : string_type -> t = function
    | `ByteString | `DOMString | `USVString -> String

  and buffer_exn : buffer -> t =
   fun t ->
    let t : Array.t =
      match t with
      | `ArrayBuffer -> ArrayBuffer
      | `DataView -> DataView
      | `Int8Array -> Int8Array
      | `Int16Array -> Int16Array
      | `Int32Array -> Int32Array
      | `Uint8Array -> Uint8Array
      | `Uint16Array -> Uint16Array
      | `Uint32Array -> Uint32Array
      | `Float32Array -> Float32Array
      | `Float64Array -> Float64Array
      | `Uint8Clampedarray -> raise Not_implemented
    in
    Array t

  and nullable_non_any_aux_exn : type_with_ext nullable_non_any_aux -> t
    = function
    | #primitive as p -> primitive_exn p
    | #string_type as s -> string_type_exn s
    | #buffer as b -> buffer_exn b
    | `Sequence t -> Sequence (type_with_ext t)
    | `Ident s ->
      let name = Name.create_type s in
      Ident name
    | `Object | `Error | `DomException -> raise Not_implemented
    | `FrozenArray t ->
      ignore (type_with_ext t : t);
      raise Not_implemented
    | `Record (s, t) ->
      ignore (string_type_exn s : t);
      ignore (type_with_ext t : t);
      raise Not_implemented

  and nullable_union_aux_exn
      : (type_with_ext, union_type) nullable_union_aux -> t
    = function
    | `Union t -> union_type_aux_exn t
    | #nullable_non_any_aux as t -> nullable_non_any_aux_exn t

  and non_any_aux_exn : (type_with_ext, return_type) non_any_aux -> t
    = function
    | `Promise rt -> Promise (return_type_aux_exn rt)
    | `Nullable t -> nullable_non_any_aux_exn t
    | #nullable_non_any_aux as t -> nullable_non_any_aux_exn t

  and union_member_aux_exn
      :  (type_with_ext, return_type, union_type, extends) union_member_aux
      -> t
    = function
    | `NonAny ((_ : extends), t) -> non_any_aux_exn t
    | `Union t -> union_type_aux_exn t
    | `Nullable (`Union t) -> union_type_aux_exn t

  and union_type_aux_exn
      :  (type_with_ext, return_type, union_type, extends) union_type_aux
      -> t
    =
   fun ts -> Union (List.map ~f:union_member_aux_exn ts)

  and type_with_ext (_ext, t) = type_aux_exn t

  and nullable_union_aux
      : (type_with_ext, union_type) nullable_union_aux -> t
    = function
    | #nullable_non_any_aux as n -> nullable_non_any_aux_exn n
    | `Union t -> union_type_aux_exn t

  and type_aux_exn
      : ('type_with_ext, 'return_type, 'union_type) type_aux -> t
    = function
    | `Promise rt -> Promise (return_type_aux_exn rt)
    | #nullable_non_any_aux as n -> nullable_non_any_aux_exn n
    | `Any -> Any
    | `Nullable t -> nullable_union_aux_exn t
    | `Union t -> union_type_aux_exn t

  and return_type_aux_exn : return_type -> t = function
    | `Any -> Any
    | `Promise rt -> Promise (return_type_aux_exn rt)
    | `Nullable rt -> nullable_union_aux rt
    | `Union rt -> union_type_aux_exn rt
    | #nullable_non_any_aux as n -> nullable_non_any_aux_exn n
    | `Void -> Unit

  and const_exn : const -> t = function
    | #primitive as t -> primitive_exn t
    | `Ident s -> Ident (Name.create_type s)

  and const_type_exn : const_type -> t = function
    | #const as t -> const_exn t
    | `Nullable t -> const_exn t
  ;;

  let return_type_exn = return_type_aux_exn
  let type_exn = type_aux_exn
end

module Arg = struct
  open Webidl_syntax.Ast

  type t =
    { name : string
    ; typ : Type.t
    }
  [@@deriving sexp]

  let deps { name = _; typ } = Type.deps typ

  let argument_exn : argument -> t = function
    | `Variadic _ -> raise Not_implemented
    | `Optional ((_ext, t), name, _default) ->
      let typ = Type.Optdef (Type.type_exn t) in
      { name; typ }
    | `Fixed (t, name) ->
      let typ = Type.type_exn t in
      { name; typ }
  ;;

  let to_code { name; typ } = sprintf "%s:%s" name (Type.to_string typ)
end

module Method = struct
  type t =
    { name : Name.t
    ; return_type : Type.t
    ; args : Arg.t list
    }
  [@@deriving sexp]

  let deps { name = _; return_type; args } =
    Type.deps return_type :: List.map args ~f:Arg.deps
    |> Name.Set.union_list
  ;;

  let operation_exn
      { specials = _; is_static = _; return_type = rt; ident; arguments }
    =
    let return_type = Type.return_type_exn rt in
    let args =
      List.map arguments ~f:(fun (_extends, arg) -> Arg.argument_exn arg)
    in
    let name = Option.value_exn ident |> Name.create_value in
    { name; return_type; args }
  ;;

  let class_member { name; return_type; args } =
    let return_type = Type.to_string return_type in
    match args with
    | [] -> sprintf !"method %{Name} : %s meth" name return_type
    | args ->
      let args =
        List.map args ~f:Arg.to_code |> String.concat ~sep:" -> "
      in
      sprintf !"method %{Name} : %s -> %s meth" name args return_type
  ;;
end

module Property = struct
  type t =
    { name : Name.t
    ; typ : Type.t
    ; is_readonly : bool
    ; is_required : bool
    }
  [@@deriving sexp]

  let deps { name = _; is_readonly = _; typ; is_required = _ } =
    Type.deps typ
  ;;

  let dictionary_member_exn
      { is_required; type_with_ext = _, typ; ident; default = _ }
    =
    let typ = Type.type_exn typ in
    let typ = if is_required then typ else Type.Optdef typ in
    let name = Name.create_value ident in
    { name; typ; is_readonly = true; is_required }
  ;;

  let attribute_exn
      { is_static = _
      ; is_readonly
      ; is_inherit = _
      ; type_with_ext = _, typ
      ; name
      }
    =
    let typ = Type.type_exn typ in
    let name = Name.create_value name in
    { name; typ; is_readonly; is_required = true }
  ;;

  let class_member { name; typ; is_readonly; _ } =
    let typ = Type.to_string typ in
    if is_readonly
    then sprintf !"method %{Name} : %s readonly_prop" name typ
    else sprintf !"method %{Name} : %s prop" name typ
  ;;

  let object_member { name; is_required; _ } =
    if is_required
    then sprintf !"val %{Name} = %{Name}" name name
    else sprintf !"val %{Name} = Optdef.option %{Name}" name name
  ;;

  let constructor_arg (which : Which.t) { name; typ; is_required; _ } =
    match which with
    | Mli ->
      let q = if is_required then "" else "?" in
      let typ =
        match is_required, typ with
        | false, Optdef typ -> typ
        | false, typ ->
          raise_s
            [%message
              "unexpected type for non-required option" (typ : Type.t)]
        | true, _ -> typ
      in
      sprintf !"%s%{Name}:%{Type}" q name typ
    | Ml ->
      let q = if is_required then "~" else "?" in
      sprintf !"%s%{Name}" q name
  ;;
end

module Const = struct
  module Value = struct
    type t = Webidl_syntax.Ast.const_value [@@deriving sexp]

    let to_string : t -> string = function
      | `Bool x -> if x then "js._true" else "Js._false"
      | `Float x -> Float.to_string x
      | `Int x -> Int.to_string x
      | `Null -> "Js.null"
    ;;
  end

  type t =
    { name : Name.t
    ; typ : Type.t
    ; value : Value.t
    }
  [@@deriving sexp]

  let deps { typ; name = _; value = _ } = Type.deps typ

  let const (typ, name, value) =
    let typ = Type.const_type_exn typ in
    let name = Name.create_value name in
    { typ; name; value }
  ;;

  let to_code (which : Which.t) { name; typ; value } =
    match which with
    | Ml ->
      let value = Value.to_string value in
      sprintf !"let %{Name} = %s" name value
    | Mli -> sprintf !"val %{Name} : %{Type}" name typ
  ;;
end

module Module = struct
  module Js_member = struct
    type t =
      | Method of Method.t
      | Property of Property.t
    [@@deriving sexp]

    let deps = function
      | Method m -> Method.deps m
      | Property p -> Property.deps p
    ;;

    let class_member = function
      | Method m -> Method.class_member m
      | Property p -> Property.class_member p
    ;;
  end

  module Member = struct
    type t =
      | Js of Js_member.t
      | Const of Const.t
    [@@deriving sexp, variants]

    let deps = function
      | Js m -> Js_member.deps m
      | Const c -> Const.deps c
    ;;

    let get_js = function
      | Js x -> Some x
      | _ -> None
    ;;

    let get_const = function
      | Const x -> Some x
      | _ -> None
    ;;

    let interface_member_exn : interface_member -> t = function
      | `Operation op -> Js (Method (Method.operation_exn op))
      | `Attribute x -> Js (Property (Property.attribute_exn x))
      | `Const x -> Const (Const.const x)
      | `Stringifier _ | `Iterable _ | `Maplike _ | `Setlike _ ->
        raise Not_implemented
    ;;

    let mixin_member_exn : mixin_member -> t = function
      | `Const x -> Const (Const.const x)
      | `Operation op -> Js (Method (Method.operation_exn op))
      | `Attribute x -> Js (Property (Property.attribute_exn x))
      | `Stringifier _ | `Maplike _ | `Setlike _ -> raise Not_implemented
    ;;
  end

  module Dict = struct
    type t =
      { properties : Property.t list
      ; mutable base : t option
      }
    [@@deriving sexp]

    let deps { properties; base = _ } =
      List.map properties ~f:Property.deps |> Name.Set.union_list
    ;;

    let rec all_properties t =
      let base_properties =
        match t.base with
        | None -> []
        | Some base -> all_properties base
      in
      base_properties @ t.properties
    ;;
  end

  module Kind = struct
    type t =
      | Interface of Member.t list
      | Dict of Dict.t
    [@@deriving sexp]

    let deps = function
      | Interface members ->
        List.map members ~f:Member.deps |> Name.Set.union_list
      | Dict dict -> Dict.deps dict
    ;;
  end

  type t =
    { name : Name.t
    ; mutable inherits : Name.t list
    ; kind : Kind.t
    }
  [@@deriving fields, sexp]

  let deps { name = _; kind; inherits } =
    let deps = Kind.deps kind in
    Set.union deps (Name.Set.of_list inherits)
  ;;

  let interface { ident; inheritance; interface_members } =
    let name = Name.create_module ident in
    let members =
      List.filter_map interface_members ~f:(fun (_extends, member) ->
          try_with_error ~name (fun () ->
              Member.interface_member_exn member))
    in
    let inherits =
      Option.map inheritance ~f:Name.create_module |> Option.to_list
    in
    { name; inherits; kind = Interface members }
  ;;

  let mixin { ident; mixin_members } =
    let name = Name.create_module ident in
    let members =
      List.filter_map mixin_members ~f:(fun (_extends, member) ->
          try_with_error ~name (fun () -> Member.mixin_member_exn member))
    in
    { name; inherits = []; kind = Interface members }
  ;;

  let dictionary { ident; inheritance; dictionary_members } =
    let name = Name.create_module ident in
    let properties =
      List.filter_map dictionary_members ~f:(fun (_extends, member) ->
          try_with_error ~name (fun () ->
              Property.dictionary_member_exn member))
    in
    let inherits =
      Option.map inheritance ~f:Name.create_module |> Option.to_list
    in
    let dict = { Dict.properties; base = None } in
    { name; kind = Dict dict; inherits }
  ;;

  let dict_create_code (which : Which.t) properties =
    match which with
    | Mli ->
      [ sprintf "val create : " ]
      @ (List.map properties ~f:(Property.constructor_arg which)
        |> List.map ~f:(sprintf "%s -> "))
      @ [ "unit -> t" ]
    | Ml ->
      [ "let create" ]
      @ List.map properties ~f:(Property.constructor_arg which)
      @ [ "() = object%js" ]
      @ List.map properties ~f:Property.object_member
      @ [ "end" ]
  ;;

  let to_code which { name; kind; inherits } =
    let inherits =
      List.dedup_and_sort inherits ~compare:Name.compare
      |> List.map ~f:(fun name -> sprintf !"inherit %{Name}.js" name)
    in
    let body =
      match kind with
      | Interface members ->
        let js_members =
          List.filter_map members ~f:Member.get_js
          |> List.map ~f:Js_member.class_member
        in
        let consts = List.filter_map members ~f:Member.get_const in
        let consts =
          List.map consts ~f:(fun const -> Const.to_code which const)
        in
        [ "type witness"
        ; "class type js = object"
        ; sprintf
            !"method %s_witness : witness"
            (Name.to_string name |> String.lowercase)
        ]
        @ inherits
        @ js_members
        @ [ "end"; "type t = js Js.t" ]
        @ consts
      | Dict dict ->
        [ "class type js = object" ]
        @ inherits
        @ List.map ~f:Property.class_member dict.properties
        @ [ "end"; "type t = js Js.t" ]
        @ dict_create_code which (Dict.all_properties dict)
    in
    [ sprintf !"module %{Name} %s" name (Which.hat which) ]
    @ body
    @ [ "end" ]
  ;;
end

module Typedef = struct
  type t = Name.t * Type.t [@@deriving sexp]

  let deps (_, typ) = Type.deps typ
  let name (name, _) = name

  let create ((_ext, t), s) =
    let open Option.Let_syntax in
    let name = Name.create_type s in
    let%bind t = try_with_error ~name (fun () -> Type.type_exn t) in
    Some (name, t)
  ;;

  let to_code which (name, t) =
    let name = Name.to_string name in
    if Char.is_uppercase name.[0]
    then
      [ sprintf !"module %s %s" name (Which.hat which)
      ; sprintf !"type t = %{Type}" t
      ; "end"
      ]
    else [ sprintf !"type %s = %{Type}" name t ]
  ;;
end

module Includes = struct
  type t =
    { parent : Name.t
    ; child : Name.t
    }
  [@@deriving sexp]

  let deps { parent; child } = [ parent; child ] |> Name.Set.of_list
end

module Def = struct
  type t =
    | Module of Module.t
    | Typedef of Typedef.t
    | Includes of Includes.t
  [@@deriving sexp]

  let deps = function
    | Module m -> Module.deps m
    | Typedef def -> Typedef.deps def
    | Includes inc -> Includes.deps inc
  ;;

  let name = function
    | Module m -> Some (Module.name m)
    | Typedef def -> Some (Typedef.name def)
    | Includes _ -> None
  ;;

  let depends_on t1 t2 =
    match name t2 with
    | None -> false
    | Some name2 -> Set.mem (deps t1) name2
  ;;

  let create (_extends, definition) =
    let open Option.Let_syntax in
    match definition with
    | `Interface i -> Some (Module (Module.interface i))
    | `Mixin x -> Some (Module (Module.mixin x))
    | `Dictionary d -> Some (Module (Module.dictionary d))
    | `Typedef def ->
      let%bind def = Typedef.create def in
      Some (Typedef def)
    | `Enum (ident, _) ->
      (* CR-someday: make proper *)
      Some (Typedef (Name.create_type ident, Type.String))
    | `Includes (child, parent) ->
      let child = Name.create_module child in
      let parent = Name.create_module parent in
      Some (Includes { child; parent })
    | `Callback _ | `Namespace _ | `Partial _ | `Implements _ ->
      Errors.add_s [%message "unsupported" (definition : definition)];
      None
  ;;

  (* Effectively idempotent (we dedup the inehrits list). *)
  let update_with_deps t_deps t =
    match t with
    | Typedef _ -> ()
    | Includes { parent; child } ->
      let child =
        match Map.find_exn t_deps child with
        | Module child -> child
        | _ -> assert false
      in
      child.inherits <- parent :: child.inherits
    | Module { kind = Dict dict; inherits = [ base_name ]; _ } ->
      let base =
        match Map.find_exn t_deps base_name with
        | Module { kind = Dict base; _ } -> base
        | _ -> assert false
      in
      dict.base <- Some base
    | Module _ -> ()
  ;;

  let to_code which = function
    | Module m -> Module.to_code which m
    | Typedef def -> Typedef.to_code which def
    | Includes _ -> []
  ;;
end

type t =
  { defs : Def.t list
  ; errors : Error.t list
  }
[@@deriving fields, sexp]

let fold_with_deps =
  List.folding_map ~init:Name.Map.empty ~f:(fun dep_map def ->
      let deps = Map.key_set dep_map in
      let missing = Set.diff (Def.deps def) deps in
      if Set.is_empty missing
      then (
        Def.update_with_deps dep_map def;
        let dep_map =
          match Def.name def with
          | None -> dep_map
          | Some name -> Map.add_exn dep_map ~key:name ~data:def
        in
        dep_map, Some def)
      else (
        Errors.add_s
          [%message
            "dropping due to missing dep"
              (def : Def.t)
              (missing : Name.Set.t)];
        dep_map, None))
;;

let create (defs : definitions) =
  let single_pass defs =
    topsort defs ~gt:Def.depends_on
    |> fold_with_deps
    |> List.filter_map ~f:Fn.id
  in
  let defs =
    List.filter_map defs ~f:Def.create |> single_pass |> single_pass
  in
  let errors = Errors.get () in
  { defs; errors }
;;

let header =
  [ "open Js_of_ocaml"
  ; "open Js"
  ; "open Js.Unsafe"
  ; "open Typed_array"
  ; "open Js_of_ocaml_webidl.Runtime"
  ]
;;

let mli t = header @ List.concat_map t.defs ~f:(Def.to_code Mli)
let ml t = header @ List.concat_map t.defs ~f:(Def.to_code Ml)
