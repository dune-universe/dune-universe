open! Import

module Format = Caml.Format

(* set of matches for "foo.bar.blah":
   - "foo.bar.blah"
   -     "bar.blah"
   -         "blah"
*)
let matches ~pattern matched =
  String.equal pattern matched || (
    let len_pattern = String.length pattern in
    let len_matched = String.length matched in
    let start = len_pattern - len_matched in
    start > 0 && Char.equal pattern.[start - 1] '.' &&
    (
      let i = ref 0 in
      while !i < len_matched &&
            Char.equal
              (String.unsafe_get matched !i)
              (String.unsafe_get pattern (start + !i))
      do
        i := !i + 1
      done;
      !i = len_matched
    )
  )

let fold_dot_suffixes name ~init:acc ~f =
  let rec loop pos acc =
    if pos >= 0 then
      match String.rindex_from name pos '.' with
      | None -> f name acc
      | Some i ->
        let sub_name = String.sub name ~pos:(i + 1) ~len:(String.length name - (i + 1)) in
        loop (i - 1) (f sub_name acc)
    else
      acc
  in
  loop (String.length name - 1) acc
;;

let get_outer_namespace name =
  match String.index name '.' with
  | None -> None
  | Some i -> Some (String.sub name ~pos:0 ~len:i)

module Whitelisted = struct
  (* White list the following attributes, as well as all their dot suffixes.

     Since these attributes are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.

     Sadly, the compiler silently ignores them if they are misplaced...
  *)
 let create_set fully_qualified_names =
    List.fold_left
      ~f:(fun acc name ->
        fold_dot_suffixes name ~init:acc ~f:(fun x acc -> Set.add acc x))
      ~init:(Set.empty (module String))
      fully_qualified_names

 let attributes =
   create_set
      [ "ocaml.warning"
      ; "ocaml.ppwarning"
      ; "ocaml.deprecated"
      ; "ocaml.doc"
      ; "ocaml.text"
      ; "ocaml.noalloc"
      ; "ocaml.unboxed"
      ; "ocaml.untagged"
      ; "ocaml.inline"
      ; "ocaml.inlined"
      ; "ocaml.specialise"
      ; "ocaml.specialised"
      ; "ocaml.unroll"
      ; "immediate"
      ]

  (* White list the following extensions.

     Since these extensions are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.
  *)
  let extensions =
    create_set
      [ "ocaml.error"
      ; "ocaml.extension_constructor"
      ]

  let is_whitelisted ~kind name =
    match kind with
    | `Attribute -> Set.mem attributes name
    | `Extension -> Set.mem extensions name

  let get_attribute_list () = Set.elements attributes
  let get_extension_list () = Set.elements extensions
end

module Reserved_namespaces = struct
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create (module String) ()

  let reserve ns = Hashtbl.add_exn tbl ~key:ns ~data:()

  let () = reserve "merlin"

  let is_in_reserved_namespaces name =
    match get_outer_namespace name with
    | Some ns -> Hashtbl.mem tbl ns
    | _ -> false

  let check_not_reserved ~kind name =
    let kind, list =
      match kind with
      | `Attribute -> "attribute", Whitelisted.attributes
      | `Extension -> "extension", Whitelisted.extensions
    in
    if Set.mem list name then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as it matches an \
         %s reserved by the compiler"
        kind name kind
    else if is_in_reserved_namespaces name then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as its namespace \
         is marked as reserved"
        kind name

end

let comes_from_merlin name =
  match get_outer_namespace name with
  | Some "merlin" -> true
  | _ -> false

module Registrar = struct
  type element =
    { fully_qualified_name : string
    ; declared_at          : Caller_id.t
    }

  type all_for_context = { mutable all : element Map.M(String).t }

  type 'a t =
    { all_by_context    : ('a, all_for_context) Hashtbl.t
    ; skip              : string list
    ; kind              : string
    ; string_of_context : 'a -> string option
    }

  let create ~kind ~current_file ~string_of_context =
    { all_by_context = Hashtbl.Poly.create ()
    ; skip           = [current_file; __FILE__]
    ; kind
    ; string_of_context
    }

  let get_all_for_context t context =
    Hashtbl.find_or_add t.all_by_context context ~default:(fun () ->
      { all = Map.empty (module String) })
  ;;

  let register ~kind t context name =
    Reserved_namespaces.check_not_reserved ~kind name;
    let caller = Caller_id.get ~skip:t.skip in
    let all = get_all_for_context t context in
    (match Map.find all.all name with
     | None -> ()
     | Some e ->
       let declared_at = function
         | None -> ""
         | Some (loc : Caml.Printexc.location) ->
           Printf.sprintf " declared at %s:%d" loc.filename loc.line_number
       in
       let context =
         match t.string_of_context context with
         | None -> ""
         | Some s -> " on " ^ s ^ "s"
       in
       Printf.ksprintf
         failwith "%s '%s'%s%s matches %s '%s'%s"
         (String.capitalize t.kind) name context (declared_at caller)
         t.kind e.fully_qualified_name (declared_at e.declared_at)
    );
    let t =
      { fully_qualified_name = name
      ; declared_at          = caller
      }
    in
    all.all <- fold_dot_suffixes name ~init:all.all ~f:(fun name acc ->
      Map.add acc ~key:name ~data:t);
  ;;

  let spellcheck t context ?(white_list=[]) name =
    let all =
      let all = get_all_for_context t context in
      Map.fold all.all ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
    in
    match Spellcheck.spellcheck (all @ white_list) name with
    | Some _ as x -> x
    | None ->
      let other_contexts =
        Hashtbl.fold t.all_by_context ~init:[] ~f:(fun ~key:ctx ~data:{ all } acc ->
          if Polymorphic_compare.(<>) context ctx && Map.mem all name then
            match t.string_of_context ctx with
            | None -> acc
            | Some s -> (s ^ "s") :: acc
          else
            acc)
      in
      let pp_text = Format.pp_print_text in
      let current_context ppf =
        match t.string_of_context context with
        | None | Some "" -> ()
        | Some s ->
          let a_or_an =
            match s.[0] with
            | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> "an"
            | _ -> "a"
          in
          Format.fprintf ppf "@ but@ is@ used@ here@ in@ the@ context@ of@ %s@ %a"
            a_or_an pp_text s
      in
      match List.sort ~cmp:(fun x y -> - (String.compare x y)) other_contexts with
      | [] -> None
      | [c] ->
        Some
          (Format.asprintf
             "@[Hint:@ `%s'@ is@ available@ for@ %a%t.@]@\n\
              Did you put it at the wrong level?"
             name pp_text c current_context)
      | last :: rev_others ->
        let others = List.rev rev_others in
        Some
          (Format.asprintf
             "@[Hint:@ `%s'@ is@ available@ for@ %a@ and@ %a%t.@]@\n\
              Did you put it at the wrong level?"
             name
             (Format.pp_print_list pp_text
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
             others pp_text last current_context)
  ;;

  let raise_errorf t context ?white_list fmt (name : string Loc.t) =
    Printf.ksprintf (fun msg ->
      match spellcheck t context name.txt ?white_list with
      | None ->
        Location.raise_errorf ~loc:name.loc "%s" msg
      | Some s ->
        Location.raise_errorf ~loc:name.loc "%s.\n%s" msg s)
      fmt name.txt
  ;;
end
