open ExtLib
open Printf

open Common

let resolve_types mapping shape =
  let rec map : resolve_type -> result_type = function
  | `List t -> `List (map t)
  | `Object t -> `Object (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Typeof (Field x) -> let name = ES_name.make mapping x in `Ref (name, typeof mapping name)
  | `Typeof (Script _) -> `Json
  | `Maybe t -> `Maybe (map t)
  | `Json | `Int64 | `Int | `String | `Double | `Bool | `Ref _ as t -> t
  in
  map shape

let output ~init mapping query =
  let shape =
    match Query.extract query with
    | Get (_,Some filter) -> Hit.doc @@ `Maybe (Hit.of_mapping ~filter mapping)
    | Get (_,None) -> Hit.doc_no_source
    | Mget { conf; _ } ->
      begin match Query.extract_source conf with
        | None -> `Dict ["docs",`List Hit.doc_no_source]
        | Some filter -> `Dict ["docs",`List (Hit.doc @@ `Maybe (Hit.of_mapping ~filter mapping))]
      end
    | Search { source; highlight; _ } ->
      let highlight = Option.map (Aggregations.derive_highlight mapping) highlight in
      let hits = Hit.hits mapping ~highlight source in
      let aggs = List.map snd @@ snd @@ Aggregations.analyze mapping query in (* XXX discarding constraints *)
      let result = `Dict (("hits", (hits:>resolve_type)) :: (if aggs = [] then [] else ["aggregations", `Dict aggs])) in
      resolve_types mapping result
  in
  Atdgen.of_shape ~init "result" shape

let print_reflect name mapping =
  let extern name = name ^ "_" in
  let rec iter nr_indent (name,x) =
    let indent = String.make nr_indent ' ' in
    let meta = get_meta x in
    let repr = get_repr_opt meta in
    match U.member "type" x, U.member "properties" x with
    | `String typ, `Null ->
      let typ = Option.default typ repr in
      printfn "%smodule %s = %s(%s)" indent (to_valid_modname name) (extern "Id") (to_valid_modname @@ extern typ)
    | (`Null | `String "nested"), `Assoc props ->
      let modul = to_valid_modname name in
      printfn "%smodule %s = struct" indent modul;
      List.iter (iter (nr_indent+2)) props;
      printfn "%send (* %s *)" indent modul
    | `Null, `Null -> fail "neither type nor properties found for %S" name
    | _, `Null -> fail "strange type for %S" name
    | `Null, _ -> fail "strange properties for %S" name
    | _, _ -> fail "both type and properties found for %S" name
  in
  iter 0 (name,mapping)

let convert_wire_type = function
| `Int -> sprintf "string_of_int %s"
| `Int64 -> sprintf "Int64.to_string %s"
| `String -> sprintf "Json.to_string (`String %s)"
| `Double -> sprintf "Json.to_string (`Float %s)"
| `Bool -> sprintf "string_of_bool %s"
| `Json -> sprintf "Json.to_string %s"

let map_wire_type typ =
  sprintf @@ match typ with
  | `Int -> "`Int %s"
  | `Int64 -> "`String (Int64.to_string %s)"
  | `String -> "`String %s"
  | `Double -> "`Float %s"
  | `Bool -> "`Bool %s"
  | `Json -> "%s"

let convertor (t:var_type option) unwrap name =
  match t with
  | None -> sprintf "Json.to_string %s" (unwrap name)
  | Some {multi;typ;ref=_} ->
  match multi with
  | One -> convert_wire_type typ (unwrap name)
  | Many -> sprintf "Json.to_string (`List (List.map (fun x -> %s) %s))" (map_wire_type typ (unwrap "x")) name

(** Turn source related fields from json into query parameters. *)
let source_args source =
  match source with
  | None -> []
  | Some {excludes;includes} ->
    ["_source_include",includes; "_source_exclude",excludes]
    |> List.filter_map (function (_,None) -> None | (k,Some v) -> Some (k, String.concat "," v))

let source_args_to_string args =
  sprintf "[%s]" @@ String.concat ";" @@ List.map (fun (a,b) -> sprintf "%S,%S" a b) args

let derive mapping json =
  let query = Query.extract json in
  let (vars,json,http) =
    match query with
    | Search { q; extra; source=_; highlight=_; } ->
      let (agg_json, aggs) = Aggregations.analyze mapping json in
      let c1 = List.concat @@ fst @@ List.split aggs in
      let c2 = Query.infer' extra q in
      let vars = Query.resolve_constraints mapping (c1 @ c2) in
      let json = Tjson.replace json "query" q.json in
      let json = Tjson.replace (Tjson.replace json "aggregations" agg_json) "aggs" agg_json in
      vars, json, ("`POST","[__esgg_index;\"_search\"]","[]",Some json)
    | Mget { ids; json; conf } ->
      let args = conf |> Query.extract_source |> source_args |> source_args_to_string in
      Query.resolve_mget_types ids, json, ("`POST","[__esgg_index;\"_mget\"]",args,Some json)
    | Get (id,source) ->
      let args = source |> source_args |> source_args_to_string in
      let http = ("`GET",sprintf "[__esgg_index;__esgg_kind;%s]" id.name,args,None) in (* assuming name *)
      Query.resolve_get_types id, json, http
  in
  let var_unwrap name =
    match Hashtbl.find vars name with
    | exception _ -> id
    | Property (_,es_name,_) -> sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name)
    | Any | Type _ | List _ -> id
  in
  let var_type name : var_type option =
    match Hashtbl.find vars name with
    | Property (multi,name,typ) -> Some { multi; ref = Some name; typ; }
    | exception _ -> None
    | Any -> None
    | List typ -> Some { multi = Many; ref = None; typ; }
    | Type typ -> Some { multi = One; ref = None; typ; }
  in
  let check_multi v t =
    match t with
    | Some t when t.multi = One && v.Tjson.list -> fail "var %S marked as list, but derived otherwise" v.name
    | _ -> ()
  in
  let map name = convertor (var_type name) (var_unwrap name) name in
  let (bindings,groups) = Tjson.vars json in
  let bindings = bindings |> List.map begin fun (var:Tjson.var) ->
    let t = var_type var.name in
    check_multi var t;
    var.name, ((if var.optional then `Optional else `Required), `Simple t)
  end in
  let groups = groups |> List.map begin fun {Tjson.label;vars} ->
    let v =
      match vars with
      | [var] -> `Simple (var_type var)
      | _ -> `Group (List.map (fun v -> v, (`Required, `Simple (var_type v))) vars)
    in
    label, (`Optional, v)
  end in
  query, bindings @ groups, map, http
