open Core
open S.Route_exceptions

(*TODO: add an mli file. *)

let slash = Re2.create_exn "/"

let segments path =
    Re2.split slash path
    |> List.filter ~f:(fun s -> s <> "")

let trailing_segments path =
    Re2.split slash path
    |> function
       | ""::tl -> tl
       | segments -> segments

module Path_segment = struct

    let rule = Re2.create_exn
{|(?P<static>[^<]*)<(?:(?P<converter>[a-zA-Z_][a-zA-Z0-9_]*)(?:\((?P<args>.*?)\))?\:)?(?P<variable>[a-zA-Z_][a-zA-Z0-9_]*)>|}
    
    type t =
        (* TODO add coverter option to the dynamic case *)
        | Dynamic of { variable : string }
        | Static of string [@@deriving sexp, compare, hash]


    let extract_components m =
        let v, c, s = Re2.Match.get ~sub:(`Name "variable") m,
                      Re2.Match.get ~sub:(`Name "converter") m,
                      Re2.Match.get ~sub:(`Name "static") m in
        let template_parts =
            match v, c with
            | Some _variable, Some _converter ->
                        (*(Dynamic_with_converter { variable=variable;
                                                  converter=converter})::[] *)
                        (* TODO: support converters *)
                        failwith "Converters not supported yet"
            | Some variable, None -> (Dynamic { variable=variable })::[]
            | None, Some _converter -> failwith "Can't have a converter without a variable"
            | None, None -> [] in
        Option.value_map s
                         ~default:template_parts
                         ~f:(fun static ->
                             let segments = segments static
                                            |> List.map ~f:(fun segment -> Static segment) in
                             List.append segments template_parts)

    let variable_name =
        function
        | Dynamic { variable; _ } -> Some variable
        | Static _ -> None

    let path_segments_and_variable_names template =
        let ms = Re2.get_matches_exn rule template in
        let ps = List.concat_map ms ~f:extract_components in
        let vars = List.fold_left ps ~init:String.Set.empty ~f:(fun vs segment ->
                                         match variable_name segment with
                                         | Some v -> if not (String.Set.mem vs v)
                                                     then String.Set.add vs v
                                                     else InvalidRouteTemplate (sprintf "Duplicate variable name found in template %s" template)
                                                          |> raise
                                         | None -> vs) in 
        let end_match =
            List.last ms
            |> Option.map ~f:(Re2.Match.get_pos_exn ~sub:(`Index 0))
            |> Option.value_map ~default:0
                                ~f:(fun (start, offset) -> start + offset) in
        if end_match < String.length template
        then let remainder =
             let len = (String.length template) - end_match in
             String.sub ~pos:end_match ~len template in
             if String.contains remainder '<' ||
                String.contains remainder '>'
             then InvalidRouteTemplate (sprintf "Malformed variable in url template %s" template)
                  |> raise
             else List.append ps (trailing_segments remainder |> List.map ~f:(fun s -> Static s)), vars
        else
            ps, vars

end

module Path = struct
    include Path_segment
    include Hashable.Make(Path_segment)
end

type 'a path_trie = { mutable is_terminal : 'a option
                    ; mutable is_dynamic_path : (string * 'a path_trie) option
                    ; map : 'a path_trie String.Table.t } [@@deriving sexp]

let empty () =
    { is_terminal=None
    ; is_dynamic_path=None
    ; map=String.Table.create () }

let insert t segments value =
    let rec helper t segments =
        match segments with
        | [] -> 
            begin
                match t.is_terminal with
                | Some _ -> `Duplicate
                | None -> t.is_terminal <- Some value; `Ok
            end
        | Path.Dynamic { variable } :: tl ->
            begin
                match t.is_dynamic_path with
                | Some (_, t') -> helper t' tl
                | None ->
                    let t' = empty () in
                    begin
                        match helper t' tl with
                        | `Ok -> t.is_dynamic_path <- Some (variable, t'); `Ok
                        | `Duplicate -> `Duplicate
                    end
            end
        | Path.Static static :: tl ->
            begin
                match Hashtbl.find t.map static with
                | Some t' -> helper t' tl
                | None -> let t' = empty () in
                    begin
                        match helper t' tl with
                        | `Ok -> Hashtbl.add_exn t.map ~key:static ~data:t'; `Ok
                        | `Duplicate -> `Duplicate
                    end
            end in
    helper t segments

(* Convert string to list of segments *)
let insert_path trie path =
    let path_segments, _ = Path_segment.path_segments_and_variable_names path in
    insert trie path_segments

let matches t path =
    let segments = trailing_segments path in
    let rec helper t vs segments =
        match segments with
        | [] -> Option.map
                t.is_terminal
                ~f:(fun value -> vs, value)
        | segment::tl ->
            begin
                match Hashtbl.find t.map segment with
                | Some t' -> helper t' vs tl
                | None -> Option.bind t.is_dynamic_path ~f:(fun (v, t') -> Hashtbl.add_exn vs ~key:v ~data:segment;
                                                                           helper t' vs tl)
            end in
    helper t (String.Table.create ()) segments
