open Core
open Ppxlib
open Url_template

let meth_callback_pair =
    let open Ast_pattern in
    (* Of type Cohttp.Code.meth *)
    let meth = alt (pexp_variant (string "Other") (some (estring __)))
                   (pexp_variant __ none) in
    let callback = __ in
    meth ^:: callback ^:: nil |> pexp_tuple

(* This function is used purely to validate tuple of Cohttp.Code.meth, callback.
 * Don't need to extract values (as of yet, could change) *)
let expand_meth_callback _meth_name _callback_expr = ()

let route_pattern =
    let open Ast_pattern in
    let route_binding = value_binding
                            ~pat:(pstring __)
                            ~expr:(elist __)
                            ^:: nil in
    let route_extension = extension
                            (string "route")
                            (pstr_value nonrecursive route_binding
                          ^:: nil
                          |> pstr) in
    pstr_extension route_extension nil

let expand_route_pattern ~loc acc path meth_callback_pairs =
    let open Ast_builder.Default in
    let template_id = id_from_template path in
    List.iter meth_callback_pairs ~f:(fun meth_callback ->
                           Ast_pattern.parse 
                                       meth_callback_pair
                                       loc
                                       meth_callback
                                       ~on_error:(fun () -> failwith "Some informative message")
                                       expand_meth_callback);
    [%stri let [%p pvar template_id ~loc] = [%e estring path
      ~loc ], [%e elist meth_callback_pairs ~loc ]], template_id::acc

let visit_route_extension ~loc ~on_error ext acc =
    Ast_pattern.parse
        route_pattern
        loc
        ~on_error
        ext
        (expand_route_pattern ~loc acc)


let route_nodes_expander = object (self)
    inherit [string list] Ast_traverse.fold_map as _super

    method! structure s acc =
        let s', ids =
            List.fold_right
                 s
                 ~f:(fun s_i (s', ids) ->
                         let s_i', ids' = self#structure_item s_i ids in
                         s_i'::s', ids')
                ~init:([], []) in
        if ids = []
        then s', acc
        else
            let open Ast_builder.Default in
            let loc = !Ast_helper.default_loc in
            let route_ids = List.map
                                 ids
                                 ~f:(fun id -> evar id ~loc:loc) in
            let s'' = List.append s'
                      [[%stri let [%p pvar "routes" ~loc:loc ] =
                                  [%e elist route_ids ~loc:loc ]]] in
            s'', acc

    method! structure_item e acc =
        let on_error = (fun () -> 
                        let desc', acc' = self#structure_item_desc e.pstr_desc acc in 
                        { e with pstr_desc = desc' }, acc') in
        visit_route_extension ~loc:!Ast_helper.default_loc ~on_error:on_error e acc
end

let expand_route_nodes s =
    route_nodes_expander#structure s [] |> fst
