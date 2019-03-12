open Core
open Ppxlib

let name = "router"

(* Refactor the common expression outputs into a helper function. *)
let expand_router_str ~loc router_id module_names error_handler =
    let open Ast_builder.Default in
    let routes_list = List.map ~f:(fun module_name -> pexp_ident ~loc {txt=Ldot(module_name, "routes"); loc=loc})
                               module_names in
    [%stri let [%p pvar router_id ~loc ] = Ocamlapi_router.create (List.concat [%e elist ~loc routes_list])
                                                             [%e error_handler]]
let router_str_pattern =
    let open Ast_pattern in
    let id = ppat_var __ in
    let module_names = pexp_construct __ none
                       |> elist in
    let error_handler = __ ^:: nil in
    let router_binding = value_binding
                         ~pat:id
                         ~expr:(module_names
                                ^:: error_handler
                                |> pexp_tuple)
                         ^::nil in
    pstr_value nonrecursive router_binding ^:: nil |> pstr

let router_extension_str =
    Extension.declare
              name
              Extension.Context.structure_item
              router_str_pattern
              (fun ~loc ~path:_ router_id module_names error_handler ->
                   expand_router_str ~loc router_id module_names
                   error_handler)

let expand_router_str = Context_free.Rule.extension router_extension_str

let expand_router ~loc router_id module_names error_handler exp =
    let open Ast_builder.Default in
    if module_names = []
    then failwith "Cannot create a router with empty list of modules"
    else
        let routes_list = List.map ~f:(fun module_name ->
                                           pexp_ident
                                           ~loc
                                           { txt=Ldot(module_name, "routes"); loc=loc})
                                   module_names in
        [%expr let [%p pvar router_id ~loc ] = Ocamlapi_router.create (List.concat [%e
        elist ~loc routes_list]) [%e error_handler ]
        in [%e exp]]

let router_pattern =
    let open Ast_pattern in
    let id = ppat_var __ in
    let module_names =
            pexp_construct __ none
            |> elist in
    let error_handler =  __ ^:: nil in
    pexp_let nonrecursive
             (value_binding ~pat:id
                            ~expr:(module_names
                                   ^:: error_handler
                                   |> pexp_tuple)
                                   ^:: nil) __
    |> single_expr_payload

let router_extension =
    Extension.declare
              name
              Extension.Context.expression
              router_pattern
              (fun ~loc ~path:_ router_id module_names exp ->
                    expand_router ~loc router_id module_names exp)

let expand_router = Context_free.Rule.extension router_extension

let register () =
    Driver.register_transformation
           name
           ~rules:[ expand_router;
                    expand_router_str ]
