open Ppxlib

type maybe_drop =
  | Keep
  | Drop

let drop_mode = ref Keep

let maybe_drop loc code =
  match !drop_mode with
  | Keep               -> [%str let () = [%e code]]
  | Drop               -> Attribute.explicitly_drop#expression code; [%str ]

let () =
  Driver.add_arg "-inline-test-drop"
    (Unit (fun () -> drop_mode := Drop))
    ~doc:"Drop unit tests"

let () =
  Driver.Cookies.add_simple_handler "inline-test"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
        | None -> ()
        | Some id -> match id.txt with
          | "enabled" -> drop_mode := Drop
          | "disabled" -> drop_mode := Keep
          | s ->
            Location.raise_errorf ~loc:id.loc
              "invalid 'inline_alcotests' cookie (%s), expected one of: enabled or disabled"
              s)

let tags =
  let open Ast_pattern in
  Attribute.declare
    "tags"
    Attribute.Context.pattern
    (single_expr_payload (
        pexp_tuple (many (estring __))
        |||  map (estring __) ~f:(fun f x -> f [x])))
    (fun x -> x)

let list_of_option = function None -> [] | Some v -> v

let name_and_expr expr =
  let open Ast_pattern in
  pstr ((
      pstr_value nonrecursive
        (value_binding
           ~pat:(pstring __)
          ~expr
          ^:: nil
        )
        ^:: nil))

let expand_test ~loc ~path:(path:label) id e =
  let open Ast_builder.Default in
  let loc = { loc with loc_ghost = true } in
  let expr =
    [%expr
      Ppx_inline_alcotest_runner.add_test
        ~path:[%e pexp_constant ~loc (Pconst_string (path, loc, None))]
        ~test_name:[%e pexp_constant ~loc (Pconst_string (id, loc, None))]
        (fun () -> [%e e])
    ]
  in
  maybe_drop loc expr

let test =
  Extension.declare_inline "inline_alcotest.test"
    Extension.Context.structure_item
    Ast_pattern.(name_and_expr __)
    expand_test

let extensions = [test]

let () =
  Driver.register_transformation "inline-alcotest"
    ~extensions
