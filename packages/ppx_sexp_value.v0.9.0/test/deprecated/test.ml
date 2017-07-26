open Core

let%test_module "expressions and their evaluation" =
  (module struct
    let%test_unit "one expression" =
      let x = 1 and y = 2 in
      [%test_result: Sexp.t]
        ~expect:(Atom "3")
        [%sexp (x + y : int)]
    ;;

    let%test_unit "several expressions" =
      let x = 1 and y = 2 in
      [%test_result: Sexp.t]
        ~expect:(List [ Atom "message"
                      ; List [ Atom "x";     Atom "1" ]
                      ; List [ Atom "x + y"; Atom "3" ]
                      ])
        [%sexp "message" (x : int) (x + y : int)]
    ;;
  end)
;;

module Other_quotation_expanders = struct

  let test_exn here f =
    try f ()
    with e ->
      [%test_result: string] ~here:[here]
        (Exn.to_string e)
        ~expect:"(message ((value 2)))"

  let%test_unit _ =
    test_exn [%here] (fun () -> [%raise_structural_sexp "message" { value = 2 }])
  ;;

  let%test_unit _ =
    test_exn [%here] (fun () -> Error.raise [%structural_error "message" { value = 2 }])
  ;;

  let%test_unit _ =
    test_exn [%here] (fun () -> ok_exn [%structural_or_error "message" { value = 2 }])
  ;;
end
