open OUnit2

open Uritemplate

let test_list_of_examples ~cases =
  List.map
    (fun (template_string, _) ->
       template_string >:: (fun _ ->
           let template = Parser.template_of_string template_string in
           assert_equal ~printer:(fun a -> a) template_string (Template.string_of_template template)
         )
    ) cases


let test_fixture = "UriTemplate.Template" >::: [
    "get_variable_names" >::: [
      test_case (fun _ ->
          let template = Template.empty
                         |> Template.add_literal "https://example.com"
                         |> Template.add_expression
                           Expansion_type.PathParameter
                           [Template.create_variable_expression "a";
                            Template.create_variable_expression "b";]
                         |> Template.add_expression
                           Expansion_type.Fragment
                           [Template.create_variable_expression "e";
                            Template.create_variable_expression "f";]

          in
          assert_equal
            ~cmp:(List.for_all2 (=))
            ~printer:(List.fold_left (fun s a -> s ^ " " ^ a) "")
            ["a"; "b"; "e"; "f"]
            (Template.get_variable_names template)
        )
    ];

    "string_of_template" >:::
    let open Example_tests_t in
    List.map (fun (name, { testcases; _ }) ->
        name >::: test_list_of_examples ~cases:testcases
      ) (Example_tests_j.file_of_string Samples.json)
  ]
