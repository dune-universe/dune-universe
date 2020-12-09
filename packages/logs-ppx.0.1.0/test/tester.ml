let argv =
  match Array.to_list Sys.argv with
  | program :: rest -> (
      match List.rev rest with
      | output_file :: input_file :: args ->
          List.concat
            [
              [ program ];
              List.rev args;
              [ input_file; "-o"; output_file; "--dump-ast" ];
            ]
          |> Array.of_list
      | _ -> Sys.argv)
  | _ -> Sys.argv

let () = Migrate_parsetree.Driver.run_main ~argv ()
