open Gen_test_lib

let gen_test ?(ftotest = "rendu.ml") ?tsrange ?boltz_evaluated file_name size n
    t =
  let count = ref 1 in
  generic_loop
    (fun out_file ~out_err:_ _ sigs ts random_fun_def ->
      Format.fprintf out_file
        "open Boltzgen_runtime.Gen_test_lib\n\
         let _ = set_max_size %i;;\n\
         %a\n\
         module TestFunctor (R : EXPSIG ) = struct\n\
         \topen R\n\
         \tlet to_string = %s\n\
        \       %a\tlet _ =\n\
        \       print_endline \"to_string = %s\";\n"
        (int_of_float size) sigs () ts random_fun_def () (String.escaped ts))
    (fun out_file ~out_err:_ ?throw:_ ?canonize:_ s ->
      Format.fprintf out_file "\t\tprint_endline \"Case = Boltzgen test %i\";\n"
        !count;
      incr count;
      Format.fprintf out_file "\t\tprint_endline \"input = %s\";\n"
        (String.escaped s);
      Format.fprintf out_file
        "\t\tprint_endline (\"output = \"^(try to_string (%s) with x -> \
         Printexc.to_string x)^\"\");\n\
         print_newline ();\n"
        s)
    (fun out_file _ ->
      Format.fprintf out_file
        "\t\t()\nend;;\n#mod_use \"%s\"\nmodule TA = TestFunctor (%s);;" ftotest
        (mname_of_string ftotest))
    ?tsrange ?boltz_evaluated file_name size n t ftotest
