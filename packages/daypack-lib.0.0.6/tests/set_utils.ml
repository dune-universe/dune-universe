open Test_utils

module Qc = struct
  (*$ #use "tests/set_utils.cinaps";;

    let set_list = [
      ("int64_set",
       "pos_int64_set",
       "Daypack_lib.Int64_set_utils.diff",
       "Daypack_lib.Int64_set_utils.add_diff",
       "Daypack_lib.Int64_set_utils.sub_diff",
       "Daypack_lib.Int64_set.equal"
      )
    ] in

    List.iter (fun (name, set_gen, f_diff, f_add_diff, _f_sub_diff, f_equal) ->
        print_add_diff_test
          ~name
          ~set_gen
          ~f_diff
          ~f_add_diff
          ~f_equal;
      )
      set_list;

    List.iter (fun (name, set_gen, f_diff, _f_add_diff, f_sub_diff, f_equal) ->
        print_sub_diff_test
          ~name
          ~set_gen
          ~f_diff
          ~f_sub_diff
          ~f_equal;
      )
      set_list;

    List.iter (fun (name, set_gen, f_diff, f_add_diff, f_sub_diff, f_equal) ->
        print_sub_diff_is_inverse_of_add_diff_test
          ~name
          ~set_gen
          ~f_diff
          ~f_add_diff
          ~f_sub_diff
          ~f_equal;
      )
      set_list;

    List.iter (fun (name, set_gen, f_diff, f_add_diff, f_sub_diff, f_equal) ->
        print_add_diff_is_inverse_of_sub_diff_test
          ~name
          ~set_gen
          ~f_diff
          ~f_add_diff
          ~f_sub_diff
          ~f_equal;
      )
      set_list;

    print_endline "let suite = [";
    List.iter (fun (name, _, _, _, _, _) ->
        Printf.printf "%s;\n" (get_add_diff_test_name name);
      ) set_list;
    List.iter (fun (name, _, _, _, _, _) ->
        Printf.printf "%s;\n" (get_sub_diff_test_name name);
      ) set_list;
    List.iter (fun (name, _, _, _, _, _) ->
        Printf.printf "%s;\n" (get_sub_diff_is_inverse_of_add_diff_test_name name);
      ) set_list;
    List.iter (fun (name, _, _, _, _, _) ->
        Printf.printf "%s;\n" (get_add_diff_is_inverse_of_sub_diff_test_name name);
      ) set_list;
    print_endline "]"
  *)

  let add_diff_test_int64_set =
    QCheck.Test.make ~count:5000 ~name:"add_diff_test_int64_set"
      QCheck.(pair pos_int64_set pos_int64_set)
      (fun (old, x) ->
         let diff = Daypack_lib.Int64_set_utils.diff ~old x in
         Daypack_lib.Int64_set.equal
           (Daypack_lib.Int64_set_utils.add_diff diff old)
           x)

  let sub_diff_test_int64_set =
    QCheck.Test.make ~count:5000 ~name:"sub_diff_test_int64_set"
      QCheck.(pair pos_int64_set pos_int64_set)
      (fun (old, x) ->
         let diff = Daypack_lib.Int64_set_utils.diff ~old x in
         Daypack_lib.Int64_set.equal
           (Daypack_lib.Int64_set_utils.sub_diff diff x)
           old)

  let sub_diff_is_inverse_of_add_diff_test_int64_set =
    QCheck.Test.make ~count:5000
      ~name:"sub_diff_is_inverse_of_add_diff_test_int64_set"
      QCheck.(pair pos_int64_set pos_int64_set)
      (fun (old, x) ->
         let diff = Daypack_lib.Int64_set_utils.diff ~old x in
         Daypack_lib.Int64_set.equal
           (Daypack_lib.Int64_set_utils.sub_diff diff
              (Daypack_lib.Int64_set_utils.add_diff diff old))
           old)

  let add_diff_is_inverse_of_sub_diff_test_int64_set =
    QCheck.Test.make ~count:5000
      ~name:"add_diff_is_inverse_of_sub_diff_test_int64_set"
      QCheck.(pair pos_int64_set pos_int64_set)
      (fun (old, x) ->
         let diff = Daypack_lib.Int64_set_utils.diff ~old x in
         Daypack_lib.Int64_set.equal
           (Daypack_lib.Int64_set_utils.add_diff diff
              (Daypack_lib.Int64_set_utils.sub_diff diff x))
           x)

  let suite =
    [
      add_diff_test_int64_set;
      sub_diff_test_int64_set;
      sub_diff_is_inverse_of_add_diff_test_int64_set;
      add_diff_is_inverse_of_sub_diff_test_int64_set;
    ]

  (*$*)
end
