open Test_utils

module Qc = struct
  let list_of_base_and_diffs_is_inverse_of_base_and_diffs_of_list =
    QCheck.Test.make ~count:1000
      ~name:"list_of_base_and_diffs_is_inverse_of_base_and_diffs_of_list"
      QCheck.(list_of_size Gen.(int_range 1 10) sched)
      (fun scheds ->
         match
           Daypack_lib.Sched_ver_history.Serialize.base_and_diffs_of_list scheds
         with
         | None -> false
         | Some (base, diffs) ->
           let reconstructed_scheds =
             Daypack_lib.Sched_ver_history.Deserialize.list_of_base_and_diffs
               base diffs
           in
           List.for_all2
             (fun s1 s2 -> Daypack_lib.Sched.Equal.sched_equal s1 s2)
             scheds reconstructed_scheds)

  let of_base_and_diffs_is_inverse_of_to_base_and_diffs =
    QCheck.Test.make ~count:1000
      ~name:"of_base_and_diffs_is_inverse_of_to_base_and_diffs"
      QCheck.(list_of_size Gen.(int_range 1 10) sched)
      (fun scheds ->
         let t = Daypack_lib.Sched_ver_history.of_sched_list scheds in
         match Daypack_lib.Sched_ver_history.Serialize.to_base_and_diffs t with
         | None -> false
         | Some (base, diffs) ->
           let reconstructed_t =
             Daypack_lib.Sched_ver_history.Deserialize.of_base_and_diffs base
               diffs
           in
           Daypack_lib.Sched_ver_history.Equal.equal t reconstructed_t)

  let read_from_dir_is_inverse_of_write_to_dir =
    QCheck.Test.make ~count:1000
      ~name:"read_from_dir_is_inverse_of_write_to_dir" sched_ver_history
      (fun sched_ver_history ->
         let dir = Core.Filename.temp_dir "daypack" "sched_ver_history" in
         ( match
             Daypack_lib.Sched_ver_history.Serialize.write_to_dir ~dir
               sched_ver_history
           with
           | Ok () -> ()
           | Error msg -> failwith msg );
         let sched_ver_history' =
           Daypack_lib.Sched_ver_history.Deserialize.read_from_dir ~dir
           |> Stdlib.Result.get_ok
         in
         FileUtil.rm ~recurse:true [ dir ];
         Daypack_lib.Sched_ver_history.Equal.equal sched_ver_history
           sched_ver_history')

  let suite =
    [
      list_of_base_and_diffs_is_inverse_of_base_and_diffs_of_list;
      of_base_and_diffs_is_inverse_of_to_base_and_diffs;
      read_from_dir_is_inverse_of_write_to_dir;
    ]
end
