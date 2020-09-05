open Test_utils

module Qc = struct
  let int64_of_int32_int32_is_inverse_of_int32_int32_of_int64 =
    QCheck.Test.make ~count:10_000
      ~name:"int64_of_int32_int32_is_inverse_of_int32_int32_of_int64" pos_int64
      (fun x ->
         let y =
           x
           |> Daypack_lib.Misc_utils.int32_int32_of_int64
           |> Daypack_lib.Misc_utils.int64_of_int32_int32
         in
         x = y)

  let suite = [ int64_of_int32_int32_is_inverse_of_int32_int32_of_int64 ]
end
