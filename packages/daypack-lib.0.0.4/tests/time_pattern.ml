open Test_utils

module Alco = struct end

module Qc = struct
  let unpack_is_inverse_of_pack_pattern =
    QCheck.Test.make ~count:5000 ~name:"unpack_is_inverse_of_pack_pattern"
      time_pattern (fun pattern ->
          let pattern' =
            pattern
            |> Daypack_lib.Time_pattern.Serialize.pack_pattern
            |> Daypack_lib.Time_pattern.Deserialize.unpack_pattern
          in
          pattern = pattern')

  let suite = [ unpack_is_inverse_of_pack_pattern ]
end
