open! Core.Core_stable

module V1 = struct
  include Make_stable.Of_stable_format.V1
      (String.V1)
      (struct
        type t = Re2.t

        let compare = Re2.compare
        let to_stable_format = Re2.to_string
        let of_stable_format = Re2.of_string
      end)

  let%expect_test _ =
    print_endline [%bin_digest: t];
    [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
  ;;
end
