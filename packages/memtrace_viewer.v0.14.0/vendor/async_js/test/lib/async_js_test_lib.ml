open Core_kernel
open Async_rpc_kernel

module Rpcs = struct
  let send_string =
    Rpc.Rpc.create
      ~name:"send-string"
      ~version:1
      ~bin_query:String.bin_t
      ~bin_response:String.bin_t
  ;;

  let close_connection =
    Rpc.Rpc.create
      ~name:"close-connection"
      ~version:1
      ~bin_query:Unit.bin_t
      ~bin_response:Unit.bin_t
  ;;
end

module Callback_function = struct
  type 'a t = Open_rpc_and_wait of 'a [@@deriving sexp_of]

  let name (g : _ t) =
    match g with
    | Open_rpc_and_wait (_ : 'a) -> "openRpcAndWait"
  ;;

  let to_javascript_invocation = function
    | Open_rpc_and_wait uri ->
      sprintf {|%s("%s")|} (name (Open_rpc_and_wait ())) (Uri.to_string uri)
  ;;

  let%expect_test "to_javascript_invocation" =
    Open_rpc_and_wait (Uri.of_string "https://localhost:8443")
    |> to_javascript_invocation
    |> print_endline;
    [%expect {| openRpcAndWait("https://localhost:8443") |}]
  ;;
end
