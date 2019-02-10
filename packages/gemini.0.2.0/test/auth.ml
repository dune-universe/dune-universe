
open Gemini
open! Gemini.V1

let api_secret = "1234abcd"

let request_str =
{|{
     "request": "/v1/order/status",
     "nonce": 123456,

     "order_id": 18834
}
|}

let hex_encoding_test () =
  let hex = Auth.of_payload request_str in
  printf "%s" (Auth.to_string hex);
  ()


let hmac384_encrypting_test () =
  let hex = Auth.of_payload request_str in
  let hmac = Auth.hmac_sha384 ~api_secret hex in
  let str = Auth.to_string hmac in
  printf "%s" str;
  ()

let%expect_test "hex encoding" =
  hex_encoding_test () |> fun () ->
  [%expect "ewogICAgICJyZXF1ZXN0IjogIi92MS9vcmRlci9zdGF0dXMiLAogICAgICJub25jZSI6IDEyMzQ1NiwKCiAgICAgIm9yZGVyX2lkIjogMTg4MzQKfQo="]

let%expect_test "hmac384 encrypting" =
  hmac384_encrypting_test () |> fun () ->
  [%expect "aceaacbe56beb3b08c9f1b4bfd099cc523f8c15b36ccd0fdcf823e71ea6ff413b5a1de251cfab9c3c0e863739d76368f"]
