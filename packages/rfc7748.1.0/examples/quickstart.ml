
(* Open library to get module [X25519] and function [x25519]. *)
open Rfc7748

(* Make this example deterministic by fixing the random seed. *)
let _ = Random.init 7748

(* Create a list of [n] random bytes (could be using [List.init] in OCaml 4.06+). *)
let rec random_bytes = function
  | n when n <= 0 -> []
  | n -> Random.int 256 :: random_bytes (n - 1)

(* This function generates [n] random bytes and encodes them in a hexadecimal
   string. *)
let random_bytes_hex n =
  random_bytes n
  |> List.map @@ Format.sprintf "%02x"
  |> String.concat ""

(* The base point is used to derive public keys from private keys. *)
let base = X25519.(base |> string_of_public_key)

(* The main function shows how a shared secret can be negotiated using [x25519]. *)
let _ =
  (* We define two actors holding their own private keys. *)
  let alice = x25519 ~priv:(random_bytes_hex X25519.key_size) in
  let bob = x25519 ~priv:(random_bytes_hex X25519.key_size) in

  (* Each actor creates their own public key. *)
  let alice_pub = alice ~pub:base in
  let bob_pub = bob ~pub:base in

  (* Public keys are transferred over an insecure medium ... *)

  (* Each actor creates the shared secret using their own private key and the
     other actor's public key. *)
  let alice_shared = alice ~pub:bob_pub in
  let bob_shared = bob ~pub:alice_pub in

  (* Both actors hold the same shared secret. *)
  if alice_shared <> bob_shared then
    failwith @@ alice_shared ^ " != " ^ bob_shared
