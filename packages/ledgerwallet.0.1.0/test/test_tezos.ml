open Rresult
open Ledgerwallet_tezos

let vendor_id = 0x2C97
let product_id = 0x0001

let fail_on_error = function
  | Result.Ok () -> ()
  | Result.Error e ->
     failwith
       (Format.asprintf "Ledger error: %a" Ledgerwallet.Transport.pp_error e)

let with_connection f =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
  try
    fail_on_error (f h) ;
    Hidapi.close h
  with exn ->
    Hidapi.close h ;
    raise exn

let test_open_close () =
  with_connection (fun _ -> R.ok ())

let test_ping () =
  with_connection Ledgerwallet.Transport.ping

let hard x =
  Int32.logor x 0x8000_0000l

let path = [
  hard 44l ; hard 1729l
]

let curves = [Ed25519; Secp256k1; Secp256r1]

let msg = Cstruct.of_string "Voulez-vous coucher avec moi, ce soir ?"
let msg_ba = Cstruct.to_bigarray msg

let test_getpk h curve =
  get_public_key h curve path >>| fun pk ->
  Alcotest.(check int "pklen"
              (if curve = Ed25519 then 33 else 65) (Cstruct.len pk))

let test_getpk () =
  with_connection (fun h ->
      List.iter (fun x -> fail_on_error (test_getpk h x)) curves;
      R.ok ())

let test_sign h curve =
  let open Alcotest in
  get_public_key h curve path >>= fun pk ->
  sign h curve path msg >>| fun signature ->
  match curve with
  | Bip32_ed25519 -> ()
  | Ed25519 -> ()
     (*let pk = Tweetnacl.Sign.(pk pk) in
      check bool "sign Ed25519" true
        (Tweetnacl.Sign.verify_detached ~key:pk ~signature msg)*)
  | Secp256k1 -> begin
      let pk = Cstruct.to_bigarray pk in
      let signature = Cstruct.to_bigarray signature in
      match Uecc.(pk_of_bytes secp256k1 pk) with
      | None -> assert false
      | Some pk ->
          check bool "sign Secp256k1" true (Uecc.verify pk ~msg:msg_ba ~signature)
    end
  | Secp256r1 -> begin
      let pk = Cstruct.to_bigarray pk in
      let signature = Cstruct.to_bigarray signature in
      match Uecc.(pk_of_bytes secp256r1 pk) with
      | None -> assert false
      | Some pk ->
          check bool "sign Secp256r1" true (Uecc.verify pk ~msg:msg_ba ~signature)
    end

let test_sign () =
  with_connection (fun h -> test_sign h Secp256k1)

let basic = [
  "open_close", `Quick, test_open_close ;
  "ping", `Quick, test_ping ;
  "get_public_key", `Quick, test_getpk ;
  "sign", `Quick, test_sign ;
]

let () =
  Alcotest.run "ledgerwallet.tezos" [
    "basic", basic ;
  ]
