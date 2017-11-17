open Cryptodbm_internals
open Kinds
open Exenum_internals.Convenience
open Big_int_Z

(* Converts an arbitrary string into a printable string (without escape codes). *)
let convert_char c = if Char.code c < 32 || Char.code c > 126 then '.' else c
let convert s = String.map convert_char s

let kind2s kind =
  (loc2s kind.key_loc) ^ 
  (match kind.key_how with
  | Uncrypted -> " (uncrypted)"
  | Encrypted (k1, k2, p) -> Printf.sprintf " (crypted : '%s', '%s', %d)" (convert (Cipher.plain_passwd k1)) (convert (Cipher.plain_passwd k2)) p)

let rec direct_bigint_aux s index len hash =
  if index >= len then hash
  else
    let hash' = (shift_left_big_int hash 8) +++ (Char.code s.[index]) in
    direct_bigint_aux s (index+1) len hash'

let direct_bigint s = direct_bigint_aux s 0 (String.length s) bigzero

(* Compute a 256-bits hash for the given string. *)
let hash256 s =
  let d1 = Digest.string s
  and d2 = Digest.string ("@+" ^ s ^ "--") in
  (shift_left_big_int (direct_bigint d1) 128) ++ (direct_bigint d2)

let rec get_big_int_size bi count =
  if sign_big_int bi = 0 then count
  else get_big_int_size (shift_right_big_int bi 1) (count+1)

(* Builds a string from a list of items *)
let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l
