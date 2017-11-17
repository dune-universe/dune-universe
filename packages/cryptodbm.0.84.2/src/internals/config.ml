
(* Our format version *)
let current_format_version = "0.83"

(*** Builtin bindings ***)

(* This builtin key is associated to the signature data, when a cryptographic signature is required. 
 * It can be found for the table signature as well as for subtable signatures. *)
let signature_key = "Signature"

(* This builtin key is associated to the subtable salt or table salt. *)
let salt_key = "Salt"

(* In bytes *)
let salt_size = 16

(* Mixes salt and a password. *)
let add_salt salt passwd =
  assert (passwd <> "") ;
  "u%" ^ salt ^ "@\001\n" ^ passwd ^ "$\002"

(* Number of iterations to forge a strong password starting from a weak password. *)
(* The number is large, so that it takes significant time to forge a strong password. *)
let passwd_iterations = 13281

(* These keys are associated to some table parameters. *)
let max_extra_key_key  = "Max_extra_key"
let max_extra_data_key = "Max_extra_data"
let max_extra_bindings_key = "Max_extra_bindings"

let format_version_key = "Format version"

(* This binding is automatically added to all encrypted subtables, and to the global table.
 * It is used to check if the password is correct. *)
let test_key  = "Test Key"
let test_data = "Test Data"

(* Name of the subtable containing the dictionary. *)
let dictionary = "\000--Builtin--Dictionary--"
let uncrypted_dictionary = "\000--Builtin--Uncrypted--Dictionary--"
let extra_bindings = "\000--Builtin--Extra-Bindings--"
