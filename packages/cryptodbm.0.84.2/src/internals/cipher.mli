(* This library is available in cryptodbm_internals *)

(*** Passwords ***)

type passwd
type t = passwd

(* mk_passwd is very expansive. Call it once when possible. *)
val mk_passwd : iterations:int -> string -> passwd

(* Creates the password as such, without strengthening. *)
val mk_weak_passwd : string -> passwd

(* There is only one empty password, guaranteed by mk_passwd. *)
val empty_passwd : passwd

(* Read the plain password. *)
val plain_passwd  : passwd -> string

(* Read the strengthened password. *)
val strong_passwd  : passwd -> string

(* Concatenate passwords.
 * Note that this is not equivalent to mk_passwd (p1 ^ p2 ^ ... ) *)
val concat : passwd list -> passwd

(*** Encryption, decryption ***)

(* Digest (hash) function. Used to compute the signature. 
 * Current algorithm: sha256 *)
val digest : string -> string

(* Encrypt and decrypt functions (currently : AES).
 * Partial evaluation is *NOT* done just after the ~passwd argument (can be optimized?) *)
val encrypt : passwd:passwd -> string -> string

(* @raise Error (Bad_password Any) when something went wrong (most likely, a wrong password). *)
val decrypt : passwd:passwd -> string -> string

(* Compute the number of padding bytes for a key, in a deterministic way. 
 * Returns a value in [0..max_pad]. *)
val compute_padding : key:string -> passwd:passwd -> max_pad:int -> int

(* Generates a random salt string. *)
val random_salt: len:int -> string
  

