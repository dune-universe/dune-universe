open Types

type passwd =
  { plain_passwd : string ;
    strong_passwd  : string }

type t = passwd

let empty_passwd = 
  { plain_passwd = "" ;
    strong_passwd = "" }

let plain_passwd p = p.plain_passwd
let strong_passwd p = p.strong_passwd

let digest s = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s

(* Transforms an arbitrary passwd into a 32 bytes passwd. *)
let normalize_passwd passwd = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) passwd

(* Encrypts or decrypts a string. *)
let transform ~passwd way p = 
  let tr = Cryptokit.Cipher.aes ~pad:Cryptokit.Padding.length (normalize_passwd passwd) way in
  Cryptokit.transform_string tr p

(* Encrypting function. *)
let encrypt ~passwd = 
  transform ~passwd:passwd.strong_passwd Cryptokit.Cipher.Encrypt

(* Decrypting function. *)
let decrypt ~passwd = 
  let f = transform ~passwd:passwd.strong_passwd Cryptokit.Cipher.Decrypt in
  fun s -> try f s with _ -> raiserror (Bad_password Any)

(* Debug versions *)
(*
let () = Printf.printf "Warning: debug version in cipher !!!\n%!"
let esc = String.escaped
let encrypt ~passwd s = Printf.sprintf "Encrypted-with(%s):%s" passwd.plain_passwd s
let decrypt ~passwd s =
  Printf.printf "Decrypting {%s} with password {%s}\n%!" (esc s) (esc passwd.plain_passwd) ;
  Scanf.sscanf s "Encrypted-with(%s@):%s@\014" 
    begin fun pw msg ->
      assert (pw = passwd.plain_passwd) ;
      Printf.printf "pw = %s, msg = %s\n%!" (esc pw) msg ;
      msg
    end
*)

(* Compute the number of padding bytes, in a deterministic way. Used for keys. *)
let compute_padding ~key ~passwd ~max_pad =
  let hash = digest (key ^ "*\000z" ^ passwd.strong_passwd ^ "!\000x" ^ string_of_int max_pad) in
  assert (String.length hash >= 3) ;
  let numb = (Char.code hash.[0]) lsl 16 + (Char.code hash.[1]) lsl 8 + (Char.code hash.[2]) in
  numb mod (1 + max_pad)

(* Expansive computation to get the strong password. 
 * We compute f^n p, where:
 *   p is the initial password,
 *   n is supposed large enough,
 *   f x is the encryption of x with p, xored with its own hashcode.
 *       the xor operation is used to add some complexity mixing encryption and hashing
*)

(* xor s1 with (repeating copies of) s2, starting at position pos, finishing at len1. *)
let rec circular_xor s1 s2 pos len1 len2 =
  if pos >= len1 then ()
  else
    begin
      let n = min (len1 - pos) len2 in
      Cryptokit.xor_string s2 0 s1 pos n ;
      circular_xor s1 s2 (pos + n) len1 len2
    end

let compute_strong_passwd iterations p =

  let rec iterate current n =
    (* Printf.printf "Iterate = %d\n%!" n ; *)
    if n = 0 then current
    else
      (* Encrypt with the password. *)
      let current2 = transform ~passwd:p Cryptokit.Cipher.Encrypt current in

      (* Xor with its own hashcode.) *)
      let current_hash = digest current2 in

      let current2 = Bytes.of_string current2 in

      (* circular_xor directly on current2. *)
      circular_xor current2 current_hash 0 (Bytes.length current2) (String.length current_hash) ;

      (* Get the digest in order to reduce the password size. *)
      iterate (digest (Bytes.to_string current2)) (n-1)
  in

  (* Start from the password itself. *)
  try digest (iterate p (abs iterations))
  with _ -> assert false (* This is not supposed to fail. *)

let mk_passwd ~iterations p =
  if p = "" then empty_passwd
  else 
    { plain_passwd = p ;
      strong_passwd  = compute_strong_passwd iterations p } 

let mk_weak_passwd p =
  if p = "" then empty_passwd
  else
    { plain_passwd = p ;
      strong_passwd = p }

let concat plist =
  List.fold_left 
    begin fun acu p ->
      if p == empty_passwd then acu
      else if acu == empty_passwd then p
      else
        { plain_passwd = acu.plain_passwd ^ p.plain_passwd ;
          strong_passwd = acu.strong_passwd ^ p.strong_passwd }
    end
    empty_passwd plist

let random_salt ~len = Utils.(random_string Utils.gen len)
