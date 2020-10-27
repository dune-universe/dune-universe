open Ezjs_min
open Promise
open Typed_array

class type keygenParams = object
  method name : js_string t readonly_prop
  method modulusLength : int optdef readonly_prop
  method publicExponent : uint8Array t optdef readonly_prop
  method hash : js_string t optdef readonly_prop
  method length : int optdef readonly_prop
  method namedCurve : js_string t optdef readonly_prop
end

class type cryptoKey = object
  method type_ : js_string t readonly_prop
  method extractable : bool t readonly_prop
  method algorithm : keygenParams t readonly_prop
  method usages : js_string t js_array t readonly_prop
end

class type cryptoKeyPair = object
  method privateKey : cryptoKey t readonly_prop
  method publicKey : cryptoKey t readonly_prop
end

class type signParams = object
  method name : js_string t readonly_prop
  method saltLength : int optdef readonly_prop
  method hash : js_string t optdef readonly_prop
end

class type deriveParams = object
  method name : js_string t readonly_prop
  method public : cryptoKey t optdef readonly_prop
  method hash : js_string t optdef readonly_prop
  method salt : arrayBuffer t optdef readonly_prop
  method info : arrayBuffer t optdef readonly_prop
  method iterations : int optdef readonly_prop
end

class type cryptParams = object
  method name : js_string t readonly_prop
  method label : arrayBuffer t optdef readonly_prop
  method counter : arrayBuffer t optdef readonly_prop
  method length : int optdef readonly_prop
  method iv : arrayBuffer t optdef readonly_prop
  method additionalData : arrayBuffer t optdef readonly_prop
  method tagLength : int optdef readonly_prop
end

class type importParams0 = object
  method name : js_string t readonly_prop
  method hash : js_string t optdef readonly_prop
  method namedCurve : js_string t optdef readonly_prop
end

type importParams = Unsafe.top

class type subtle = object
  method encrypt : cryptParams t -> cryptoKey t -> arrayBuffer t -> arrayBuffer t promise t meth
  method decrypt : cryptParams t -> cryptoKey t -> arrayBuffer t -> arrayBuffer t promise t meth
  method sign : signParams t -> cryptoKey t -> arrayBuffer t -> arrayBuffer t promise t meth
  method verify : signParams t -> cryptoKey t -> arrayBuffer t -> arrayBuffer t -> bool t promise t meth
  method digest : js_string t -> arrayBuffer t -> arrayBuffer t promise t meth
  method generateKey : keygenParams t -> bool t -> js_string t js_array t -> cryptoKey t promise t meth
  method generateKey_pair : keygenParams t -> bool t -> js_string t js_array t -> cryptoKeyPair t promise t meth
  method deriveKey : deriveParams t -> cryptoKey t -> keygenParams t -> bool t -> js_string t js_array t -> cryptoKey t promise t meth
  method deriveBits : deriveParams t -> cryptoKey t -> int -> arrayBuffer t promise t meth
  method importKey : js_string t -> arrayBuffer t -> importParams t -> bool t -> js_string t js_array t -> cryptoKey t promise t meth
  method exportKey : js_string t -> cryptoKey t -> arrayBuffer t promise t meth
  method wrapKey : js_string t -> cryptoKey t -> cryptoKey t -> cryptParams t -> arrayBuffer t promise t meth
  method unwrapKey : js_string t -> arrayBuffer t -> cryptoKey t -> cryptParams t -> importParams0 t -> bool t -> js_string t js_array t -> cryptoKey t promise t meth
end

class type crypto = object
  method subtle : subtle t prop
  method getRandomValues : ('a, 'b) typedArray t -> unit meth
end

let crypto : crypto t = Unsafe.variable "window.crypto"
let subtle : subtle t = crypto##.subtle

let random_values n =
  let a : uint8Array t = new%js uint8Array(n) in
  crypto##getRandomValues(a);
  Bigstring.of_arrayBuffer a##.buffer

type ec_curve = P256 | P384 | P521
type sha_algo = SHA1 | SHA256 | SHA384 | SHA512

type rsa_kind = PKCS | PSS | OAEP
type aes_kind = CTR | CBC | GCM
type ec_kind = DSA | DH

type sign_usage = [ `Sign | `Verify ]
type crypt_usage = [ `Encrypt | `Decrypt | `WrapKey | `UnwrapKey ]
type derive_usage = [ `DeriveKey | `DeriveBits ]
type usage = [ sign_usage | crypt_usage | derive_usage ]

type sign_params =
  | RSA_PKCS
  | RSA_PSS of int (* salt length *)
  | ECDSA of sha_algo
  | HMAC

type crypt_params =
  | RSA_OAEP of Bigstring.t option (* label *)
  | AES_CTR of Bigstring.t (* counter *) * int (* length *)
  | AES_CBC of Bigstring.t (* iv *)
  | AES_GCM of Bigstring.t (* iv *) * Bigstring.t option (* additional data *) * int option (* tag length *)

type keygen_params =
  | GRSA of rsa_kind * int (* modulus length *) * sha_algo * int list option (* public exponent *)
  | GEC of ec_kind * ec_curve
  | GHMAC of sha_algo * int option (* length *)
  | GAES of aes_kind * int (* length *)

type import_params =
  | IRSA of rsa_kind * sha_algo
  | IEC of ec_kind * ec_curve
  | IHMAC of sha_algo
  | IAES of aes_kind
  | IHKDF | IPBKDF2

type key_format = Raw | PKCS8 | SPKI | JWK

type 'usage crypto_key = {
  kind : string;
  extractable : bool;
  algorithm : keygen_params;
  usages : 'usage list;
}

type 'usage crypto_keypair = {
  public_key : 'usage crypto_key;
  private_key : 'usage crypto_key;
}

type 'usage crypto_key_all =
  | CKey of 'usage crypto_key
  | CKeyPair of 'usage crypto_keypair

type derive_algo =
  | ECDH of derive_usage crypto_key
  | HKDF of sha_algo * Bigstring.t * Bigstring.t
  | PBKDF2 of sha_algo * Bigstring.t * int

let to_eccurve = function
  | P256 -> string "P-256"
  | P384 -> string "P-384"
  | P521 -> string "P-521"

let to_sha = function
  | SHA1 -> string "SHA-1"
  | SHA256 -> string "SHA-256"
  | SHA384 -> string "SHA-384"
  | SHA512 -> string "SHA-512"

let str_of_rsa = function
  | PKCS -> "RSASSA-PKCS1-v1_5"
  | PSS -> "RSA-PSS"
  | OAEP -> "RSA-OAEP"

let str_of_ec = function DSA -> "ECDSA" | DH -> "ECDH"

let str_of_aes = function
  | CTR -> "AES-CTR"
  | CBC -> "AES-CBC"
  | GCM -> "AES-GCM"

let str_of_format = function
  | Raw -> "raw"
  | PKCS8 -> "pkcs8"
  | SPKI -> "spki"
  | JWK -> "jwk"

let str_of_usage : ([< usage ] -> js_string t) = function
  | `Sign -> string "sign"
  | `Verify -> string "verify"
  | `Encrypt -> string "encrypt"
  | `Decrypt -> string "decrypt"
  | `WrapKey -> string "wrapKey"
  | `UnwrapKey -> string "unwrapKey"
  | `DeriveKey -> string "deriveKey"
  | `DeriveBits -> string "deriveBits"

let pub_expo l =
  let l = match l with None -> [1; 0; 1] | Some l -> l in
  new%js uint8Array_fromArray (of_list l)

let to_keygen_params0 ?mod_length ?exponent ?sha ?length ?curve name : keygenParams t = object%js
  val name = string name
  val modulusLength = Optdef.option mod_length
  val publicExponent = optdef pub_expo exponent
  val hash = optdef to_sha sha
  val length = Optdef.option length
  val namedCurve = optdef to_eccurve curve
end

let to_keygen_params = function
  | GRSA (k, mod_length, sha, exponent) ->
    to_keygen_params0 ~mod_length ~sha ~exponent (str_of_rsa k)
  | GEC (k, curve) -> to_keygen_params0 ~curve (str_of_ec k)
  | GHMAC (sha, length) -> to_keygen_params0 ~sha ?length "HMAC"
  | GAES (k, length) -> to_keygen_params0 ~length (str_of_aes k)

let to_crypto_key key : cryptoKey t = object%js
  val type_ = string key.kind
  val extractable = bool key.extractable
  val algorithm = to_keygen_params key.algorithm
  val usages = of_listf str_of_usage key.usages
end

let to_crypto_keypair pair : cryptoKeyPair t = object%js
  val privateKey = to_crypto_key pair.private_key
  val publicKey = to_crypto_key pair.public_key
end

let to_sign_params0 ?length ?sha name : signParams t = object%js
  val name = string name
  val saltLength = Optdef.option length
  val hash = optdef to_sha sha
end

let to_sign_params = function
  | RSA_PKCS -> to_sign_params0 (str_of_rsa PKCS)
  | RSA_PSS length -> to_sign_params0 ~length (str_of_rsa PSS)
  | ECDSA sha -> to_sign_params0 ~sha (str_of_ec DSA)
  | HMAC -> to_sign_params0 "HMAC"

let to_derive_params0 ?public ?sha ?salt ?info ?iterations name : deriveParams t = object%js
  val name = string name
  val public = Optdef.option public
  val hash = optdef to_sha sha
  val salt = optdef Bigstring.to_arrayBuffer salt
  val info = optdef Bigstring.to_arrayBuffer info
  val iterations = Optdef.option iterations
end

let to_derive_params = function
  | ECDH key -> to_derive_params0 ~public:(to_crypto_key key) (str_of_ec DH)
  | HKDF (sha, salt, info) -> to_derive_params0 ~sha ~salt ~info "HKDF"
  | PBKDF2 (sha, salt, iterations) -> to_derive_params0 ~sha ~salt ~iterations "PBKDF2"

let to_crypt_params0 ?label ?counter ?length ?iv ?data ?tag_length name : cryptParams t = object%js
  val name = string name
  val label = optdef Bigstring.to_arrayBuffer label
  val counter = optdef Bigstring.to_arrayBuffer counter
  val length = Optdef.option length
  val iv = optdef Bigstring.to_arrayBuffer iv
  val additionalData = optdef Bigstring.to_arrayBuffer data
  val tagLength = Optdef.option tag_length
end

let to_crypt_params = function
  | RSA_OAEP label -> to_crypt_params0 ?label (str_of_rsa OAEP)
  | AES_CTR (counter, length) -> to_crypt_params0 ~counter ~length (str_of_aes CTR)
  | AES_CBC iv -> to_crypt_params0 ~iv (str_of_aes CBC)
  | AES_GCM (iv, data, tag_length) -> to_crypt_params0 ~iv ?data ?tag_length (str_of_aes GCM)

let to_import_params_base ?sha ?curve name : importParams0 t = object%js
  val name = string name
  val hash = optdef to_sha sha
  val namedCurve = optdef to_eccurve curve
end

let to_import_params0 = function
  | IRSA (k, sha) -> Some (to_import_params_base ~sha (str_of_rsa k))
  | IEC (k, curve) -> Some (to_import_params_base ~curve (str_of_ec k))
  | IHMAC sha -> Some (to_import_params_base ~sha "HMAC")
  | IAES k -> Some (to_import_params_base (str_of_aes k))
  | IHKDF | IPBKDF2 -> None

let to_import_params a : importParams t = match a, to_import_params0 a with
  | _, Some o -> Unsafe.inject o
  | IHKDF, _ -> Unsafe.inject (string "HKDF")
  | IPBKDF2, _ -> Unsafe.inject (string "PBKDF2")
  | _ -> assert false

let of_sha s =
  let f cb s = match to_string s with
    | "SHA-1" -> SHA1
    | "SHA-256" -> SHA256
    | "SHA-384" -> SHA384
    | "SHA-512" -> SHA512
    | _ -> cb s in
  match Optdef.to_option s with
  | Some x -> f (fun s -> f (fun _ -> assert false) s) (Unsafe.coerce x)##.name
  | None -> assert false

let of_eccurve s = match to_optdef to_string s with
  | Some "P-256" -> P256
  | Some "P-384" -> P384
  | Some "P-521" -> P521
  | _ -> assert false

let of_exponent _ = None

let of_keygen_params (p : keygenParams t) = match to_string p##.name with
  | "RSASSA-PKCS1-v1_5" ->
    GRSA (PKCS, unoptdef 0 p##.modulusLength,
          of_sha p##.hash, of_exponent p##.publicExponent)
  | "RSA-PSS" ->
    GRSA (PSS, unoptdef 0 p##.modulusLength,
          of_sha p##.hash, of_exponent p##.publicExponent)
  | "RSA-OAEP" ->
    GRSA (OAEP, unoptdef 0 p##.modulusLength,
          of_sha p##.hash, of_exponent p##.publicExponent)
  | "ECDSA" -> GEC (DSA, of_eccurve p##.namedCurve)
  | "ECDH" -> GEC (DH, of_eccurve p##.namedCurve)
  | "HMAC" -> GHMAC (of_sha p##.hash, Optdef.to_option p##.length)
  | "AES-CTR" -> GAES (CTR, unoptdef 0 p##.length)
  | "AES-CBC" -> GAES (CBC, unoptdef 0 p##.length)
  | "AES-GCM" -> GAES (GCM, unoptdef 0 p##.length)
  | _ -> assert false

let str_to_usage s : usage = match to_string s with
  | "sign" -> `Sign
  | "verify" -> `Verify
  | "encrypt" -> `Encrypt
  | "decrypt" -> `Decrypt
  | "wrapKey" -> `WrapKey
  | "unwrapKey" -> `UnwrapKey
  | "deriveKey" -> `DeriveKey
  | "deriveBits" -> `DeriveBits
  | _ -> assert false

let of_crypto_key k = {
  kind = to_string k##.type_;
  extractable = to_bool k##.extractable;
  algorithm = of_keygen_params k##.algorithm;
  usages = to_listf str_to_usage k##.usages;
}

let of_crypto_keypair p = {
  public_key = of_crypto_key p##.publicKey;
  private_key = of_crypto_key p##.privateKey;
}

let str_error s = Error (new%js error_constr (string s))

let to_bigstring f = function
  | Error e -> f @@ (Error e)
  | Ok b -> f @@ Ok (Bigstring.of_arrayBuffer b)

let encrypt ~algo ~(key : crypt_usage crypto_key) b f =
  let algo = to_crypt_params algo in
  let key = to_crypto_key key in
  rthen (subtle##encrypt algo key (Bigstring.to_arrayBuffer b)) (to_bigstring f)

let decrypt ~algo ~(key : crypt_usage crypto_key) b  f =
  let algo = to_crypt_params algo in
  let key = to_crypto_key key in
  rthen (subtle##decrypt algo key (Bigstring.to_arrayBuffer b)) (to_bigstring f)

let sign ~algo ~(key : sign_usage crypto_key)  b f =
  let algo = to_sign_params algo in
  let key = to_crypto_key key in
  rthen (subtle##sign algo key (Bigstring.to_arrayBuffer b)) (to_bigstring f)

let verify ~algo ~(key : sign_usage crypto_key) b1 b2 f =
  let algo = to_sign_params algo in
  let key = to_crypto_key key in
  rthen (subtle##verify algo key (Bigstring.to_arrayBuffer b1)
           (Bigstring.to_arrayBuffer b2))
    (function Error e -> f (Error e) | Ok b -> f (Ok (to_bool b)))

let digest ~algo b f =
  let algo = to_sha algo in
  rthen (subtle##digest algo (Bigstring.to_arrayBuffer b)) (to_bigstring f)

let generate_symmetric_key ?(extractable=true) ~algo ~usages f =
  let algo_js = to_keygen_params algo in
  match algo with
  | GAES _ | GHMAC (_, _) ->
    rthen (subtle##generateKey algo_js (bool extractable) (of_listf str_of_usage usages))
      (function Error e -> f (Error e) | Ok k -> f @@ Ok (of_crypto_key k))
  | _ -> f @@ str_error "asymmetric algorithm"

let generate_asymmetric_key ?(extractable=true) ~algo ~usages f =
  let algo_js = to_keygen_params algo in
  match algo with
  | GAES _ | GHMAC (_, _) -> f @@ str_error "symmetric algorithm"
  | _ ->
    rthen (subtle##generateKey_pair algo_js (bool extractable) (of_listf str_of_usage usages))
      (function Error e -> f (Error e) | Ok k -> f @@ Ok (of_crypto_keypair k))

let generate_key ?(extractable=true) ~algo ~usages f =
  let algo_js = to_keygen_params algo in
  match algo with
  | GAES _ | GHMAC (_, _) ->
    rthen (subtle##generateKey algo_js (bool extractable) (of_listf str_of_usage usages))
      (function Error e -> f (Error e) | Ok k -> f @@ Ok (CKey (of_crypto_key k)))
  | _ ->
    rthen (subtle##generateKey_pair algo_js (bool extractable) (of_listf str_of_usage usages))
      (function Error e -> f (Error e) | Ok k -> f @@ Ok (CKeyPair (of_crypto_keypair k)))

let derive_key ?(extractable=true) ~algo ~dalgo ~(key : derive_usage crypto_key) ~usages f =
  let algo = to_derive_params algo in
  let dalgo = to_keygen_params dalgo in
  let key = to_crypto_key key in
  rthen (subtle##deriveKey algo key dalgo (bool extractable) (of_listf str_of_usage usages))
    (function Error e -> f (Error e) | Ok k -> f (Ok (of_crypto_key k)))

let derive_bits ~algo ~(key : derive_usage crypto_key)  ~length f =
  let algo = to_derive_params algo in
  let key = to_crypto_key key in
  rthen (subtle##deriveBits algo key length) (to_bigstring f)

let import_key ?(extractable=true) ?(format=Raw) ~data ~algo ~usages f =
  let format = str_of_format format in
  let algo = to_import_params algo in
  rthen (subtle##importKey (string format) (Bigstring.to_arrayBuffer data) algo
           (bool extractable) (of_listf str_of_usage usages))
    (function Error e -> f (Error e) | Ok k -> f (Ok (of_crypto_key k)))

let export_key ?(format=Raw) key f =
  let format = str_of_format format in
  let key = to_crypto_key key in
  rthen (subtle##exportKey (string format) key) (to_bigstring f)

let wrap_key ?(format=Raw) ~key ~(wkey : crypt_usage crypto_key) ~algo f =
  let format = str_of_format format in
  let algo = to_crypt_params algo in
  let key = to_crypto_key key in
  let wkey = to_crypto_key wkey in
  rthen (subtle##wrapKey (string format)  key wkey algo) (to_bigstring f)

let unwrap_key ?(extractable=true) ?(format=Raw) ~wkey ~(ukey : crypt_usage crypto_key)
    ~ualgo ~ukeyalgo ~usages f =
  let ukey = to_crypto_key ukey in
  let ualgo = to_crypt_params ualgo in
  let ukeyalgo = match to_import_params0 ukeyalgo with None -> assert false | Some p -> p in
  let format = str_of_format format in
  rthen (subtle##unwrapKey
           (string format) (Bigstring.to_arrayBuffer wkey) ukey ualgo
           ukeyalgo (bool extractable) (of_listf str_of_usage usages))
    (function Error e -> f (Error e) | Ok k -> f (Ok (of_crypto_key k)))
