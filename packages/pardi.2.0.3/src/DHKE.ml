
module DH = Cryptokit.DH

(* all protocol messages *)
type from_srv =
  (* the server will choose the parameters for each client *)
  | Srv_to_Cli_DHKE_handshake_req of (DH.parameters * string)
  | Srv_to_Cli_jobs_ack of string list (* jobs; empty list means quit *)
type from_cli =
  | Cli_to_Srv_DHKE_handshake_ack of (DH.parameters * string)
  | Cli_to_Srv_jobs_req of (int * string) (* (nb_jobs, prev_results) *)

let signing_key_bit_len = 160
let signing_key_bytes_len = signing_key_bit_len / 8

let encryption_key_bit_len = 128
let encryption_key_bytes_len = encryption_key_bit_len / 8

let shared_secret_bit_len = signing_key_bit_len + encryption_key_bit_len
let shared_secret_bytes_len = shared_secret_bit_len / 8

(* parameters are public but new ones need to be created each time a DHKE
   is to take place; only called by the server once *)
let create_parameters () =
  (* 2048 is recommended by
     @inproceedings{weakdh15,
     title = {Imperfect Forward Secrecy: {H}ow {D}iffie-{H}ellman Fails
                  in Practice},
     author = {David Adrian and Karthikeyan Bhargavan and Zakir Durumeric
                  and Pierrick Gaudry and Matthew Green and J. Alex
                  Halderman and Nadia Heninger and Drew Springall and
                  Emmanuel Thom\'e and Luke Valenta and Benjamin
                  VanderSloot and Eric Wustrow and Santiago
                  Zanella-B\'eguelin and Paul Zimmermann},
     booktitle = {22nd ACM Conference on Computer and Communications
                  Security},
     month = oct,
     year = 2015
     } *)
  DH.new_parameters ~privlen:shared_secret_bit_len 2048

(* called by the server once per client and one time by each client *)
let create_secret (params: DH.parameters): DH.private_secret =
  DH.private_secret params

let create_handshake_req (params: DH.parameters) (secret: DH.private_secret)
  : DH.parameters * string =
  (params, DH.message params secret)

(* (sign_key, encryp_key) *)
let process_handshake
    (params: DH.parameters) (secret: DH.private_secret) (msg: string)
  : (string * string) =
  (* Cryptokit wipes [secret] upon DH.shared_secret *)
  let shared_secret = DH.shared_secret params secret msg in
  (* derive keys from it *)
  let sign_encrypt_keys = DH.derive_key shared_secret shared_secret_bytes_len in
  Cryptokit.wipe_string shared_secret;
  assert((shared_secret_bytes_len =
          signing_key_bytes_len + encryption_key_bytes_len) &&
         signing_key_bytes_len = 20 && encryption_key_bytes_len = 16);
  (* cut into (signing_key, encryption_key) *)
  let signing_key =
    String.sub sign_encrypt_keys 0 signing_key_bytes_len in
  let encryption_key =
    String.sub sign_encrypt_keys signing_key_bytes_len encryption_key_bytes_len in
  Cryptokit.wipe_string sign_encrypt_keys;
  (signing_key, encryption_key)
