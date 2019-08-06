type p = X509.Private_key.t list * Nocrypto.Rsa.priv
type authenticator = X509.Authenticator.t

let ( <.> ) f g = fun x -> f (g x)

let load_directory path =
  Lwt_unix.files_of_directory (Fpath.to_string path)
  |> Lwt_stream.map (Fpath.add_seg path)
  |> Lwt_stream.to_list

let load_file path =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string path) >>= fun ic ->
  Lwt_io.length ic >|= Int64.to_int >>= fun len ->
  let raw = Bytes.create len in
  Lwt_io.read_into_exactly ic raw 0 len >>= fun () ->
  Lwt.return (Bytes.unsafe_to_string raw)

let private_of_pems ~cert ~pk =
  let open Lwt.Infix in
  let open X509 in
  load_file cert
  >|= (Certificate.decode_pem <.> Cstruct.of_string)
  >|= Rresult.R.get_ok
  >>= fun certs ->
  load_file pk
  >|= (Certificate.decode_pem_multiple <.> Cstruct.of_string)
  >|= Rresult.R.get_ok
  >|= fun pk -> Ok (certs, pk)

let certs_of_pem path =
  let open Lwt.Infix in
  load_file path
  >|= (X509.Certificate.decode_pem <.> Cstruct.of_string)
  >|= Rresult.R.get_ok

let certs_of_pem_directory ?(ext= "crt") path =
  let open Lwt.Infix in
  load_directory path
  >>= Lwt_list.filter_p (Lwt.return <.> Fpath.has_ext ext)
  >>= Lwt_list.map_p certs_of_pem

let authenticator meth =
  let now = Ptime_clock.now () in
  let make certs = X509.Authenticator.chain_of_trust ~time:now certs in

  let open Lwt.Infix in

  match meth with
  | `Ca_file path -> certs_of_pem path >|= fun x -> make [ x ]
  | `Ca_directory path -> certs_of_pem_directory path >|= make
  | `No_authentication -> Lwt.return X509.Authenticator.null
