val authenticator :
  ?crls:X509.CRL.t list ->
  ?allowed_hashes:Mirage_crypto.Hash.hash list ->
  unit ->
  (X509.Authenticator.t, [> `Msg of string ]) result
(** [authenticator ~crls ~allowed_hashes ()] detects the root CAs (trust
    anchors) in the operating system's trust store using {!trust_anchors}. It
    constructs an authenticator with the current timestamp {!Ptime_clock.now},
    and the provided [~crls] and [~allowed_hashes] arguments. The resulting
    authenticator can be used for {!Tls.Config.client}.
    Returns [Error `Msg msg] if detection did not succeed. *)

val trust_anchors : unit -> (string, [> `Msg of string ]) result
(** [trust_anchors ()] detects the root CAs (trust anchors) in the operating
    system's trust store. On Unix systems, if the environment variable
    [NIX_SSL_CERT_FILE] is set, its value is used as path to the trust anchors.
    The successful result is a list of pem-encoded X509 certificates. *)
