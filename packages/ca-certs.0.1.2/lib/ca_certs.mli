val authenticator :
  ?crls:X509.CRL.t list ->
  ?hash_whitelist:Mirage_crypto.Hash.hash list ->
  unit ->
  (X509.Authenticator.t, [> `Msg of string ]) result
(** [authenticator ~crls ~hash_whitelist ()] detects the root CAs (trust
    anchors) in the operating system's trust store using {!trust_anchors}. It
    constructs an authenticator with the current timestamp {!Ptime_clock.now},
    and the provided [~crls] and [~hash_whitelist] arguments, to be used for
    {!Tls.Config.client}.
    Returns [Error `Msg msg] if detection did not succeed. *)

val trust_anchors : unit -> (string, [> `Msg of string ]) result
(** [trust_anchors ()] detects the root CAs (trust anchors) in the operating
    system's trust store.
    The successful result is a list of pem-encoded X509 certificates. *)
