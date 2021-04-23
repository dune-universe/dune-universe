module Make (C : Mirage_clock.PCLOCK) : sig
  val authenticator :
    ?crls:X509.CRL.t list ->
    ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    unit ->
    (X509.Authenticator.t, [> `Msg of string ]) result
  (** [authenticator ~crls ~hash_whitelist ()] is an authenticator with the
    provided revocation lists, hash_whitelist. The trust anchors are based on
    the extraction from NSS' certdata.txt. *)
end
