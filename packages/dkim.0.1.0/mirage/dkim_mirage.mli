type 'a stream = unit -> 'a option Lwt.t

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) : sig
  val verify :
    ?newline:Dkim.newline ->
    ?size:int ->
    ?nameserver:[ `TCP | `UDP ] * (Ipaddr.V4.t * int) ->
    ?timeout:int64 ->
    (string * int * int) stream ->
    S.t ->
    ( Dkim.signed Dkim.dkim list * Dkim.signed Dkim.dkim list,
      [> `Msg of string ] )
    result
    Lwt.t
end

val sign :
  key:Mirage_crypto_pk.Rsa.priv ->
  ?newline:Dkim.newline ->
  (string * int * int) stream ->
  Dkim.unsigned Dkim.dkim ->
  (Dkim.signed Dkim.dkim * (string * int * int) stream) Lwt.t
