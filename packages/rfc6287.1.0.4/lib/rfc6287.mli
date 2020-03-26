(** RFC6287 (OCRA)
    @see <http://tools.ietf.org/html/rfc6287> RFC6287
    @see <https://www.rfc-editor.org/errata_search.php?eid=3729> Errata 3729
 *)

open Rresult

(** The abstract OCRA [suite] type *)
type t

type err = Invalid_suite_string | DataInput of string | Window of string

val t_of_string : string -> (t, err) result

val string_of_t : t -> string

type di =
  { c: bool  (** C *)
  ; q: [`A | `N | `H] * int  (** Q *)
  ; p: [`SHA1 | `SHA256 | `SHA512] option  (** P *)
  ; s: int option  (** S *)
  ; t: int option  (** T *) }

val di_of_t : t -> di
(** @return DataInput spec *)

val challenge : t -> string
(** @return random challenge string [q] with format and length as specified in
    [suite] *)

(** if pinhash is [`String x], {!gen} and {!verify} will apply the Pin Hash
    algorithm specified in [suite] to calculate the digest of x.

    if pinhash is [`Digest d], its length must equal digests size of Pin Hash
    function (as specified in [suite]) *)
type pinhash = [`String of string | `Digest of Cstruct.t]

(** if timestamp is [`Now], {!gen} and {!verify} will use {!Unix.time} and the
    timestep specified in [suite] to calculate the timestamp value *)
type timestamp = [`Now | `Int64 of int64]

val gen :
     ?time:int64
  -> ?c:int64
  -> ?p:pinhash
  -> ?s:Cstruct.t
  -> ?t:timestamp
  -> key:Cstruct.t
  -> q:string
  -> t
  -> (Cstruct.t, err) result
(** Generate [OCRA(K, {\[C\] | Q | \[P | S | T\]})].
    @return {ul
     {- [Ok a] the response}
     {- [Error (DataInput error_message)] if parameters do not match [suite]}}
    @param c DataInput C: Counter
    @param p DataInput P: Pin Hash
    @param s DataInput S: Session; length must equal session size
     (as specified in [suite])
    @param t DataInput T: Timestamp
    @param key CryptoFunction key K
    @param q DataInput Q: Challenge
*)

val gen1 :
     ?time:int64
  -> c:int64 option
  -> p:pinhash option
  -> s:Cstruct.t option
  -> t:timestamp option
  -> key:Cstruct.t
  -> q:string
  -> t
  -> (Cstruct.t, err) result

val verify :
     ?time:int64
  -> ?c:int64
  -> ?p:pinhash
  -> ?s:Cstruct.t
  -> ?t:timestamp
  -> ?cw:int
  -> ?tw:int
  -> key:Cstruct.t
  -> q:string
  -> a:Cstruct.t
  -> t
  -> (bool * int64 option, err) result
(** Verify OCRA Response.
    @return {ul
     {- [Ok (true, None)] upon successful verification for [suite] without
       [C] DataInput}
     {- [Ok (true, Some next_counter)] upon successful verification for [suite]
       with [C] DataInput}
     {- [Ok (false, None)] if verification failed}
     {- [Error (DataInput error_message)] if parameters do not match [suite]}
     {- [Error (Window error_message)] on invalid [cw] and [tw] parameters}}
    @param c DataInput C: Counter
    @param p DataInput P: Pin Hash
    @param s DataInput S: Session; length must equal session size
     (as specified in [suite])
    @param t DataInput T: Timestamp
    @param cw Counter Window
    @param tw Timestamp Window
    @param key CryptoFunction key K
    @param q DataInput Q: Challenge
    @param a Response to check against
*)

val verify1 :
     ?time:int64
  -> c:int64 option
  -> p:pinhash option
  -> s:Cstruct.t option
  -> t:timestamp option
  -> cw:int option
  -> tw:int option
  -> key:Cstruct.t
  -> q:string
  -> a:Cstruct.t
  -> t
  -> (bool * int64 option, err) result
