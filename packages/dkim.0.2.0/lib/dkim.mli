module Sigs = Sigs

type (+'a, 'err) or_err = ('a, ([> Rresult.R.msg ] as 'err)) result

type newline = CRLF | LF

type map

type signed

type unsigned

type 'a dkim

type server

type body

val pp_dkim : 'a dkim Fmt.t

val pp_server : server Fmt.t

type extracted = {
  dkim_fields : (Mrmime.Field_name.t * Unstrctrd.t * map) list;
  fields : (Mrmime.Field_name.t * Unstrctrd.t) list;
  prelude : string;
}

val extract_dkim :
  ?newline:newline ->
  'flow ->
  't Sigs.state ->
  (module Sigs.FLOW with type flow = 'flow and type backend = 't) ->
  ((extracted, _) or_err, 't) Sigs.io
(** [extract_dkim ?newline flow state (module Flow)] reads [flow] with
    Input/Output scheduler represented by [state] and primitives implemented by
    [(module Flow)]. [?newline] specifies kind of contents ([CRLF] from network
    or [LF] from database like {i maildir}).

    It tries to extract [DKIM-Signature] fields with values, others fields and
    give a prelude of the body of the email (given by [flow]). *)

val post_process_dkim : map -> (signed dkim, _) or_err
(** [post_process_dkim map] from an already parsed [DKIM-Signature] represented
    by {!map}, we compute a post process analyze (check required/optional well
    formed values) and return a safe representation of [DKIM-Signature],
    {!dkim}, which can be used by {!verify}. *)

val selector : 'a dkim -> [ `raw ] Domain_name.t
(** [selector dkim] returns the selector of the DKIM-Signature field.

    Selectors might indicate the names of office locations, the signing date, or
    even an individual user. *)

val domain : 'a dkim -> [ `raw ] Domain_name.t
(** [domain dkim] returns the domain which signed the mail. *)

val fields : 'a dkim -> Mrmime.Field_name.t list

val domain_name :
  'a dkim -> ([ `raw ] Domain_name.t, [> `Msg of string ]) result
(** [domain_name dkim] returns the full domain-name where the DNS TXT record can
    be get. *)

val extract_server :
  't ->
  'backend Sigs.state ->
  (module Sigs.DNS with type t = 't and type backend = 'backend) ->
  'a dkim ->
  ((map, _) or_err, 'backend) Sigs.io
(** [extract_server dns state (module Dns) dkim] gets public-key noticed by
    [dkim] from authority server over DNS protocol (with Input/Output scheduler
    represented by [state] and primitives implemented by [(module Dns)]). *)

val post_process_server : map -> (server, _) or_err
(** [post_process_server map] from an already parsed TXT record (given by a DNS
    service) represented by {!map}, we compute a post-process analyze (check
    required/optional well formed values) and return a safe representation of
    the public-key, {!server}, which can be used by {!verify}. *)

val extract_body :
  ?newline:newline ->
  'flow ->
  'backend Sigs.state ->
  (module Sigs.FLOW with type flow = 'flow and type backend = 'backend) ->
  prelude:string ->
  (body, 'backend) Sigs.io
(** [extract_body ?newline flow state (module Flow) ~prelude] extracts a thin
    representation of the body of the email. It should follow {!extract_dkim}
    with [prelude] and with [flow], [state], [(module Flow)] and [?newline]
    arguments. It returns a {!body} which can be used by {!verify}. *)

val verify :
  (Mrmime.Field_name.t * Unstrctrd.t) list ->
  Mrmime.Field_name.t * Unstrctrd.t ->
  signed dkim ->
  server ->
  body ->
  bool
(** [verify fields (dkim_field_name, dkim_value) dkim server body] verifies the
    given email (represented by {!body}. [fields] and
    [(dkim_field_name, dkim_value)]) with a signature {!dkim} and the public-key
    represented by {!server}.

    It returns [true] if signature is correct or [false] if something is wrong.

    Establishing the exact cause of a failed verification if difficult:

    - [selector] can not be found.
    - Public-key was updated.
    - [DKIM-Signature] is not well-formed.
    - etc.

    At least, [dkim] provides some logs to highlight where the verification
    failed. Finally, the given email should be treated the same as all
    unverified email - regardless of whether or not it looks like it was signed. *)

type algorithm = [ `RSA ]

type hash = [ `SHA1 | `SHA256 ]

type canonicalization = [ `Simple | `Relaxed ]

type query = [ `DNS of [ `TXT ] ]

val v :
  ?version:int ->
  ?fields:Mrmime.Field_name.t list ->
  selector:[ `raw ] Domain_name.t ->
  ?algorithm:algorithm ->
  ?hash:hash ->
  ?canonicalization:canonicalization * canonicalization ->
  ?length:int ->
  ?query:query ->
  ?timestamp:int64 ->
  ?expiration:int64 ->
  [ `raw ] Domain_name.t ->
  unsigned dkim

module Encoder : sig
  val dkim_signature : signed dkim Prettym.t

  val as_field : signed dkim Prettym.t
end

val sign :
  key:Mirage_crypto_pk.Rsa.priv ->
  ?newline:newline ->
  'flow ->
  't Sigs.state ->
  (module Sigs.FLOW with type flow = 'flow and type backend = 't) ->
  unsigned dkim ->
  (signed dkim, 't) Sigs.io
(** [sign ~key ~newline flow state (module Flow) dkim] returns a signed {!dkim}
    value which can be serialized into the given email (represented by [flow]).
    According to [dkim], it will sign some fields and the body.

    The returned signed {!dkim} can be serialized with:

    {[ let dkim_field = Prettym.to_string Dkim.Encoder.as_field dkim ]} *)

val server_of_dkim : key:Mirage_crypto_pk.Rsa.priv -> 'a dkim -> server
(** [server_of_dkim] returns the required server value from a {!dkim} value. The
    user is able to store the associated server value into the DNS TXT record
    with {!server_to_string} such as:

    {[
      let str = Dkim.server_to_string (Dkim.server_of_dkim ~key dkim) in
      nsupdate (Dkim.domain_name dkim) `TXT str
    ]} *)

val server_to_string : server -> string
(** [server_to_string server] generates a [string] from the given [server] value
    to be able to store the string into the DNS TXT record. *)

(** / *)

val remove_signature_of_raw_dkim : Unstrctrd.t -> Unstrctrd.t

val relaxed_field_canonicalization :
  Mrmime.Field_name.t -> Unstrctrd.t -> (string -> unit) -> unit

module Body = Body
