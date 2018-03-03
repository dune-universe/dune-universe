(** The Milter module allows the creation of milter applications in OCaml,
    providing complete bindings to libmilter.

    Since libmilter uses pthreads internally, this module is thread-safe.
    However, due to the current OCaml implementation, each of the milter
    callbacks must acquire a global runtime lock while being executed, meaning
    that effectively only a single thread will run at once.

    See the {{:https://www.milter.org} official milter documentation} for
    more details.
*)


(** A milter context. *)
type ctx

(** The return status of milter callbacks. *)
type stat
  = Continue
  | Reject
  | Discard
  | Accept
  | Tempfail
  | No_reply
  | Skip
  | All

(** Flags that a filter must set in order to be able to execute their
    respective actions. *)
type flag
  = ADDHDRS
  | CHGHDRS
  | CHGBODY
  | ADDRCPT
  | ADDRCPT_PAR
  | DELRCPT
  | QUARANTINE
  | CHGFROM
  | SETSYMLIST

(** Stages of the SMTP session for which the registered filter callback
    functions are called. *)
type stage
  = CONNECT
  | HELO
  | ENVFROM
  | ENVRCPT
  | DATA
  | EOM
  | EOH

(** The type of milter protocol stages. *)
type step
  = NOCONNECT
  | NOHELO
  | NOMAIL
  | NORCPT
  | NOBODY
  | NOHDRS
  | NOEOH
  | NR_HDR
  | NOUNKNOWN
  | NODATA
  | SKIP
  | RCPT_REJ
  | NR_CONN
  | NR_HELO
  | NR_MAIL
  | NR_RCPT
  | NR_DATA
  | NR_UNKN
  | NR_EOH
  | NR_BODY
  | HDR_LEADSPC
  | MDS_256K
  | MDS_1M

type bytes =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** The type of filters. *)
type filter =
  { name : string
      (** The name of the filter. *)
  ; version : int
      (** The libmilter version. Must be set to {!version_code}. *)
  ; flags : flag list
      (** The list of flags corresponding to the actions performed by the
          filter. *)
  ; connect : (ctx -> string option -> Unix.sockaddr option -> stat) option
      (** Called at the start of each SMTP connection. Arguments: milter
          context, client host name (equals [None] if the reverse DNS lookup
          fails for the client address) and client address (equals [None] if
          the address type is not supported or if the connection is made via
          [stdin]). *)
  ; helo : (ctx -> string -> stat) option
      (** Called when the client sends a HELO/EHLO command. May be called zero
          or more times per connection. Arguments: milter context and HELO/EHLO
          string. *)
  ; envfrom : (ctx -> string -> string list -> stat) option
      (** Called when the client sends the MAIL FROM command. Arguments:
          milter context, sender address and ESMTP arguments. *)
  ; envrcpt : (ctx -> string -> string list -> stat) option
      (** Called once per RCPT TO command. Arguments: milter context,
          recipient address and ESMTP arguments. *)
  ; header : (ctx -> string -> string -> stat) option
      (** Called once for each message header. Arguments: milter context,
          header name and header value. *)
  ; eoh : (ctx -> stat) option
      (** Called after all message headers have been processed. Arguments:
          milter context. *)
  ; body : (ctx -> bytes -> int -> stat) option
      (** Called zero or more times between [eoh] and [eom] to handle a
          piece of the message body. Arguments: milter context, the current
          piece of the message body and its length. *)
  ; eom : (ctx -> stat) option
      (** Called once after all calls to [body] are made. Arguments: milter
          context. *)
  ; abort : (ctx -> stat) option
      (** Called when the current message is aborted. Must reclaim all
          per-message resources. The [abort] and [eom] callbacks are
          mutually exclusive. Arguments: milter context. *)
  ; close : (ctx -> stat) option
      (** Called when the current connection is being closed. Not necessarily
          called after all other callbacks. The [close] callback is called
          even if the message was aborted. Must reclaim all per-connection
          resources. Arguments: milter context. *)
  ; unknown : (ctx -> string -> stat) option
      (** Called when the client sends an unknown or unimplemented SMTP
          command. Arguments: milter context and SMTP command. *)
  ; data : (ctx -> stat) option
      (** Called when the client uses the DATA command. Arguments: milter
          context. *)
  ; negotiate : (ctx -> flag list -> step list -> stat * flag list * step list)
                option
      (** Called to dynamically determine and request operations and actions
          during filter startup. Arguments: milter context, list of {!flag}s
          offered by the MTA and list of {!step}s offered by the MTA. Must
          return a tuple with a {!stat} and the lists of flags and steps that
          the filter requires. *)
  }

exception Milter_error of string
  (** The exception raised by functions in this module in case of error. *)

val opensocket : bool -> unit
  (** Tries to create the milter interface socket specified by {!setconn}.
      If the boolean argument is [true] any existing UNIX sockets will be
      removed before trying to create a new one. Must be called before
      {!main}. *)

val register : filter -> unit
  (** Registers a filter. Only one filter may be created per process. Must be
      called before {!main}. *)

val setconn : string -> unit
  (** Specifies the socket through which communication with the MTA will
      be carried. The socket must be specified as a string following the
      formats below.

      - [(unix|local):/path/to/file] -- A named pipe.
      - [inet:port@(hostname|ip-address)] -- An IPV4 socket.
      - [inet6:port@(hostname|ip-address)] -- An IPV6 socket.

      Must be called once before {!main}. *)

val settimeout : int -> unit
  (** Sets the filter's I/O timeout value in seconds. Setting the timeout to
      [0] means "do not wait", not "wait forever". Must be called before
      {!main}. *)

val setbacklog : int -> unit
  (** Sets the filter's [listen(2)] backlog value. Must be called before
      {!main}. *)

val setdbg : int -> unit
  (** Sets the debug level for the milter library. Useful values are in the
      [0-6] range, with [0] meaning no debug messages and [6] being the
      most verbose level. *)

val stop : unit -> unit
  (** Shuts down the filter. *)

val main : unit -> unit
  (** Hands control to libmilter's event loop. This function only returns
      if {!stop} is called from one of the callbacks defined in {!register}
      of if an error occurs. *)

val getsymval : ctx -> string -> string option
  (** Gets the value of a milter macro. The availability of macros depends on
      each specific MTA. *)

val getpriv : ctx -> 'a option
  (** Gets the connection-specific private data for this connection. *)

val setpriv : ctx -> 'a option -> unit
  (** Sets the private data for this connection. *)

val setreply : ctx -> string -> string option -> string option -> unit
  (** [setreply ctx rcode xcode message] sets the default SMTP error reply
      code to [rcode], with an optional extended reply code as specified by
      RFC 1893/2034, with [message] as the text part of the reply. Only [4XX]
      and [5XX] replies are accepted. Cannot be called from the [connect]
      callback. *)

val setmlreply : ctx -> string -> string option -> string list -> unit
  (** The same as {!setreply} but uses a multi-line response, given
      as a list of strings. *)

val addheader : ctx -> string -> string -> unit
  (** [addheader ctx field value] adds a header named [field] with value
      [value] to the current message. Can only be called from the [eom]
      callback. *)

val chgheader : ctx -> string -> int -> string option -> unit
  (** [chgheader ctx field idx value] changes header [field] at index [idx]
      to the given value. If value is [None], the header is deleted. Can only
      be called from the [eom] callback. *)

val insheader : ctx -> int -> string -> string -> unit
  (** [insheader ctx idx field value] inserts a header to the current message
      at index [idx]. Index [0] corresponds to the topmost header. Can only be
      called from the [eom] callback. *)

val chgfrom : ctx -> string -> string option -> unit
  (** [chgfrom ctx from args] changes the envelope sender (MAIL FROM) address
      of the current message to [from] with optional ESMTP arguments [args].
      Can only be called from the [eom] callback. *)

val addrcpt : ctx -> string -> unit
  (** Adds a recipient for the current message. Can only be called from the
      [eom] callback. *)

val addrcpt_par : ctx -> string -> string option -> unit
  (** Adds a recipient for the current message, possibly including ESMTP
      parameters. Can only be called from the [eom] callback. *)

val delrcpt : ctx -> string -> unit
  (** Removes a recipient from the current message's envelope. Can only be
      called from the [eom] callback. *)

val replacebody : ctx -> bytes -> unit
  (** Replaces the message body. If called more than once, subsequent calls
      result in data being appended to the new body. Can only be called from
      the [eom] callback. *)

val progress : ctx -> unit
  (** Notifies the MTA that an operation is still in progress. This causes
      the MTA to reset its timeouts. Can only be called from the [eom]
      callback. *)

val quarantine : ctx -> string -> unit
  (** Quarantines the message using the given reason. Can only be called from
      the [eom] callback. *)

val version : unit -> int * int * int
  (** Gets the (runtime) version of libmilter. *)

val setsymlist : ctx -> stage -> string -> unit
  (** Sets the list of macros that the milter wants to receive from the MTA for
      a protocol stage. Can only be called from the [negotiate] callback. *)

val version_code : int
  (** The libmilter (compile time) version. *)

val empty : filter
  (** A default filter with [name] set to an empty string, [version] set to
      {!version_code} and all callback fields set to [None]. *)
