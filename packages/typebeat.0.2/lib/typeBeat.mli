(** Module Content-Type

    The purpose of the ["Content-Type"] field is to describe the data contained
    in the body fully enough that the receiving user agent can pick an
    appropriate agent or mechanism to present the data to the user, or otherwise
    deal with the data in an appropriate manner. The value in this field is
    called a media type.

    The ["Content-Type"] header field specifies the nature of the data in the
    body of an entity by giving media type and subtype identifiers, and by
    providing auxiliary information that may be required for certain media
    types. After the media type and subtype names, the remainder of the header
    field is simply a set of parameters, specified in a ["attribute=value"]
    notation. The ordering of parameters is not significant.
*)

(** In general, the top-level media type is used to declare the general type of
    data.

    If another top-level type is to be used for any reason, it must be given a
    name starting with ["X-"] to indicate its non-standard status and to avoid a
    potential conflict with a future official name.
*)
type ty      =
  [ `Application
  | `Audio
  | `Ietf_token of string
  | `Image
  | `Message
  | `Multipart
  | `Text
  | `Video
  | `X_token of string ]

(** While the subtype specifies a specific format for that type of data. Thus, a
    media type of ["image/xyz"] is enough to tell a user agent that the data is
    an image, even if the user agent has no knowledge of the specific image
    format ["xyz"]. Such information can be used, for example, to decide whether or
    not to show a user the raw data from an unrecognized subtype -- such an
    action might be reasonable for unrecognized subtype of text, but not for
    unrecognized subtypes of image or audio.

    For this reason, registered subtypes of text, image, audio and video should
    not contain embedded information that is really of a different type. Such
    compound formats should be represented using the ["multipart"] or
    ["application"] types.

    TypeBeat recognizes 3 subtypes:
    - Subtype from the
    {{:http://www.iana.org/assignments/media-types/media-types.xhtml}IANA
    database}: [`Iana_token]
    - Valid subtype but not recognized by the IANA database: [`Ietf_token]
    - Like {!ty}, a non-standard subtype starting with ["X-"]: [`X_token]
*)
type subty   =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token of string ]

(** Parameters are modifiers of the media subtype, and as such do not
    fundamentally alter the nature of the content. The set of meaningful
    parameters depends on the media type and subtype, However, a given top-level
    media type may define parameters which are applicable to any subtype of that
    type.

    Parameters may be required by their defining content type or subtype or they
    may be optional. TypeBeat does not check any assumption about parameters
    except for the [`Multipart] where it expects the ["boundary"] parameter -
    but TypeBeat does not process (not yet!) the ["charset"] parameter for
    example.

    Note that the value of a parameter can be a quoted string ([`String]). In this
    case, the value does not include the quotes. That is, the quotation marks in
    a quoted-string are not a part of the value of the parameter, but are merely
    used to delimit that parameter value. In other case, the value is [`Token].
*)
type value   = [ `String of string | `Token of string ]

(** A convenience record to deal with the ["Content-Type"] which contains:
    - the media type [ty]
    - the subtype [subty]
    - parameters: a associative list of [(attribute, value)]
*)
type content =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

(** [pp_ty ty] prints a human-readable representation of {!ty}. *)
val pp_ty         : Format.formatter -> ty -> unit

(** [pp_subty subty] prints a human-readable representation of {!subty}. *)
val pp_subty      : Format.formatter -> subty -> unit

(** [pp_value value] prints a human-readable representation of {!value}. *)
val pp_value      : Format.formatter -> value -> unit

(** [pp_parameter (attribute, value)] prints a human-readable representation of
    [parameter].
*)
val pp_parameter  : Format.formatter -> (string * value) -> unit

(** [pp content] prints a human-readable representation of {!content}. *)
val pp            : Format.formatter -> content -> unit

val make          : ?parameters:(string * value) list -> ty -> string -> content

val equal         : ?insensitive:[ `All | `Parameters of string list ] -> content -> content -> bool

(** Default {{:https://tools.ietf.org/html/rfc822}RFC822} messages without a
    MIME ["Content-Type"] header are taken by this protocol to be plain text in
    the US-ASCII character set, which can be excplicitly specified as:

    {[
    Content-Type: text/plain; charset=use-ascii
    ]}

    This is assumed when no ["Content-Type"] header field is specified. It is also
    recommended that this default be assumed when a syntactically invalid
    ["Content-Type"] header field is encountered.
*)
val default       : content

(** The angstrom parser. *)
val parser        : content Angstrom.t

type error =
  [ `Invalid of (string * string list)
  | `Incomplete ]

(** [of_string str] parses an
    {{:https://tools.ietf.org/html/rfc2045#section-5.1}RFC2045} {!content}
    starting at [0] in [str]. We append a CRLF line break to the string [str]
    to ensure that the parsing terminates.
*)
val of_string     : string -> (content, error) result

(** [of_string_with_crlf str] parses an
    {{:https://tools.ietf.org/html/rfc2045#section-5.1}RFC2045} {!content}
    starting at [0] in [str]. We {b don't} append a CRLF line break to the string [str]
    but we expect this line break inside [str]. We need the CRLF line
    break to terminate the parsing.
*)
val of_string_with_crlf : string -> (content, error) result

(** [of_string_raw str off len] parses an
    {{:https://tools.ietf.org/html/rfc2045#section-5.1}RFC2045} {!content}
    starting at [off] in [str] to a tuple [(content, count)] with:

    - [content] the {!content}
    - [count] the number of byte read starting at [off] to parse the [content].

    We {b don't} append a CRLF line break to the string [str] but we
    expect this line break inside [str]. We the the CRLF line break to terminate
    the parsing.
*)
val of_string_raw : string -> int -> int -> (content * int, error) result
