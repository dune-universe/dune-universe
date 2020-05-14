(** {1 Unstrctrd.}

    {b Unstrctrd} (Unstructured) is a lexer/parser according RFC822.
    It accepts any input which respects ABNF described by RFC5322 (including obsolete form).
    To contextualize the purpose, email header, a part of DEB format, or HTTP 1.1 header respect, at least,
    a form, the {i unstructured} form which allows to split a value with a {i folding-whitespace}
    token.

    This token permits to limit any values to 80 characters per line:
{[To: Romain Calascibetta\r\n
   <romain@calascibetta.org>]}

    Then, others forms like email address or subject should, at least, be a subset of this form.
    The goal of this library is to delay complexity of this form to a little and basic library.

    {b Unstrctrd} handles UTF-8 as well (RFC6532). Any input should always terminate by CRLF. In other case, you can use {!safely_decode}.

    An usual process with {b Unstrctrd} is to use {!of_string} and {i delete} FWS with {!fold_fws} like:
{[let parse str = of_string str >>= fun (i, t) -> Ok (fold_fws t) ;;]}

    You can {i canonicalize} a string too. In other words, parse the given string, delete FWS and regenerate the string without any FWS such as:
{[# let canon str =
    let (_, t) = safely_decode str in
    let t = replace_invalid_bytes ~f:(fun _ -> None) t in
    let t = fold_fws t in
    to_utf_8_string t ;;
# canon "Hello\r\n World!" ;;
- : string = "Hello World!"]}
*)

type elt =
  [ `Uchar of Uchar.t
  | `WSP of wsp
  | `LF
  | `CR
  | `FWS of wsp
  | `d0
  | `OBS_NO_WS_CTL of obs
  | `Invalid_char of invalid_char ]
and wsp = private string
and obs = private char
and invalid_char = private char

type t = private elt list

type error = [ `Msg of string ]

val empty : t
val length : t -> int

val of_string : string -> (int * t, [> error ]) result
(** [of_string raw] tries to parse [raw] and extract the {i unstructured} form. [raw] should, at least,
    terminate by CRLF. *)

val safely_decode : string -> int * t
(** [safely_decode str] parses the given string and return a {!t} and how many bytes it consumed.
    The process puts systematically a CRLF at the end of the given string to never fails. *)

val replace_invalid_bytes : f:(invalid_char -> elt option) -> t -> t
(** [replace_invalid_bytes f t] wants to replace or delete invalid bytes into the given {!t}. You probably
    can replace them by [`Uchar Uutf.u_rep]. *)

val of_list : elt list -> (t, [> error ]) result
(** [of_list lst] tries to coerce [lst] to {!t}. It verifies that [lst] can not produce CRLF terminating token (eg. [[`CR; `LF]]). *)

val to_utf_8_string : t -> string
(** [to_utf_8_string t] returns a valid UTF-8 string of [t]. The given [t] must not contain [`Invalid_char], you probably
    should clean-up with {!replace_invalid_bytes}. *)

val iter : f:(elt -> unit) -> t -> unit
val fold : f:('a -> elt -> 'a) -> 'a -> t -> 'a
val map : f:(elt -> elt) -> t -> t

val wsp : len:int -> elt
val tab : len:int -> elt
val fws : ?tab:bool -> int -> elt

val without_comments : t -> (t, [> error ]) result
(** [without_comments t] tries to delete any comment of [t]. A comment is a part which begins with ['('] and ends with [')']. If we find a
    non-associated parenthesis, we return an error. *)

val fold_fws : t -> t
(* [fold_fws t] folds [FWS] token in [t]. A sequence of [FWS] will produce only one [WSP] token with the first [FWS]'s whitespaces. *)

val split_at : index:int -> t -> t * t
(* [split_at ~index t] splits [t] into two parts [(t0, t1)] where:
   {ul
   {- [length t0 = index]}
   {- [length t1 = length t - index]}}

   [index] must be upper or equal to [0] and less or equal to [length t]. *)

val split_on : on:[ `WSP | `FWS | `Uchar of Uchar.t | `Char of char | `LF | `CR ] -> t -> (t * t) option
(** [split_on ~on t] is either the pair [(t0, t1)] of the two (possibly empty)
    subparts of [t] that are delimited by the first match of [on] or [None] if
    [on] can't be matched in [t].

    The invariant [t0 ^ sep ^ t1 = t] holds. *)

(**/**)

module type MONAD = sig
  type 'a t
  type buffer

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t
  val read : (int -> 'a t) -> buffer -> 'a t
end

module type BUFFER = sig
  type t

  val blit_to_bytes : t -> int -> bytes -> int -> int -> unit
  val buf : t
end

module Make
  (Buffer : BUFFER)
  (Monad : MONAD with type buffer = Buffer.t) :
sig
  val make : unit -> Lexing.lexbuf

  val unstructured :
    ([ `FWS of string | `OBS_UTEXT of int * int * string | `VCHAR of string | `WSP of string | `Invalid_char of char ] as 'a) list ->
    Lexing.lexbuf -> 'a list Monad.t
end

val lexbuf_make : unit -> Lexing.lexbuf
val post_process : (t -> 'a) -> [ `FWS of string | `OBS_UTEXT of int * int * string | `VCHAR of string | `WSP of string | `Invalid_char of char ] list -> 'a
