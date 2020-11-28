type t
(** The collection of TextMate grammars. *)

type grammar
(** A TextMate grammar. *)

type stack
(** The state of the code highlighter. *)

exception Error of string
[@ocaml.warn_on_literal_pattern]
(** The error message is purely informational and is not to be matched on. *)

type plist =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of plist list
  | `Dict of (string * plist) list
  ]
(** A plist document. This is the same plist type that is defined in the
    {{:https://opam.ocaml.org/packages/plist-xml/} [plist-xml]} package (as of
    version 0.3.0), but is reproduced here as not to depend on [plist-xml]. *)

val create : unit -> t
(** Create an empty collection of grammars. *)

val add_grammar : t -> grammar -> unit
(** Add a grammar to the collection. *)

val find_by_name : t -> string -> grammar option
(** Finds a grammar by its [name] attribute. Case-insensitive. *)

val find_by_scope_name : t -> string -> grammar option
(** Finds for a grammar by its [scopeName] attribute. Case-sensitive. *)

val of_plist_exn : plist -> grammar
(** Reads a TextMate grammar from a plist file. Raises {!exception:Error} if
    the plist does not represent a valid TextMate grammar. *)

val empty : stack
(** The initial state of the code highlighter. *)

type token
(** A token of code. *)

val ending : token -> int
(** The index of the character right after the last character of the token.

    If [tok] is the first token of the line, it spans the substring from [0] to
    [ending tok]. If [tok] succeeds a token [prev], [tok] spans the substring
    from [ending prev] to [ending tok]. *)

val scopes : token -> string list
(** The token's stack of TextMate grammar scopes. *)

val tokenize_exn : t -> grammar -> stack -> string -> token list * stack
(** Usage: [let tokens, stack = tokenize_exn t grammar stack line]

    Tokenizes a line of code. Returns the list of tokens [tokens] and the
    updated tokenization state [stack].

    Precondition:

    - [line] must be a single line, including the newline character at the end.

    Postconditions:

    - [tokens] is nonempty.
    - The [ending] of the last token in [tokens] is always [String.length line].

    Raises {!exception:Error} if it tries to match a malformed [end] or [while]
    regex.

    Raises {!exception:Error} if it tries to access a local repository that
    doesn't exist in the file. Currently, it silently ignores [include]s of
    other grammars that don't exist in [t]. *)
