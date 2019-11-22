open Core

module State : sig
  type t =
    [ `Header (** Initial state, parsing headers *)
    | `Content (** Parsing the body of the message. The details are in the body state. *)
    | `Expected_eof (** The message should end here. If it doesn't, it's an error *)
    ]

  val initial : [ `Header ]
end

module Content : sig
  type t =
    | Multipart of string list
    | Octet_stream

  val default : t
end

type t =
  { mutable state : State.t
  ; buf : Email_grammar.token Queue.t
  }

val create : unit -> t

module Result : sig
  type t =
    { new_state : State.t option
    ; tokens : Email_grammar.token list
    }
end

val combine : t -> Result.t -> unit
val return : ?new_state:State.t -> Email_grammar.token list -> Result.t
val return_eof : Result.t
val return_error : string -> Result.t

module Error : sig
  val unexpected_char : char -> string
  val unexpected_eof : string
end
