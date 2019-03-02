open Core

module State = struct
  type t =
    [ `Header  (** Initial state, parsing headers *)
    | `Content
    (** Parsing the body of the message. The details are in the body state. *)
    | `Expected_eof  (** The message should end here. If it doesn't, it's an error *) ]

  let initial = `Header
end

module Content = struct
  type t =
    | Multipart of string list
    | Octet_stream

  let default = Octet_stream
end

type t =
  { mutable state : State.t
  ; buf : Email_grammar.token Queue.t
  }

let create () = { state = State.initial; buf = Queue.create () }

module Result = struct
  type t =
    { new_state : State.t option
    ; tokens : Email_grammar.token list
    }

  module Std = struct
    let return ?new_state tokens = { new_state; tokens }
    let return_eof = return ~new_state:`Expected_eof [ Email_grammar.EOF ]
    let return_error str = return [ Email_grammar.ERROR str ]
  end
end

let combine t result =
  (match result.Result.new_state with
   | Some state -> t.state <- state
   | None -> ());
  List.iter result.Result.tokens ~f:(fun tok -> Queue.enqueue t.buf tok)
;;

include Result.Std

module Error = struct
  let unexpected_char c = sprintf "Unexpected char: %c" c
  let unexpected_eof = "Unexpected end of file"
end
