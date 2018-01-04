open Core;;

module State = struct
  type t = [
    (** Initial state, parsing headers *)
      `Header  |

      (** Parsing the body of the message. The details are in the body state. *)
      `Content |

      (** The message should end here. If it doesn't, it's an error *)
      `Expected_eof
  ]
  ;;

  let initial = `Header;;
end

module Content = struct
  type t =
      Multipart of string list |
      Octet_stream

  let default = Octet_stream;;
end

type t =
  {
    mutable state : State.t;
    buf : Email_grammar.token Queue.t;
  }
;;

let create () = {
  state = State.initial;
  buf = Queue.create ();
};;

module Result = struct
  type t = {
    new_state : State.t option;
    tokens : Email_grammar.token list;
  }

  module Std = struct
    let return ?new_state tokens =
      {
        new_state = new_state;
        tokens = tokens
      }
    ;;

    let return_eof = return ~new_state:`Expected_eof [Email_grammar.EOF];;
    let return_error str = return [Email_grammar.ERROR str];;
  end
end

let combine t result =
  begin
    match result.Result.new_state with
    | Some state -> t.state <- state
    | None       -> ()
  end
  ;
  List.iter result.Result.tokens
    ~f:(fun tok -> Queue.enqueue t.buf tok)
;;

include Result.Std;;

module Error = struct
  let unexpected_char c = sprintf "Unexpected char: %c" c
  let unexpected_eof = "Unexpected end of file"
end
