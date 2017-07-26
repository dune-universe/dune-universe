open! Base

module Positions = Positions
module Cst       = Cst
module A         = Parser_automaton

let rec feed_substring_unsafe str state stack i stop =
  if i < stop then
    let c = String.unsafe_get str i in
    let stack = A.feed state c stack in
    feed_substring_unsafe str state stack (i + 1) stop
  else
    stack

let feed_substring state str ~pos ~len stack =
  let str_len = String.length str in
  if pos < 0 || len < 0 || pos > str_len - len then
    invalid_arg "Parsexp.feed_substring";
  feed_substring_unsafe str state stack pos (pos + len)

let feed_string state str stack =
  feed_substring_unsafe str state stack 0 (String.length str)

module Parse_error = struct
  include A.Error

  let report ppf ~filename t =
    let pos = position t in
    let msg = message  t in
    Caml.Format.fprintf ppf
      "File \"%s\", line %d, character %d:\n\
       Error: s-expression parsing error;\n\
       %s\n"
      filename pos.line pos.col msg
end

module Of_sexp_error = struct
  type t =
    { user_exn : exn
    ; sub_sexp : Sexp.t
    ; location : Positions.range option
    }
  [@@deriving sexp_of]

  let user_exn t = t.user_exn
  let sub_sexp t = t.sub_sexp
  let location t = t.location

  let report ppf ~filename t =
    let line, start, stop =
      match t.location with
      | None -> (1, 0, 0)
      | Some { start_pos; end_pos } ->
        (start_pos.line,
         start_pos.col,
         start_pos.col + end_pos.offset - start_pos.offset)
    in
    Caml.Format.fprintf ppf
      "File \"%s\", line %d, characters %d-%d:\n\
       Error: s-expression conversion error;\n\
       exception %s\n"
      filename line start stop (Exn.to_string t.user_exn)
end

module Conv_error = struct
  type t =
    | Parse_error   of Parse_error.t
    | Of_sexp_error of Of_sexp_error.t
  [@@deriving sexp_of]

  let report ppf ~filename t =
    match t with
    | Parse_error   e -> Parse_error.  report ppf ~filename e
    | Of_sexp_error e -> Of_sexp_error.report ppf ~filename e
end

module type Parser = Parsexp_intf.Parser
  with module Error := Parse_error

module type Conv = Parsexp_intf.Conv
  with module Parse_error   := Parse_error
  with module Of_sexp_error := Of_sexp_error
  with module Conv_error    := Conv_error

module type Eager_parser = Parsexp_intf.Eager_parser

exception Parse_error = A.Parse_error
exception Of_sexp_error of Of_sexp_error.t [@@deriving sexp_of]


module Make(Params : sig
    type parsed_value
    type state
    type stack
    val kind       : (state, stack) A.kind
    val mode       : (state, stack) A.mode
    val empty      : stack
    val make_value : (state, stack) A.state -> stack -> parsed_value
  end) : Parser
  with type parsed_value = Params.parsed_value
  with type State.t = (Params.state, Params.stack) A.state
  with type Stack.t = Params.stack
= struct
  type parsed_value = Params.parsed_value

  module Stack = struct
    type t = Params.stack
    let empty = Params.empty
  end

  module State = struct
    type t = (Params.state, Stack.t) A.state
    let create ?pos () = A.new_state ?initial_pos:pos Params.mode Params.kind
    let reset  = A.reset
    let offset = A.offset
    let line   = A.line
    let column = A.column

    let position t : Positions.pos =
      { offset = offset t
      ; line   = line   t
      ; col    = column t
      }

    let stop state = A.set_error_state state
  end

  let feed = A.feed
  let feed_eoi state stack = Params.make_value state (A.feed_eoi state stack)

  let feed_substring = feed_substring
  let feed_string    = feed_string

  let parse_string_exn str =
    let state = State.create () in
    feed_eoi state (feed_string state str Stack.empty)

  let parse_string str =
    match parse_string_exn str with
    | x                         -> Ok x
    | exception (Parse_error e) -> Error e
end

module Make_eager(Params : sig
    type parsed_value
    type state
    type stack
    val kind    : (state, stack) A.kind
    val empty   : stack
    val make_value : (state, stack) A.state -> stack -> parsed_value
  end) : Eager_parser
  with type parsed_value = Params.parsed_value
  with type State.t = (Params.state, Params.stack) A.state
  with type Stack.t = Params.stack
= struct
  type parsed_value = Params.parsed_value

  module Stack = struct
    type t = Params.stack
    let empty = Params.empty
  end

  module State = struct
    module Read_only = struct
      type t = (Params.state, Stack.t) A.state

      let offset = A.offset
      let line   = A.line
      let column = A.column

      let position t : Positions.pos =
        { offset = offset t
        ; line   = line   t
        ; col    = column t
        }
    end
    include Read_only

    let create ?pos ?(no_sexp_is_error=false) f =
      let got_sexp state stack =
        let parsed_value = Params.make_value state stack in
        f state parsed_value;
        Params.empty
      in
      A.new_state ?initial_pos:pos (Eager { got_sexp; no_sexp_is_error }) Params.kind
    let reset  = A.reset

    let stop t = A.set_error_state t
  end

  let feed = A.feed

  let feed_eoi state stack =
    ignore (A.feed_eoi state stack : Stack.t)

  let feed_substring = feed_substring
  let feed_string    = feed_string

  module Lexbuf_consumer = struct
    type t = State.t

    exception Got_sexp of parsed_value * Positions.pos
    let got_sexp state parsed_value =
      Exn.raise_without_backtrace (Got_sexp (parsed_value, State.position state))

    let create () = State.create got_sexp

    let pos_of_lexbuf lexbuf =
      let p = lexbuf.Lexing.lex_curr_p in
      { Positions.
        line   = p.pos_lnum
      ; col    = p.pos_cnum - p.pos_bol
      ; offset = p.pos_cnum
      }

    let update_lexbuf (lexbuf : Lexing.lexbuf) (pos : Positions.pos) =
      let p = pos.offset - lexbuf.lex_abs_pos in
      lexbuf.lex_curr_pos <- p;
      lexbuf.lex_start_pos <- p;
      lexbuf.lex_curr_p <-
        { lexbuf.lex_curr_p with
          pos_lnum = pos.line
        ; pos_cnum = pos.offset
        ; pos_bol  = pos.offset - pos.col
        }

    let rec feed_lexbuf t (lexbuf : Lexing.lexbuf) stack =
      let stack =
        feed_substring t lexbuf.lex_buffer stack
          ~pos:lexbuf.lex_curr_pos
          ~len:(lexbuf.lex_buffer_len - lexbuf.lex_curr_pos)
      in
      lexbuf.lex_curr_pos <- lexbuf.lex_buffer_len;
      lexbuf.lex_start_pos <- lexbuf.lex_buffer_len;
      if not lexbuf.lex_eof_reached then begin
        lexbuf.refill_buff lexbuf;
        feed_lexbuf t lexbuf stack
      end else
        feed_eoi t stack

    let parse_gen t (lexbuf : Lexing.lexbuf) =
      A.reset t ~pos:(pos_of_lexbuf lexbuf);
      match feed_lexbuf t lexbuf Stack.empty with
      | () ->
        update_lexbuf lexbuf (State.position t);
        None
      | exception Got_sexp (parsed_value, pos) ->
        update_lexbuf lexbuf pos;
        Some parsed_value
      | exception exn ->
        update_lexbuf lexbuf (State.position t);
        raise exn

    let set_no_sexp_is_error t x =
      match A.mode t with
      | Eager e -> e.no_sexp_is_error <- x
      | _ -> assert false

    let parse t lexbuf =
      set_no_sexp_is_error t true;
      Option.value_exn (parse_gen t lexbuf)

    let parse_opt t lexbuf =
      set_no_sexp_is_error t false;
      parse_gen t lexbuf
  end
end

module Single = Make(struct
    type parsed_value = Sexp.t
    type stack = A.stack
    type state = unit
    let kind = A.Sexp
    let mode = A.Single
    let empty = A.empty_stack
    let make_value _ stack = A.sexp_of_stack stack
  end)

module Many = Make(struct
    type parsed_value = Sexp.t list
    type stack = A.stack
    type state = unit
    let kind = A.Sexp
    let mode = A.Many
    let empty = A.empty_stack
    let make_value _ stack = A.sexps_of_stack stack
  end)

module Eager = Make_eager(struct
    type parsed_value = Sexp.t
    type stack = A.stack
    type state = unit
    let kind = A.Sexp
    let empty = A.empty_stack
    let make_value _ stack = A.sexp_of_stack stack
  end)

module Single_and_positions = Make(struct
    type parsed_value = Sexp.t * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t
    let kind = A.Sexp_with_positions
    let mode = A.Single
    let empty = A.empty_stack
    let make_value state stack = (A.sexp_of_stack stack, A.positions state)
  end)

module Many_and_positions = Make(struct
    type parsed_value = Sexp.t list * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t
    let kind = A.Sexp_with_positions
    let mode = A.Many
    let empty = A.empty_stack
    let make_value state stack = (A.sexps_of_stack stack, A.positions state)
  end)

module Eager_and_positions = Make_eager(struct
    type parsed_value = Sexp.t * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t
    let kind = A.Sexp_with_positions
    let empty = A.empty_stack
    let make_value state stack = (A.sexp_of_stack stack, A.positions state)
  end)

module Single_just_positions = Make(struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t
    let kind = A.Positions
    let mode = A.Single
    let empty = ()
    let make_value state () = A.positions state
  end)

module Many_just_positions = Make(struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t
    let kind = A.Positions
    let mode = A.Many
    let empty = ()
    let make_value state () = A.positions state
  end)

module Eager_just_positions = Make_eager(struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t
    let kind = A.Positions
    let empty = ()
    let make_value state () = A.positions state
  end)

module Many_cst = Make(struct
    type parsed_value = Cst.t_or_comment list
    type stack = A.stack_cst
    type state = A.state_cst
    let kind = A.Cst
    let mode = A.Many
    let empty = A.empty_stack_cst
    let make_value _ stack = A.sexps_cst_of_stack stack
  end)

module Eager_cst = Make_eager(struct
    type parsed_value = Cst.t_or_comment
    type stack = A.stack_cst
    type state = A.state_cst
    let kind = A.Cst
    let empty = A.empty_stack_cst
    let make_value _ stack =
      match A.sexps_cst_of_stack stack with
      | [sexp] -> sexp
      | _ -> assert false
  end)

module Make_conv
    (Mode : sig
       type 'a t
       val map : Sexp.t t -> f:(Sexp.t -> 'a) -> 'a t
       val find : Positions.t -> Sexp.t t -> sub:Sexp.t -> Positions.range option
     end)
    (Parser      : Parser with type parsed_value = Sexp.t Mode.t)
    (Parser_pos  : Parser with type parsed_value = Positions.t)
= struct
  type 'a single_or_many = 'a Mode.t

  let reraise positions parsed_value ~sub exn =
    let loc = Mode.find positions parsed_value ~sub in
    raise (Of_sexp_error
             { user_exn = exn
             ; sub_sexp = sub
             ; location = loc
             })

  let parse_string_exn str f =
    let parsed_value = Parser.parse_string_exn str in
    match Mode.map parsed_value ~f with
    | x -> x
    | exception (Sexp.Of_sexp_error (exn, sub)) ->
      let positions = Parser_pos.parse_string_exn str in
      reraise positions parsed_value exn ~sub

  let parse_string str f : (_, Conv_error.t) Result.t =
    match parse_string_exn str f with
    | x                           -> Ok x
    | exception (Parse_error   e) -> Error (Parse_error   e)
    | exception (Of_sexp_error e) -> Error (Of_sexp_error e)

  let conv_exn (parsed_value, positions) f =
    match Mode.map parsed_value ~f with
    | x -> x
    | exception (Sexp.Of_sexp_error (exn, sub)) ->
      reraise positions parsed_value exn ~sub

  let conv x f =
    match conv_exn x f with
    | x                           -> Ok x
    | exception (Of_sexp_error e) -> Error e

  let conv_combine result f : (_, Conv_error.t) Result.t =
    match result with
    | Error e ->
      Error (Parse_error e)
    | Ok x ->
      match conv x f with
      | Ok _ as r -> r
      | Error e ->
        Error (Of_sexp_error e)
end

module Conv_single =
  Make_conv
    (struct
      type 'a t = 'a
      let map x ~f = f x
      let find = Positions.find_sub_sexp_phys
    end)
    (Single)
    (Single_just_positions)

module Conv_many =
  Make_conv
    (struct
      type 'a t = 'a list
      let map x ~f = List.map x ~f
      let find = Positions.find_sub_sexp_in_list_phys
    end)
    (Many)
    (Many_just_positions)

module Private = struct
  module Parser_automaton = Parser_automaton
end
