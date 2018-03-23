module Stable_no_v1_bin_io = struct
  open !Core.Core_stable

  module V1 = struct
    type t =
      { headers     : Headers.Stable.V1.t
      ; raw_content : Email_raw_content.Stable.V1.t
      } [@@deriving sexp]
  end
end

open Core

open Or_error.Monad_infix

module T = struct
  type t = Stable_no_v1_bin_io.V1.t =
    { headers     : Headers.t
    ; raw_content : Email_raw_content.t
    } [@@deriving sexp_of, fields, compare, hash]
end
include T
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)

(* The default type of a message depends on the type of its parent,
   so we need to pass it around. *)
let of_bigstring_shared bstr =
  let lexbuf = Bigstring_shared.to_lexbuf bstr in
  begin
    try Ok (Email_grammar.message
              (Email_lexer.message (Email_lexer_state.create ())) lexbuf)
    with _ ->
      (* Looks like lexer just throws Failure, not Parsing.Parse_error *)
      let pos = lexbuf.Lexing.lex_curr_p in
      Or_error.error_string
        (sprintf "Error parsing email at line %d, column %d"
           pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
  end
  >>| fun (`Message (headers, content_offset)) ->
  let headers = Headers.of_list ~whitespace:`Raw headers in
  let raw_content =
    match content_offset with
    | `Truncated -> None
    | `Bad_headers pos ->
      Some (Bigstring_shared.sub ~pos bstr)
    | `Content_offset pos ->
      Some (Bigstring_shared.sub ~pos bstr)
  in
  { headers
  ; raw_content = Email_raw_content.Expert.of_bigstring_shared_option raw_content
  }
;;

let of_string str =
  of_bigstring_shared (Bigstring_shared.of_string str)
  |> Or_error.ok_exn
;;

let of_bigstring bstr =
  of_bigstring_shared (Bigstring_shared.of_bigstring bstr)
  |> Or_error.ok_exn
;;

let of_bigbuffer buffer =
  of_bigstring (Bigbuffer.big_contents buffer)
;;

(* Message bodies are optional. I highly doubt anybody would handle [None] differently
   from [Some ""], so we don't expose this detail. It allows us to be smarter with
   [to_string] so we don't add a newline. *)
let to_string_monoid ?(eol_except_raw_content=`LF) t =
  let optional_body =
    match Email_raw_content.Expert.to_bigstring_shared_option t.raw_content with
    | None -> []
    | Some raw_content ->
      [ String_monoid.concat
          [ String_monoid.of_string (Lf_or_crlf.to_string eol_except_raw_content)
          ; String_monoid.of_bigstring (Bigstring_shared.to_bigstring raw_content)
          ]
      ]
  in
  String_monoid.concat
    (Headers.to_string_monoid ~eol:eol_except_raw_content t.headers :: optional_body)
;;

let to_string ?eol_except_raw_content t =
  String_monoid.to_string (to_string_monoid ?eol_except_raw_content t)
;;

let to_bigstring ?eol_except_raw_content t =
  String_monoid.to_bigstring (to_string_monoid ?eol_except_raw_content t)
;;

let to_bigstring_shared ?eol_except_raw_content t =
  Bigstring_shared.of_string_monoid (to_string_monoid ?eol_except_raw_content t)
;;

let create = Fields.create

let set_headers     t headers     = { t with headers     }
let set_raw_content t raw_content = { t with raw_content }

let modify_headers     t ~f = set_headers     t (f t.headers)
let modify_raw_content t ~f = set_raw_content t (f (raw_content t))

let save ?temp_file ?perm ?fsync ?eol_except_raw_content t path =
  let open Async in
  Writer.with_file_atomic ?temp_file ?perm ?fsync path
    ~f:(fun writer ->
      String_monoid.iter (to_string_monoid ?eol_except_raw_content t) ~f:(function
        | String_monoid.Underlying.Char c ->
          Writer.write_char writer c
        | String str ->
          Writer.write writer str
        | Bigstring bstr ->
          Writer.schedule_bigstring writer bstr);
      return ())

module Stable = struct
  module V1 = struct
    include Stable_no_v1_bin_io.V1

    include Binable.Of_binable(Bigstring)(struct
        type nonrec t = t
        let to_binable t = to_bigstring t
        let of_binable = of_bigstring
      end)
  end
end
