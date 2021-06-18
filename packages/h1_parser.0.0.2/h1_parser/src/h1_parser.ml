type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Source = struct
  type t = {
    buffer : bigstring;
    mutable pos : int;
    min_pos : int;
    upper_bound : int;
  }

  let of_bigstring ?pos ?len buffer =
    let buf_len = Base_bigstring.length buffer in
    let pos = Option.value pos ~default:0 in
    if pos < 0 || pos > buf_len then
      invalid_arg
        (Printf.sprintf
           "H1_parser.Source.of_bigstring: Invalid offset %d. Buffer length: %d"
           pos buf_len);
    let len = Option.value len ~default:(buf_len - pos) in
    if len < 0 || pos + len > buf_len then
      invalid_arg
        (Printf.sprintf
           "H1_parse.Source.of_bigstring: Invalid len %d. offset: %d, \
            buffer_length: %d, requested_length: %d"
           len pos buf_len (pos + len));
    { buffer; pos; min_pos = pos; upper_bound = pos + len }

  let get t idx =
    if idx < 0 || t.pos + idx >= t.upper_bound then
      invalid_arg "H1_parser.Source.get: Index out of bounds";
    Base_bigstring.get t.buffer (t.pos + idx)

  let advance t count =
    if count < 0 || t.pos + count > t.upper_bound then
      invalid_arg
        (Printf.sprintf
           "H1_parser.Source.advance: Index out of bounds. Requested count: %d"
           count);
    t.pos <- t.pos + count

  let length t = t.upper_bound - t.pos

  let to_string t ~pos ~len =
    if
      pos < 0
      || t.pos + pos >= t.upper_bound
      || len < 0
      || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "H1_parser.Source.substring: Index out of bounds., Requested off: \
            %d, len: %d"
           pos len);
    Base_bigstring.To_string.sub t.buffer ~pos:(t.pos + pos) ~len

  let consumed t = t.pos - t.min_pos

  let index t ch =
    let res =
      Base_bigstring.unsafe_find t.buffer ~pos:t.pos ch ~len:(length t)
    in
    if res = -1 then -1 else res - t.pos

  let for_all t ~pos ~len ~f =
    if
      pos < 0
      || t.pos + pos >= t.upper_bound
      || len < 0
      || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "H1_parser.Source.substring: Index out of bounds. Requested off: \
            %d, len: %d"
           pos len);
    let idx = ref pos in
    while !idx < len && f (get t !idx) do
      incr idx
    done;
    if !idx = len then true else false
end

type error = Msg of string | Partial

type 'a parser = { run : 'r. Source.t -> (error -> 'r) -> ('a -> 'r) -> 'r }
[@@unboxed]

let return x = { run = (fun _source _on_err on_succ -> on_succ x) }
let fail msg = { run = (fun _source on_err _on_succ -> on_err (Msg msg)) }

let ( let+ ) t f =
  {
    run =
      (fun source on_err on_succ ->
        t.run source on_err (fun v -> on_succ (f v)));
  }

let ( let* ) t f =
  {
    run =
      (fun source on_err on_succ ->
        t.run source on_err (fun v -> (f v).run source on_err on_succ));
  }

let ( >>= ) = ( let* )

let ( and+ ) a b =
  {
    run =
      (fun source on_err on_succ ->
        a.run source on_err (fun res_a ->
            b.run source on_err (fun res_b -> on_succ (res_a, res_b))));
  }

let ( *> ) a b =
  {
    run =
      (fun source on_err on_succ ->
        a.run source on_err (fun _res_a ->
            b.run source on_err (fun res_b -> on_succ res_b)));
  }

let ( <* ) a b =
  {
    run =
      (fun source on_err on_succ ->
        a.run source on_err (fun res_a ->
            b.run source on_err (fun _ -> on_succ res_a)));
  }

let take n =
  let run source on_err on_succ =
    if Source.length source < n then on_err Partial
    else
      let res = Source.to_string source ~pos:0 ~len:n in
      Source.advance source n;
      on_succ res
  in
  { run }

let string str =
  let run source on_err on_succ =
    let len = String.length str in
    if Source.length source < len then on_err Partial
    else
      let rec aux idx =
        if idx = len then (
          Source.advance source len;
          on_succ str)
        else if Source.get source idx = String.unsafe_get str idx then
          aux (idx + 1)
        else on_err (Msg (Printf.sprintf "Could not match: %S" str))
      in
      aux 0
  in
  { run }

let any_char =
  let run source on_err on_succ =
    if Source.length source = 0 then on_err Partial
    else
      let c = Source.get source 0 in
      Source.advance source 1;
      on_succ c
  in
  { run }

let eol = string "\r\n"

(* token = 1*tchar

   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
   / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters *)

let is_tchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
      true
  | _ -> false

let token =
  let run source on_err on_succ =
    let pos = Source.index source ' ' in
    if pos = -1 then on_err Partial
    else
      let res = Source.to_string source ~pos:0 ~len:pos in
      Source.advance source (pos + 1);
      on_succ res
  in
  { run }

let meth =
  let+ token = token in
  Cohttp.Code.method_of_string token

let version =
  string "HTTP/1."
  *> (any_char >>= function
      | '1' -> return `HTTP_1_1
      | '0' -> return `HTTP_1_0
      | _ -> fail "Invalid http version")
  <* eol

let header =
  let run source on_err on_succ =
    let pos = Source.index source ':' in
    if pos = -1 then on_err Partial
    else if pos = 0 then on_err (Msg "Invalid header: Empty header key")
    else if Source.for_all source ~pos:0 ~len:pos ~f:is_tchar then (
      let key = Source.to_string source ~pos:0 ~len:pos in
      Source.advance source (pos + 1);
      while Source.length source > 0 && Source.get source 0 = ' ' do
        Source.advance source 1
      done;
      let pos = Source.index source '\r' in
      if pos = -1 then on_err Partial
      else
        let v = Source.to_string source ~pos:0 ~len:pos in
        Source.advance source pos;
        on_succ (key, String.trim v))
    else on_err (Msg "Invalid Header Key")
  in
  { run } <* eol

let headers =
  let run source on_err on_succ =
    let rec loop acc =
      let len = Source.length source in
      if len > 0 && Source.get source 0 = '\r' then
        eol.run source on_err (fun _ -> on_succ (Cohttp.Header.of_list acc))
      else
        match header.run source (fun e -> Error e) (fun v -> Ok v) with
        | Error e -> on_err e
        | Ok v -> loop (v :: acc)
    in
    loop []
  in
  { run }

let chunk_length =
  let run source on_err on_succ =
    let ( lsl ) = Int64.shift_left in
    let ( lor ) = Int64.logor in

    let length = ref 0L in
    let stop = ref false in
    let state = ref `Ok in
    let count = ref 0 in

    let processing_chunk = ref true in

    let in_chunk_extension = ref false in

    while not !stop do
      if Source.length source = 0 then (
        stop := true;
        state := `Partial)
      else if !count = 16 && not !in_chunk_extension then (
        stop := true;
        state := `Chunk_too_big)
      else
        let ch = Source.get source 0 in
        Source.advance source 1;
        incr count;
        match ch with
        | '0' .. '9' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code '0') in
            length := (!length lsl 4) lor curr
        | 'a' .. 'f' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'a' + 10) in
            length := (!length lsl 4) lor curr
        | 'A' .. 'F' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'A' + 10) in
            length := (!length lsl 4) lor curr
        | ';' when not !in_chunk_extension ->
            in_chunk_extension := true;
            processing_chunk := false
        | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
        | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk
          ->
            ()
        | '\r' ->
            if Source.length source = 0 then (
              stop := true;
              state := `Partial)
            else if Source.get source 0 = '\n' then (
              Source.advance source 1;
              stop := true)
            else (
              stop := true;
              state := `Expected_newline)
        | _ when !in_chunk_extension ->
            (* Chunk extensions aren't very common, see:
               https://tools.ietf.org/html/rfc7230#section-4.1.1

               Chunk extensions aren't pre-defined, and they are specific to
               invidividual connections. In the future we might surface these to
               the user somehow, but for now we will ignore any extensions.

               TODO: Should there be any limit on the size of chunk extensions
               we parse? We might want to error if a request contains really
               large chunk extensions. *)
            ()
        | ch ->
            stop := true;
            state := `Invalid_char ch
    done;
    match !state with
    | `Ok -> on_succ !length
    | `Partial -> on_err Partial
    | `Expected_newline -> on_err (Msg "Expected_newline")
    | `Chunk_too_big -> on_err (Msg "Chunk size is too large")
    | `Invalid_char ch ->
        on_err (Msg (Printf.sprintf "Invalid chunk_length character %C" ch))
  in

  { run }

let guess_encoding ?(encoding = Cohttp.Transfer.Fixed Int64.zero) headers =
  match Cohttp.Header.get_transfer_encoding headers with
  | Cohttp.Transfer.(Chunked | Fixed _) as enc -> enc
  | Unknown -> encoding

let request =
  let+ meth = meth
  and+ path = token
  and+ version = version
  and+ headers = headers in
  {
    Cohttp.Request.headers;
    meth;
    scheme = None;
    resource = path;
    version;
    encoding = guess_encoding headers;
  }

let parse_chunk =
  let* chunk_length = chunk_length in
  if chunk_length = 0L then
    let+ _ = eol in
    None
  else
    let+ res = take (Int64.to_int chunk_length) <* eol in
    Some res

let run_parser ?pos ?len buf p =
  let source = Source.of_bigstring ?pos ?len buf in
  p.run source (fun e -> Error e) (fun v -> Ok (v, Source.consumed source))

let parse_request ?pos ?len buf = run_parser ?pos ?len buf request
let parse_headers ?pos ?len buf = run_parser ?pos ?len buf headers
let parse_chunk_length ?pos ?len buf = run_parser ?pos ?len buf chunk_length
let parse_chunk ?pos ?len buf = run_parser ?pos ?len buf parse_chunk
