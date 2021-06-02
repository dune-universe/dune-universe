module type Intf = sig
  val json_to_string : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_exn : 'a Json_internal.constrained -> string
  val to_string : 'a Json_internal.constrained -> string
  val json_to_string_hum : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_hum_exn : 'a Json_internal.constrained -> string
  val to_string_hum : 'a Json_internal.constrained -> string
  val json_to_buffer : Buffer.t -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_buffer_exn : Buffer.t -> 'a Json_internal.constrained -> unit
  val json_to_buffer_hum : Buffer.t -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_buffer_hum_exn : Buffer.t -> 'a Json_internal.constrained -> unit
  val to_buffer : Buffer.t -> 'a Json_internal.constrained -> unit
  val to_buffer_hum : Buffer.t -> 'a Json_internal.constrained -> unit
  val stream_to_string : 'a Json_internal.constrained Stream.t -> string
  val stream_to_buffer : Buffer.t -> 'a Json_internal.constrained Stream.t -> unit
end

module Make (Compliance : Compliance.S) : Intf = struct

  let nibble_to_hex i = char_of_int (if i > 9 then 65 + i - 10 else 48 + i)

  let add_hex_byte buf i =
    Buffer.add_char buf (nibble_to_hex ((i lsr 4) land 0x0f));
    Buffer.add_char buf (nibble_to_hex (i land 0x0f))

  let escape buf s =
    let add_char = Buffer.add_char buf in
    let add_string = Buffer.add_string buf in
    let l = String.length s in
    for i = 0 to l - 1 do
      match s.[i] with
      | '"'    -> add_string "\\\""
      | '\\'   -> add_string "\\\\"
      | '\b'   -> add_string "\\b"
      | '\012' -> add_string "\\f"
      | '\n'   -> add_string "\\n"
      | '\r'   -> add_string "\\r"
      | '\t'   -> add_string "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c ->
        add_string "\\u00";  add_hex_byte buf (int_of_char c)
      | _      -> add_char s.[i]
    done
   
  let json_to_buffer' buf json =
    let add_char = Buffer.add_char buf in
    let add_string = Buffer.add_string buf in
    let add_quote_string s = add_char '"'; escape buf s; add_char '"' in
    let add_int i = add_string (string_of_int i) in
    let add_float f = add_string (Compliance.number_to_string f) in
    let rec fmt value =
      match value with
      | `Assoc o -> add_char '{'; json_assoc o; add_char '}'
      | `List l -> add_char '['; json_list l; add_char ']'
      | `Null -> add_string "null"
      | `Bool b -> add_string (string_of_bool b)
      | `Int i -> add_int i
      | `Intlit s -> add_string s
      | `Float f -> add_float f
      | `Floatlit s -> add_string s
      | `String s -> add_quote_string s
      | `Stringlit s -> add_string s
      | `Tuple t -> add_char '('; json_list t; add_char ')'
      | `Variant v -> add_char '<';  variant v; add_char '>'
    and json_assoc o =
      let sep = ref "" in List.iter (fun v -> add_string !sep; sep := ","; pair v ) o
    and pair (k, v) = add_quote_string k; add_char ':'; fmt v
    and json_list l =
      let sep = ref "" in List.iter (fun v -> add_string !sep; sep := ","; fmt v ) l
    and variant (k, j) =
      add_quote_string k;
      match j with
      | Some j -> add_char ':'; fmt j
      | None -> ()
    in
    fmt json

  let json_to_buffer_hum' buf json =
    let add_char = Buffer.add_char buf in
    let add_string = Buffer.add_string buf in
    let add_quote_string s = add_char '"'; escape buf s; add_char '"' in
    let add_int i = add_string (string_of_int i) in
    let add_float f = add_string (Compliance.number_to_string f) in
    let rec fmt ldr value =
      match value with
      | `Assoc o ->
        add_string "{\n"; json_assoc (ldr ^ "  ") o;
        add_char '\n'; add_string ldr; add_char '}'
      | `List l ->
        add_string "[\n"; json_list (ldr ^ "  ") l;
        add_char '\n'; add_string ldr; add_char ']'
      | `Null -> add_string "null"
      | `Bool b -> add_string (string_of_bool b)
      | `Int i -> add_int i
      | `Intlit s -> add_string s
      | `Float f -> add_float f
      | `Floatlit s -> add_string s
      | `String s -> add_quote_string s
      | `Stringlit s -> add_string s
      | `Tuple t ->
        add_string "(\n"; json_list (ldr ^ "  ") t;
        add_char '\n'; add_string ldr; add_char ')'
      | `Variant v ->
        add_string "<";  variant (ldr ^ "  ") v;
        add_char '\n'; add_string ldr; add_char '>'
    and json_assoc ldr o =
      let sep = ref ldr in
      let newsep = ",\n" ^ ldr in
      List.iter (fun v -> add_string !sep; sep := newsep; pair ldr v ) o
    and pair ldr (k, v) = add_quote_string k; add_string ": "; fmt ldr v
    and json_list ldr l =
      let sep = ref ldr in
      let newsep = ",\n" ^ ldr in
      List.iter (fun v -> add_string !sep; sep := newsep; fmt ldr  v ) l
    and variant ldr (k, j) =
      add_quote_string k;
      match j with
      | Some j -> add_string ": "; fmt (ldr ^ "  ") j
      | None -> ()
    in
    fmt "" json;
    add_char '\n'

  let json_to_string' json =
    let buf = Buffer.create 100 in
    json_to_buffer' buf json;
    Buffer.contents buf

  let json_to_string json =
    try Ok (json_to_string' json) with
    | Failure err -> Error err

  let json_to_buffer buf json =
    try Ok (json_to_buffer' buf json) with
    | Failure err -> Error err

  let json_to_string_exn = json_to_string'
  let to_string = json_to_string'
  let json_to_buffer_exn = json_to_buffer'
  let to_buffer = json_to_buffer'

  let json_to_string_hum' json =
    let buf = Buffer.create 100 in
    json_to_buffer_hum' buf json;
    Buffer.contents buf

  let json_to_string_hum json =
    try Ok (json_to_string_hum' json) with
    | Failure err -> Error err

  let json_to_buffer_hum buf json =
    try Ok (json_to_buffer' buf json) with
    | Failure err -> Error err

  let json_to_string_hum_exn = json_to_string_hum'
  let to_string_hum = json_to_string_hum'
  let json_to_buffer_hum_exn = json_to_buffer_hum'
  let to_buffer_hum = json_to_buffer_hum'

  let stream_to_string stream =
    let buf = Buffer.create 100 in
    let () = Stream.iter (fun json -> to_buffer buf json; Buffer.add_char buf '\n') stream in
    Buffer.contents buf

  let stream_to_buffer buf stream =
    Stream.iter (fun json -> to_buffer buf json; Buffer.add_char buf '\n') stream

end
