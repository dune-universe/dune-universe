module type IO = Io.IO

module type Writer_monad = sig
  module IO : IO

  val json_writer
       : writer:(string -> unit IO.t)
      -> eol:string
      -> incr:int
      -> psep:string
      -> 'a Json_internal.constrained
      -> unit IO.t
  val write_json : writer:(string -> unit IO.t) -> 'a Json_internal.constrained -> unit IO.t
  val write_json_hum : writer:(string -> unit IO.t) -> 'a Json_internal.constrained -> unit IO.t
end

module Make (Compliance : Compliance.S) (IO : IO) : Writer_monad with module IO := IO = struct

  open IO

  let nibble_to_hex i = char_of_int (if i > 9 then 65 + i - 10 else 48 + i)

  let add_hex_byte buf i =
    Buffer.add_char buf (nibble_to_hex ((i lsr 4) land 0x0f));
    Buffer.add_char buf (nibble_to_hex (i land 0x0f))

  let escape s =
    let buf = Buffer.create 100 in
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
    done;
    Buffer.contents buf
   
  let json_writer ~writer ~eol ~incr ~psep json = 
    let psep = ":" ^ psep in
    let string_of_char c = String.make 1 c in
    let write_char c = writer (string_of_char c) in
    let write_string = writer in
    let write_quote_string s =
      write_char '"'
      >>= fun () -> writer (escape s)
      >>= fun () -> write_char '"'
    in
    let write_int i = write_string (string_of_int i) in
    let write_float f = write_string (Compliance.number_to_string f) in
    let write_list off f l = 
      let ldr = String.make off ' ' in
      let rec loop = function
        | [] -> return ()
        | hd::tl -> write_string ("," ^ eol ^ ldr) >>= fun () -> f hd >>= fun () -> loop tl
      in
      let first = function
        | [] -> return ()
        | hd::tl -> write_string (eol ^ ldr) >>= fun () -> f hd >>= fun () -> loop tl
      in
      first l
    in
    let rec fmt off value =
      match value with
      | `Assoc o ->
        let ldr = String.make off ' ' in
        write_string "{"
        >>= fun () -> json_assoc (off + incr) o
        >>= fun () -> write_string (eol ^ ldr ^ "}")
      | `List l ->
        let ldr = String.make off ' ' in
        write_string "["
        >>= fun () -> json_list (off + incr) l;
        >>= fun () -> write_string (eol ^ ldr ^ "]")
      | `Null -> write_string "null"
      | `Bool b -> write_string (string_of_bool b)
      | `Int i -> write_int i
      | `Intlit s -> write_string s
      | `Float f -> write_float f
      | `Floatlit s -> write_string s
      | `String s -> write_quote_string s
      | `Stringlit s -> write_string s
      | `Tuple t ->
        let ldr = String.make off ' ' in
        write_string ("(" ^ eol)
        >>= fun () -> json_list (off + incr) t
        >>= fun () -> write_string (eol ^ ldr ^ ")")
      | `Variant v ->
        let ldr = String.make off ' ' in
        write_string ("<" ^ eol)
        >>= fun () -> variant (off + incr) v
        >>= fun () -> write_string (eol ^ ldr ^ ">")
    and json_assoc off o =
      write_list off (fun v -> pair off v) o
    and pair off (k, v) = write_quote_string k >>= fun () -> write_string psep >>= fun () -> fmt off v
    and json_list off l =
      write_list off (fun v -> fmt off v) l
    and variant off (k, j) =
      write_quote_string k
      >>= fun () ->
        match j with
        | Some j -> write_string psep >>= fun () -> fmt (off + incr) j
        | None -> return ()
    in
    fmt 0 json >>= fun () -> write_string eol

  let write_json ~writer json = json_writer ~writer ~eol:"" ~incr:0 ~psep:"" json
  let write_json_hum ~writer json = json_writer ~writer ~eol:"\n" ~incr:2 ~psep:" " json
end
