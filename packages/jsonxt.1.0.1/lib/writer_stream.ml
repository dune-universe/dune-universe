module type Intf = sig
  type t

  val create_encoder'
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> incr:int
    -> eol:string
    -> t

  val create_encoder
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  val create_encoder_hum
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  val create_encoder_channel : out_channel -> t
  val create_encoder_channel_hum : out_channel -> t

  val encode_stream_exn : t -> 'a Json_internal.constrained_stream -> unit
  val encode_stream : t -> 'a Json_internal.constrained_stream -> (unit, string) result
end

module Make (Compliance : Compliance.S) : Intf = struct
  type state = Token | List_start | List_next | Object_name | Object_name_next | Object_value | 
               Tuple_start | Tuple_next | Variant_start | Variant_value | Variant_end
  type t = {
    stack : (state * int) Stack.t
  ; add_char : (char -> unit)
  ; add_string : (string -> unit)
  ; incr : int
  ; eol : string
  }

  let create_encoder' ~add_char ~add_string ~incr ~eol = {
    stack = Stack.create ()
  ; add_char
  ; add_string
  ; incr
  ; eol
  }

  let create_encoder ~add_char ~add_string = create_encoder' ~add_char ~add_string ~incr:0 ~eol:""
  let create_encoder_hum ~add_char ~add_string = create_encoder' ~add_char ~add_string ~incr:2 ~eol:"\n"

  let create_encoder_channel oc =
    let add_char = output_char oc in
    let add_string = output_string oc in
      create_encoder ~add_char ~add_string

  let create_encoder_channel_hum oc =
    let add_char = output_char oc in
    let add_string = output_string oc in
      create_encoder_hum ~add_char ~add_string

  let nibble_to_hex i = char_of_int (if i > 9 then 65 + i - 10 else 48 + i)

  let add_hex_byte add_char i =
    add_char (nibble_to_hex ((i lsr 4) land 0x0f));
    add_char (nibble_to_hex (i land 0x0f))

  let escape ~add_char ~add_string s =
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
        add_string "\\u00";  add_hex_byte add_char (int_of_char c)
      | _      -> add_char s.[i]
    done
   
  let encode_stream_exn t (tok:'a Json_internal.constrained_stream) =
    let add_char = t.add_char in
    let add_string = t.add_string in
    let add_quote_string s = add_char '"'; escape ~add_char ~add_string s; add_char '"' in
    let add_leader off = add_string (String.make off ' ') in
    let add_eol () = add_string t.eol in
    let add_comma_eol_ldr off = add_char ','; add_eol (); add_leader off in
    let add_int i = add_string (string_of_int i) in
    let add_float f = add_string (Compliance.number_to_string f) in
    let end_of_obj_list off c = add_leader (off - t.incr); add_char c in
    let fmt t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Null -> add_string "null"
      | `Bool b -> add_string (string_of_bool b)
      | `Int i -> add_int i
      | `Intlit s -> add_string s
      | `Float f -> add_float f
      | `Floatlit s -> add_string s
      | `String s -> add_quote_string s
      | `Stringlit s -> add_string s
      | `Name s -> add_quote_string s; add_char ':'
      | `Infinity -> add_string "inf"
      | `Neg_infinity -> add_string "-inf"
      | `Nan -> add_string "nan"
      | `As ->
        add_char '['; add_eol ();
        Stack.push (List_start, (off + t.incr)) t.stack
      | `Ae -> ()
      | `Os ->
        add_char '{'; add_eol ();
        Stack.push (Object_name, (off + t.incr)) t.stack
      | `Oe -> ()
      | `Ts ->
        add_char '('; add_eol ();
        Stack.push (Tuple_start, (off + t.incr)) t.stack
      | `Te -> ()
      | `Vs ->
        add_char '<'; add_eol ();
        Stack.push (Variant_start, (off + t.incr)) t.stack
      | `Ve -> ()
    in
    let fmt_list_start t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Ae -> end_of_obj_list off ']'
      | _ -> let () = Stack.push (List_next, off) t.stack in add_leader off; fmt t off tok
    in
    let fmt_list_next t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Ae -> add_eol (); end_of_obj_list off ']'
      | _ -> let () = Stack.push (List_next, off) t.stack in add_comma_eol_ldr off; fmt t off tok
    in
    let fmt_object_name t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Oe -> end_of_obj_list off '}'
      | `Name s -> add_leader off; add_quote_string s; add_char ':'; Stack.push (Object_value, off) t.stack
      | _ -> raise (Failure "Unexpected token, expected object key")
    in
    let fmt_object_name_next t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Oe -> add_eol (); end_of_obj_list off '}'
      | `Name s -> add_comma_eol_ldr off; add_quote_string s; add_char ':'; Stack.push (Object_value, off) t.stack
      | _ -> raise (Failure "Unexpected token, expected object key")
    in
    let fmt_object_value t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Oe -> raise (Failure "Unexpected '`Oe', expected object value")
      | _ -> Stack.push (Object_name_next, off) t.stack; fmt t off tok
    in
    let fmt_tuple_start t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Te -> end_of_obj_list off ')'
      | _ -> let () = Stack.push (Tuple_next, off) t.stack in add_leader off; fmt t off tok
    in
    let fmt_tuple_next t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Te -> add_eol (); end_of_obj_list off ')'
      | _ -> let () = Stack.push (Tuple_next, off) t.stack in add_comma_eol_ldr off; fmt t off tok
    in
    let fmt_variant_start t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Name s -> add_leader off; add_quote_string s; Stack.push (Variant_value, off) t.stack
      | _ -> raise (Failure "Unexpected token, expected varient name")
    in
    let fmt_variant_value t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Ve -> add_eol (); end_of_obj_list off '>'
      | _ -> Stack.push (Variant_end, off) t.stack; add_char ':'; fmt t off tok
    in
    let fmt_variant_end _t off (tok:'a Json_internal.constrained_stream) =
      match tok with
      | `Ve -> add_eol (); end_of_obj_list off '>'
      | _ -> raise (Failure "Unexpected token, expected varient end (`Ve)")
    in
    let state, off = if Stack.is_empty t.stack then (Token, 0) else Stack.pop t.stack in
    match state with
    | Token -> fmt t off tok
    | List_start -> fmt_list_start t off tok
    | List_next -> fmt_list_next t off tok
    | Object_name -> fmt_object_name t off tok
    | Object_name_next -> fmt_object_name_next t off tok
    | Object_value -> fmt_object_value t off tok
    | Tuple_start -> fmt_tuple_start t off tok
    | Tuple_next -> fmt_tuple_next t off tok
    | Variant_start -> fmt_variant_start t off tok
    | Variant_value -> fmt_variant_value t off tok
    | Variant_end -> fmt_variant_end t off tok
      

  let encode_stream t (tok:'a Json_internal.constrained_stream) =
    try Ok (encode_stream_exn t tok) with
    | Failure err -> Error err
end
