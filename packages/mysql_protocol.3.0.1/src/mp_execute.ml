type flag = 
    Cursor_type_no_cursor
  | Cursor_type_read_only
  | Cursor_type_for_update
  | Cursor_type_scrollable

type bind = 
    Bind
  | No_bind

let flag_to_int f = 
  match f with
  | Cursor_type_no_cursor -> 0
  | Cursor_type_read_only -> 1
  | Cursor_type_for_update -> 2
  | Cursor_type_scrollable -> 4

let bind_to_int b = 
  match b with
  | Bind -> 1
  | No_bind -> 0

let build_params_part_null params = 
  match params with
  | [] -> Bitstring.empty_bitstring
  | p -> (
      let nb_params = List.length p in
      let nb_null_bytes = (nb_params + 7) / 8 in
      let nb_null_bits = nb_null_bytes * 8 in
      let bitstring_null = Bitstring.create_bitstring nb_null_bits in
      let () =
        let count_param = ref 0 in
        let set_null_bit e =
          let bloc_num = !count_param / 8 in
          let pos_bit = (bloc_num * 8) + 7 - (!count_param mod 8) in
          match e with
          | Mp_data.Null -> (
              let () = Bitstring.set bitstring_null pos_bit in
              incr count_param
            )
          | _ -> incr count_param
        in
        List.iter set_null_bit p
      in
      bitstring_null
    )

let build_params_part_data bind params fields =
  match params with
  | [] -> ([], [])
  | _ ->
    let f acc param _ =
      let type_number =
        match bind with
        | Bind -> Mp_data_process.to_type_number param
        | No_bind -> Bitstring.empty_bitstring
      in
      let data = Mp_data_process.to_bitstring param in
      let (lt, ld) = acc in
      (type_number::lt, data::ld)
    in
    List.fold_left2 f ([], []) params fields

let build_params_part bind params fields =
  let flag_bind = bind_to_int bind in
  let bitstring_null = build_params_part_null params in
  let (bitstring_type, bitstring_data) = build_params_part_data bind params fields in
  let bitstring_type = List.rev bitstring_type in
  let bitstring_type = Bitstring.concat bitstring_type in
  let bitstring_data = List.rev bitstring_data in
  let bitstring_data = Bitstring.concat bitstring_data in
  let nb_null_bits = Bitstring.bitstring_length bitstring_null in
  let nb_bitstring_type = Bitstring.bitstring_length bitstring_type in
  let nb_bitstring_data = Bitstring.bitstring_length bitstring_data in
  let%bitstring bits =
      {|
        bitstring_null : nb_null_bits : bitstring;
        flag_bind : 1*8 : int, unsigned;
        bitstring_type : nb_bitstring_type : bitstring;
        bitstring_data : nb_bitstring_data : bitstring
      |}
  in
  bits

let build_execute ~handler ?(flag = Cursor_type_no_cursor) ?(params = []) ?(fields = []) ?(bind = Bind) () = 
  let flag = flag_to_int flag in
  let iteration_count = Int64.one in
  let part_params = build_params_part bind params fields in
  let%bitstring bits = 
      {|
        handler : Mp_bitstring.compute32 : int, unsigned, littleendian;
        flag : 1*8 : int, unsigned;
        iteration_count : Mp_bitstring.compute32 : int, unsigned, littleendian
      |}
  in
  Bitstring.concat [bits; part_params]
