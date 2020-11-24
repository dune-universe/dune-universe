
let extract_extra_packets ic _ =
  let length_current = ref 16777215 in
  let length = ref 0 in
  let number = ref 0 in
  let acc_bits = ref Bitstring.empty_bitstring in
  let f () = 
    let bits = Bitstring.bitstring_of_chan_max ic 4 in
    match%bitstring bits with
    | {| packet_length : 3*8 : int, unsigned, littleendian;
        packet_number : 1*8 : int, unsigned, bigendian |} ->
          let bits = Bitstring.bitstring_of_chan_max ic packet_length in
          let () = length_current := packet_length in
          let () = length := !length + packet_length in
          let () = number := packet_number in 
          let () = acc_bits := Bitstring.concat [! acc_bits; bits] in
          ()
        in
        try
          let () = 
            while (!length_current = 16777215) do
              f ();
            done
          in
          (! length, ! number, ! acc_bits)
        with
        | _ -> (! length, ! number, ! acc_bits)

let extract_packet ic oc = 
  let bits = Bitstring.bitstring_of_chan_max ic 4 in
  match%bitstring bits with
  | {| packet_length : 3*8 : int, unsigned, littleendian;
      packet_number : 1*8 : int, unsigned, bigendian |} ->
        let () = 
          if (packet_length > Sys.max_string_length) then
            failwith "Packet length > max_string_length"
        in
        let bits = Bitstring.bitstring_of_chan_max ic packet_length in
        if (packet_length >= 16777215) then
          (* we may have more than 1 packet *)
          let (packet_length_extra, packet_number_extra, bits_extra) = extract_extra_packets ic oc in
          (packet_length + packet_length_extra, packet_number_extra, Bitstring.concat [bits; bits_extra])
        else
          (packet_length, packet_number, bits)

let make_packet current_num_packet bits = 
  let num = current_num_packet + 1 in
  let length = Bitstring.bitstring_length bits / 8 in
  let bits_length = 
    if (length <= 16777215) then (
      let%bitstring v = 
        {|
          length : 3*8 : int, unsigned, littleendian
        |}
      in v
    )
    else (
      failwith "Send packet length too big ( > 0xffffff )"
    )
  in
  let%bitstring packet = {|
      bits_length : Bitstring.bitstring_length bits_length : bitstring;
      num : 1*8 : int, unsigned, bigendian;
      bits : Bitstring.bitstring_length bits : bitstring
    |} 
  in
  packet
