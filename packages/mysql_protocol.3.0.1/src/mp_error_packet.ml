
type error_packet = {
  error_errno : int;
  error_sqlstate : string;
  error_message : string;
}

let error_packet_to_string p =
  Printf.sprintf "error_errno : %u\nerror_sqlstate : %s\nerror_message : %s\n"
    p.error_errno p.error_sqlstate p.error_message

let error_packet bits =
  let length_message = (Bitstring.bitstring_length bits) - ((2+1+5)*8) in
  match%bitstring bits with
  | {| errno : 2*8 : int, unsigned, littleendian;
      "#" : 1*8 : string;
      state : 5*8 : string;
      message : length_message : string |} -> (
        {
          error_errno = errno;
          error_sqlstate = state;
          error_message = message;
        }
      )