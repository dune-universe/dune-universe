include Atypes
include Acommon
include State
include Ordered
include Key_value
include Properties
include Uuid
include Mvar
include Json
include Identifiers
include Apath
module List = Alist
module Stringable = Stringable
module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)


open Result
  
let encode_vle ?size v buf =
  let to_char l = char_of_int @@ Int64.to_int l in
  let rec put_positive_vle_rec ?size' v' =
    match size', v' with
    | Some(0), _ -> raise @@ Exception(`OutOfBounds (`Msg (Printf.sprintf "encode_vle: cannot encode %Ld as a %d bytes VLE" v (Option.get size))))
    | None, v' when v' <= Vle.byte_mask -> Abuf.write_byte (to_char v') buf
    | Some(1), v' when v' <= Vle.byte_mask -> Abuf.write_byte (to_char v') buf
    | _, v' ->
      let mv = Int64.logor Vle.more_bytes_flag @@ Int64.logand v' Vle.byte_mask in
      Abuf.write_byte (to_char mv) buf;
      let sv = Int64.shift_right v' Vle.shift_len in
      let size' = Option.map size' (fun s -> s-1) in
      put_positive_vle_rec sv ?size'
  in
  if v < 0L then raise @@ Exception(`OutOfRange (`Msg "encode_vle: integer to encode must be positive"))
  else put_positive_vle_rec v ?size':size

let decode_vle buf =
  let from_char c = Vle.of_int (int_of_char c) in
  let masked_from_char c = Vle.logand Vle.byte_mask  (Vle.of_int (int_of_char c)) in
  let merge v c n = Vle.logor v (Vle.shift_left c (n * Vle.shift_len)) in
  let rec decode_vle_rec  v n =
    if n < Vle.max_bytes then
      begin
        let c = Abuf.read_byte buf in
        if (from_char c) <= Vle.byte_mask then ((merge v (masked_from_char c) n))
        else decode_vle_rec (merge v (masked_from_char c) n) (n+1)      
      end
    else
      begin
        let rec skip k buf =
          let c = Abuf.read_byte buf in
          if from_char c <= Vle.byte_mask then raise @@ Exception(`OutOfBounds (`Msg "vle out of bounds"))
          else skip (k+1) buf 
        in skip n buf
      end
  in decode_vle_rec 0L 0

let rec fast_encode_vle (v:Vle.t) buf =      
  if v <= 0x7fL then Abuf.write_byte (char_of_int @@ Vle.to_int v) buf
  else 
    begin 
      let c = Vle.logor (Vle.logand v 0x7fL) 0x80L in 
      Abuf.write_byte (char_of_int @@ Vle.to_int c) buf;
      fast_encode_vle (Vle.shift_right v  7) buf
    end 

let  fast_decode_vle buf =
  let acc = ref 0L in 
  let c = ref 0L in 
  let i = ref 0 in 
  c := Vle.of_int @@ int_of_char @@ Abuf.read_byte buf;
  while !c > 0x7fL do         
    let v = Vle.logand !c 0x7fL in     
    acc := Vle.logor !acc (Vle.shift_left v !i);
    c := Vle.of_int @@ int_of_char @@ Abuf.read_byte buf;
    i := !i + 7
  done ;  
  Vle.logor !acc (Int64.shift_left !c (!i))

let rec skip_vle buf = 
  Abuf.read_byte buf |> Vle.of_char |> Vle.logand Vle.more_bytes_flag <> 0L |> function 
  | true -> skip_vle buf 
  | false -> ()

let encode_buf src dst =
  let len = Abuf.readable_bytes src in
  fast_encode_vle (Vle.of_int len) dst;
  Abuf.write_buf src dst 

let decode_buf buf =
  let len = fast_decode_vle buf |> Vle.to_int in
  Abuf.read_buf len buf
  
let encode_bytes bs buf =
  let len = Bytes.length bs in
  fast_encode_vle (Vle.of_int len) buf;
  Abuf.write_bytes bs buf
    
let decode_bytes buf =
  let len = fast_decode_vle buf |> Vle.to_int in
  Abuf.read_bytes len buf


let encode_abytes bs buf = 
  let len = Abytes.capacity bs in
  fast_encode_vle (Vle.of_int len) buf;
  Abuf.write_abytes bs buf
  
let decode_abytes buf =
  let len = fast_decode_vle buf |> Vle.to_int in
  Abuf.read_abytes len buf

let encode_string s = encode_bytes (Bytes.unsafe_of_string s)
    
let decode_string buf = 
  decode_bytes buf 
  |> Bytes.unsafe_to_string
    
let decode_seq read buf  =
  let rec get_remaining seq length =
    match length with
    | 0 -> seq
    | _ ->
      let value = read buf in
      get_remaining (value :: seq) (length - 1)
  in
  let length = fast_decode_vle buf in 
  List.rev @@ get_remaining [] (Vle.to_int length)

let encode_seq write seq buf =
  let rec put_remaining seq =
    match seq with
    | [] -> ()
    | head :: rem -> 
      write head buf;
      put_remaining rem 
  in
  fast_encode_vle (Vle.of_int (List.length seq)) buf;
  put_remaining seq 

let encode_seq_safe write seq buf =
  let rec put_remaining seq n =
    if (n = 0x3FFF) then
      (* note: 0x3FFF is the biggest length we can encode in a 2-bytes vle *)
      (n, seq)
    else match seq with
    | [] -> (n, [])
    | head :: rem ->
      Abuf.mark_w_pos buf;
      try write head buf; put_remaining rem (n+1)
      with _ -> Abuf.reset_w_pos buf; (n, seq)
  in
  (* reserve space for seq length as a 2-bytes vle *)
  let length_pos = Abuf.w_pos buf in
  encode_vle ~size:2 0L buf;
  put_remaining seq 0 |> fun (n, remain) -> 
  Abuf.mark_w_pos buf;
  Abuf.set_w_pos length_pos buf;
  encode_vle ~size:2 (Vle.of_int n) buf;
  Abuf.reset_w_pos buf;
  remain

let read1_spec log p1 c buf =    
  log ;
  p1 buf 
  |> fun a1 -> c a1

let read2_spec log p1 p2 c buf =   
  log ;
  p1 buf 
  |> fun a1 -> p2 buf 
    |> fun a2 -> c a1 a2

let read3_spec log p1 p2 p3 c buf = 
  log ;
  p1 buf 
  |> fun a1 -> p2 buf 
    |> fun a2 -> p3 buf 
      |> fun a3 -> c a1 a2 a3

let read4_spec log p1 p2 p3 p4 c buf = 
  log ;
  p1 buf 
  |> fun a1 -> p2 buf 
    |> fun a2 -> p3 buf 
      |> fun a3 -> p4 buf 
        |> fun a4 -> c a1 a2 a3 a4

let read5_spec log p1 p2 p3 p4 p5 c buf = 
  log ;
  p1 buf 
  |> fun a1 -> p2 buf 
    |> fun a2 -> p3 buf 
      |> fun a3 -> p4 buf 
        |> fun a4 -> p5 buf 
          |> fun a5 -> c a1 a2 a3 a4 a5


let read6_spec log p1 p2 p3 p4 p5 p6 c buf = 
  log ;
  p1 buf 
  |> fun a1 -> p2 buf 
    |> fun a2 -> p3 buf 
      |> fun a3 -> p4 buf 
        |> fun a4 -> p5 buf 
          |> fun a5 -> p6 buf 
            |> fun a6 -> c a1 a2 a3 a4 a5 a6

let lwt_of_result = function 
| Ok v -> Lwt.return v
| Error e -> Lwt.fail @@ Exception e


let failw_with_not_impl () = fail @@ Exception `NotImplemented
