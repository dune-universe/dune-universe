open CCFun
open Interface'

type opcode =
  | Continuation
  | Text
  | Binary
  | Reserved_non_control of int
  | Close
  | Ping
  | Pong
  | Reserved_control of int [@@deriving show]

type masking_key = char * char * char * char [@@deriving show]

type data = [ `Masked of masking_key * string | `Plain of string ] [@@deriving show]

type t =
  { fin: bool
  ; rsvd: int
  ; opcode: opcode
  ; data : data
  } [@@deriving show]

let opcode_of_int = function
  | 0 -> Continuation
  | 1 -> Text
  | 2 -> Binary
  | x when x >= 3 && x <= 7 -> Reserved_non_control x
  | 8 -> Close
  | 9 -> Ping
  | 10 -> Pong
  | x -> Reserved_control x (* TODO: only true for values 0xB - 0xF *)

let int_of_opcode = function
  | Continuation -> 0
  | Text -> 1
  | Binary -> 2
  | Close -> 8
  | Ping -> 9
  | Pong -> 10
  | Reserved_non_control x
  | Reserved_control x -> x

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let to_bitstring = Bitstring.bitstring_of_string

let perform_mask key data =
    String.mapi
      (fun i c -> (int_of_char c) lxor (List.nth key (i mod 4)) |> char_of_int)
      data

let unmask (`Masked ((b0, b1, b2, b3), data)) =
  let key = List.map int_of_char [b0; b1; b2; b3] in
  let unmasked_data = perform_mask key data in
    `Plain unmasked_data

let mask (`Plain data) =
  let key = CCList.init 4 (fun _ -> Random.int 256) in
  let masked_data = perform_mask key data in
  let key = List.map char_of_int key in
  let b0 = List.nth key 0 in
  let b1 = List.nth key 1 in
  let b2 = List.nth key 2 in
  let b3 = List.nth key 3 in
  `Masked ((b0, b1, b2, b3), masked_data)

let of_string ~mask:m opcode s =
  let data = match m with
    | true -> mask (`Plain s)
    | false -> `Plain s in
    { fin = true
    ; rsvd = 0
    ; opcode = opcode
    ; data
    }

let close ~mask:m status =
  let%bitstring data = {| status : 2*8 |} in
  let s = data |> Bitstring.string_of_bitstring in
  let data = match m with
    | true -> mask (`Plain s)
    | false -> `Plain s in
    { fin = true
    ; rsvd = 0
    ; opcode = Close
    ; data
    }

module Make(Io: Io.S)(M_result : Monad_result.S with type E.t = string and type +'a u = 'a Io.M.t) = struct

  open M_result

  let parse_fin_rsvd_opcode = function%bitstring
    | {| fin : 1
       ; rsvd : 3
       ; opcode : 4 |} ->
      (fin, rsvd, opcode |> opcode_of_int) |> return
    | {| _ |} ->
      fail "Malformed_frame"

  let parse_mask_len ic = function%bitstring
    | {| mask : 1
       ; len1 : 7 |} ->
      (match len1 with
        | x when x <= 125 ->
          return (mask, Int64.of_int len1)
        | 126 ->
          Io.read_exactly ic 2 |> lift >>= flip lift_opt (fail "Malformed_frame")
          >|= (fun s ->
            let len = (s.[1] |> int_of_char) + ((s.[0] |> int_of_char) lsl 8) in
              mask, Int64.of_int len)
        | 127 ->
          Io.read_int64 ic |> lift >>= flip lift_opt (fail "Malformed_frame")
          >>= (fun len -> return (mask, len))
        | _ ->
          fail "Malformed_frame")
    | {| _ |} ->
        fail "Malformed_frame"

  let parse_plain_data len = function%bitstring
    | {| data : len lsl 3 : string |} -> `Plain data |> return

  let parse_masked_data len = function%bitstring
    | {| key : 4*8 : string; data : len lsl 3 : string |} ->
        let _list_key = [key.[0]; key.[1]; key.[2]; key.[3]] |> List.map int_of_char in
        `Masked
          ( (key.[0] , key.[1] , key.[2] , key.[3])
          , data ) |> return

  let read_frame ic =
    Io.read_exactly ic 1 |> lift >>= (flip lift_opt (fail "Malformed_frame"))
    >|= to_bitstring
    >>= parse_fin_rsvd_opcode
    >>= fun (fin, rsvd, opcode) ->
      Io.read_exactly ic 1 |> lift >>= (flip lift_opt (fail "Malformed_frame"))
    >|= to_bitstring
    >>= parse_mask_len ic
    >>= fun (mask, len) ->
      let intlen = Int64.to_int len in
      (if mask then
        Io.read_exactly ic (4+intlen) |> lift >>= (flip lift_opt (fail "Malformed_frame"))
          >|= to_bitstring
          >>= parse_masked_data intlen
      else
        Io.read_exactly ic intlen |> lift >>= (flip lift_opt (fail "Malformed_frame"))
          >|= to_bitstring
          >>= parse_plain_data intlen)
    >|= fun data ->
      { fin
      ; rsvd
      ; opcode
      ; data = data
      }

  let write_frame oc frame =
    let masked, mask_data, data_len = match frame.data with
      | `Plain s -> false, s, String.length s
      | `Masked ((b0, b1, b2, b3), s) ->
          let len = String.length s in
          let b = Buffer.create (4 + len) in
          List.iter (Buffer.add_char b) [b0; b1; b2; b3];
          true, Buffer.contents b, len in
    let f = match data_len with
      | x when x <= 125 ->
        let%bitstring f =
          {| frame.fin : 1
           ; frame.rsvd : 3
           ; frame.opcode |> int_of_opcode : 4
           ; masked : 1
           ; data_len : 7
           ; mask_data : data_len lsl 3 : string
          |} in f
      | x when x <= 65535 ->
        let%bitstring f =
          {| frame.fin : 1
           ; frame.rsvd : 3
           ; frame.opcode |> int_of_opcode : 4
           ; masked : 1
           ; 126 : 7
           ; data_len : 2*8
           ; mask_data : data_len lsl 3 : string
          |} in f
      | _ ->
          let%bitstring f = {| frame.fin : 1
         ; frame.rsvd : 3
         ; frame.opcode |> int_of_opcode : 4
         ; masked : 1
         ; 127 : 7
         ; Int64.of_int data_len : 8*8
         ; mask_data : data_len lsl 3 : string
        |} in f in
    f |> Bitstring.string_of_bitstring |> Io.write oc
end
