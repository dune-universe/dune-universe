let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.')
    Fmt.char

let pp_scalar
  : type buffer. get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t
  = fun ~get ~length ppf b ->
  let l = length b in

  for i = 0 to l / 16
  do Fmt.pf ppf "%08x: " (i * 16);
    let j = ref 0 in

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  ";

      if !j mod 2 <> 0 then Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "  ";
    j := 0;

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length

exception Bigger_than_80_column of string
exception Malformed of string

let pp_value ppf = function
  | `Line_break -> Fmt.string ppf "`Line_break"
  | `Char chr -> Fmt.pf ppf "(`Char @[%02X@])" (Char.code chr)
  | `End -> Fmt.string ppf "`End"

let iso input =
  let encoder = Pecu.encoder `Manual in
  let decoder = Pecu.decoder `Manual in
  let tmp = Bytes.create 0x8000 in
  let res = Bytes.create (String.length input) in
  let buf = Buffer.create (String.length input * 2) in

  let rec decode j =
    if Pecu.decoder_dangerous decoder
    then raise (Bigger_than_80_column (Buffer.contents buf))

  ; match Pecu.decode decoder with
  | `Await -> j
  | `Data data ->
    Bytes.blit_string data 0 res j (String.length data)
  ; decode (j + String.length data)
  | `Line line ->
    Bytes.blit_string line 0 res j (String.length line)
  ; Bytes.set res (j + String.length line) '\n'
  ; decode (j + String.length line + 1)
  | `End -> j
  | `Malformed s -> raise (Malformed s) in

  let rec encode i j =
    let v =
      if String.length input = i
      then `End
      else match String.get input i with
        | '\n' -> `Line_break
        | chr -> `Char chr in

    match Pecu.encode encoder v with
    | `Ok -> encode (succ i) j
    | `Partial ->
      Buffer.add_subbytes buf tmp 0 (0x8000 - (Pecu.dst_rem encoder))
    ; Pecu.src decoder tmp 0 (0x8000 - (Pecu.dst_rem encoder))
    ; Pecu.dst encoder tmp 0 0x8000
    ; match v with
      | `End ->
        let j = decode j in
        Pecu.src decoder tmp 0 0
      ; let j = decode j in
        Bytes.sub_string res 0 j, Buffer.contents buf
      | _ -> encode i (decode j) in

    Pecu.dst encoder tmp 0 0x8000
  ; encode 0 0

let ensure str =
  Astring.String.cuts ~sep:"\r\n" str
  |> List.iter (fun line -> if String.length line > 76 then raise (Bigger_than_80_column line))

let () =
  Crowbar.add_test ~name:"iso" Crowbar.[ bytes ] @@ fun u ->
  try
      let v, o = iso u in
      ensure o
    ; Crowbar.check_eq ~pp:pp_string u v
  with
  | Bigger_than_80_column o ->
    Fmt.kstrf Crowbar.fail "Encoded value does not fit on 80 columns: @[%a@]" pp_string o
  | Malformed err ->
    Fmt.kstrf Crowbar.fail "Malformed encoding value: @[%a@]" pp_string err
