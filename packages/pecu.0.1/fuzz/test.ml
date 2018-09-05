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

let load_file file =
  let ic = open_in file in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
    really_input ic rs 0 ln
  ; close_in ic
  ; Bytes.unsafe_to_string rs

let load_std () =
  let buf = Buffer.create 0x8000 in

  let rec go () = match input_line stdin with
    | line ->
      Buffer.add_string buf line
    ; go ()
    | exception End_of_file -> Buffer.contents buf in
  go ()

let test input =
  let u = match input with
    | `Std -> load_std ()
    | `File file -> load_file file in

  try
    let v, o = iso u in
    let () = ensure o in

    if String.equal u v
    then Fmt.pr "PASS\n%!"
    else Fmt.epr "@[%a@]@ <>@ @[%a@]"
        pp_string u pp_string v
  with
  | Bigger_than_80_column o ->
    Fmt.invalid_arg "Encoded value does not fit on 80 columns: @[%a@]" pp_string o
  | Malformed err ->
    Fmt.invalid_arg "Malformed encoding value: @[%a@]" pp_string err


open Cmdliner

type flow = [ `Std | `File of string ]

let input_flow =
  let parser s =
    if Sys.file_exists s
    then Ok (`File s) else Rresult.R.(error (msgf "%s not found" s)) in
  let pp ppf = function
    | `File file -> Fmt.pf ppf "<%s>" file
    | `Std -> Fmt.string ppf "#stdin" in
  Arg.conv (parser, pp)

let input =
  let doc = "Input file." in
  Arg.(value & opt input_flow `Std & info ["i"; "input"] ~doc ~docv:"<file>")

let cmd =
  let doc = "Pecu test tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Tool to test output from fuzzer." ] in
  Term.(const test $ input),
  Term.info "test" ~version:"0.1" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)
