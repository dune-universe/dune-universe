let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_raw = pp_scalar ~get:String.get ~length:String.length

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
  let rec decode dpos epos =
    if Pecu.decoder_dangerous decoder then
      raise (Bigger_than_80_column (Buffer.contents buf)) ;
    match Pecu.decode decoder with
    | `Await -> encode dpos epos (Pecu.encode encoder `Await)
    | `Data data ->
        Bytes.blit_string data 0 res dpos (String.length data) ;
        decode (dpos + String.length data) epos
    | `Line line ->
        Bytes.blit_string line 0 res dpos (String.length line) ;
        Bytes.set res (dpos + String.length line) '\n' ;
        decode (dpos + String.length line + 1) epos
    | `End -> (Bytes.sub_string res 0 dpos, Buffer.contents buf)
    | `Malformed s -> raise (Malformed s)
  and encode dpos epos = function
    | `Ok ->
      if epos > String.length input
      then
        ( Pecu.src decoder tmp 0 0
        ; decode dpos epos )
      else
        let cmd =
          if epos >= String.length input then `End
          else if input.[epos] = '\n' then `Line_break
          else `Char input.[epos] in
        encode dpos (succ epos) (Pecu.encode encoder cmd)
    | `Partial ->
        Buffer.add_subbytes buf tmp 0 (0x8000 - Pecu.dst_rem encoder) ;
        Pecu.src decoder tmp 0 (0x8000 - Pecu.dst_rem encoder) ;
        Pecu.dst encoder tmp 0 0x8000 ;
        decode dpos epos in
  Pecu.dst encoder tmp 0 0x8000 ;
  let cmd = if String.length input > 0 then `Char input.[0] else `End in
  encode 0 1 (Pecu.encode encoder cmd)

let ensure str =
  Astring.String.cuts ~sep:"\r\n" str
  |> List.iter (fun line ->
         if String.length line > 76 then raise (Bigger_than_80_column line) )

let load_file file =
  let ic = open_in file in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ; close_in ic ; Bytes.unsafe_to_string rs

let raw = Alcotest.testable pp_raw String.equal

let walk directory =
  let rec go acc = function
    | [] -> acc
    | dir :: rest ->
        let contents = Array.to_list (Sys.readdir dir) in
        let contents = List.rev_map (Filename.concat dir) contents in
        let dirs, files =
          List.fold_left
            (fun (dirs, files) kind ->
              match (Unix.stat kind).Unix.st_kind with
              | Unix.S_REG -> (dirs, kind :: files)
              | Unix.S_DIR -> (kind :: dirs, files)
              | Unix.S_BLK | Unix.S_CHR | Unix.S_FIFO | Unix.S_LNK
               |Unix.S_SOCK ->
                  (dirs, files)
              | exception Unix.Unix_error _ -> (dirs, files) )
            ([], []) contents
        in
        go (files @ acc) (dirs @ rest)
  in
  go [] [directory]

let test file =
  Alcotest.test_case file `Quick
  @@ fun () ->
  let u = load_file file in
  let v, o = iso u in
  let () = ensure o in
  Alcotest.(check raw) "iso" u v

let rfc2047 =
  let make i (value, expect) =
    Alcotest.test_case (Fmt.strf "rfc2047:%d" i) `Quick
    @@ fun () ->
    let decoder = Pecu.Inline.decoder (`String value) in
    let buffer = Buffer.create (String.length value) in
    let rec go () =
      match Pecu.Inline.decode decoder with
      | `Await -> assert false
      | `Char chr -> Buffer.add_char buffer chr ; go ()
      | `Malformed s -> raise (Malformed s)
      | `End -> ()
    in
    go () ;
    let result0 = Buffer.contents buffer in
    Buffer.clear buffer ;
    let encoder = Pecu.Inline.encoder (`Buffer buffer) in
    String.iter (fun chr -> match Pecu.Inline.encode encoder (`Char chr) with
        | `Ok -> ()
        | `Partial -> assert false) result0 ;
    let () = ignore @@ Pecu.Inline.encode encoder `End in
    let result1 = Buffer.contents buffer in
    Alcotest.(check string)
      (Fmt.strf "compare %s with %s" result0 expect)
      result0 expect ;
    Alcotest.(check string)
      (Fmt.strf "compare %s with %s" result1 value)
      result1 value ;
  in
  List.mapi make
    [ ("a", "a")
    ; ("a_b", "a b")
    ; ("Keith_Moore", "Keith Moore")
    ; ("Keld_J=F8rn_Simonsen", "Keld J\xf8rn Simonsen")
    ; ("Andr=E9", "Andr\xe9")
    ; ("Olle_J=E4rnefors", "Olle J\xe4rnefors")
    ; ("Patrick_F=E4ltstr=F6m", "Patrick F\xe4ltstr\xf6m") ]

let tests () =
  Alcotest.run "pecu"
    [("input fuzz", List.map test (walk "contents")); ("rfc2047", rfc2047)]

let () = tests ()
