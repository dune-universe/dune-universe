external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = random_seed ()

let () = Fmt.pr "seed: %a.\n%!" Fmt.(Dump.array int) seed
let () = Printexc.record_backtrace true
let () = Random.full_init seed

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

let drain buf chan =
  try
    while true do
      Buffer.add_channel buf chan 1
    done
  with End_of_file -> ()

let conv7 ~f ~t s () =
  let rs = Buffer.create 128 in
  let er = Buffer.create 128 in
  let f = match f with `UTF_7 -> "-f7" | `UTF_8 -> "-f8" in
  let t = match t with `UTF_7 -> "-t7" | `UTF_8 -> "-t8" in
  let ic, oc, ec =
    Unix.open_process_full (Fmt.strf "./conv7 %s %s" f t) [||]
  in
  output_string oc s ;
  close_out oc ;
  drain rs ic ;
  drain er ec ;
  if Buffer.length er > 0 then
    Fmt.epr "Got on stderr: @[%a@].\n%!" pp_raw (Buffer.contents er) ;
  let () =
    match Unix.close_process_full (ic, oc, ec) with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED r ->
        Fmt.epr "%a: conv7 terminated with exit code (%d).\n%!"
          Fmt.(styled `Red string)
          "[ERROR]" r
    | Unix.WSIGNALED n ->
        Fmt.epr "%a: conv7 was killed by a signal (%d).\n%!"
          Fmt.(styled `Red string)
          "[ERROR]" n
    | Unix.WSTOPPED n ->
        Fmt.epr "%a: conv7 was stopped by a signal (%d).\n%!"
          Fmt.(styled `Red string)
          "[ERROR]" n
  in
  let x = Buffer.contents rs in
  Buffer.clear rs ; Buffer.clear er ; x

let generator s () = conv7 ~f:`UTF_8 ~t:`UTF_7 s ()

let oracle s () = conv7 ~f:`UTF_7 ~t:`UTF_8 s ()

exception Invalid_UTF_8

let generate length =
  let rs = Buffer.create length in
  let rec go rest =
    if rest = 0 then (
      let x = Buffer.contents rs in
      Buffer.clear rs ; x )
    else
      let n =
        Int32.to_int (Random.int32 Int32.max_int) land 0xFFFFFFF mod 0x10FFFF
      in
      try
        let uchar = Uchar.of_int n in
        Uutf.Buffer.add_utf_8 rs uchar ;
        go (pred rest)
      with Invalid_argument _ -> go rest
  in
  go length

let utf8 = Alcotest.testable pp_raw String.equal

let test expect s () =
  let buf = Buffer.create 128 in
  let decoder = Yuscii.decoder (`String s) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buf) in
  let rec go () =
    match Yuscii.decode decoder with
    | `Await -> assert false
    | `End ->
        let[@warning "-8"] (`Ok : [`Ok | `Partial]) =
          Uutf.encode encoder `End
        in
        let x = Buffer.contents buf in
        Buffer.clear buf ; x
    | `Malformed _ as m ->
        Fmt.invalid_arg "Got an malformed input: %a." Yuscii.pp_decode m
    | `Uchar _ as v -> (
      match Uutf.encode encoder v with
      | `Ok -> go ()
      | `Partial -> assert false )
  in
  let result = go () in
  Alcotest.(check utf8) expect expect result

let tests =
  [("1 + 2 = 3;", "1 +- 2 +AD0 3;"); ("~~+", "+AH4AfgAr-"); ("~-", "+AH4--")]
  |> List.map (fun (expect, s) -> (expect, `Quick, test expect s))

let random ~length n =
  let rec go acc = function
    | 0 -> acc
    | n ->
        let u8 = generate length in
        let u7 = generator u8 () in
        let test = (u7, `Quick, test u8 u7) in
        (go [@tailcall]) (test :: acc) (n - 1)
  in
  go [] n

let () =
  Alcotest.run "yuscii"
    [ ("sample", tests)
    ; ("random (5)", random ~length:5 5)
    ; ("random (20)", random ~length:20 1) ]
