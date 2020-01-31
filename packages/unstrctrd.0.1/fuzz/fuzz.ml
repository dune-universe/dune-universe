let () =
  let buffer = Bytes.create 0x1000 in
  Crowbar.add_test ~name:"unstrctrd" Crowbar.[ bytes ] @@ fun input ->
  let parser = Unstrctrd_parser.unstrctrd buffer in
  match Angstrom.parse_string parser input with
  | Ok v ->
    let result = Unstrctrd.to_utf_8_string v in
    let part = String.sub input 0 (String.length result) in
    Crowbar.check_eq ~pp:Fmt.string ~eq:String.equal ~cmp:String.compare part result
  | Error _ -> Crowbar.bad_test ()

let buffer = Bigstringaf.create 0x1000

let rest_of_input ~committed ~len buffer input =
  let head = Bigstringaf.substring buffer ~off:committed ~len:(len - committed) in
  String.concat "" (head :: input)

let run parser input =
  let open Angstrom.Unbuffered in
  let rec go len input = function
    | Done (committed, v) ->
      let rest = rest_of_input ~committed ~len buffer input in
      Ok (v, rest)
    | Fail (_, _, err) -> Error (`Msg err)
    | Partial { committed; continue; } ->
      let len = len - committed in
      Bigstringaf.blit buffer ~src_off:committed buffer ~dst_off:0 ~len ;
      match input with
      | [] -> go len [] (continue buffer ~off:0 ~len Complete)
      | x :: r ->
        let max = min (String.length x) (Bigstringaf.length buffer - len) in
        Bigstringaf.blit_from_string x ~src_off:0 buffer ~dst_off:len ~len:max ;
        let r = if max = String.length x then r else String.sub x max (String.length x - max) :: r in
        go (len + max) r (continue buffer ~off:0 ~len:(len + max) Incomplete) in
  go 0 input (parse parser)

let () =
  let buffer0 = Bytes.create 0x1000 in
  let buffer1 = Bytes.create 0x1000 in

  Crowbar.add_test ~name:"unstrctrd" Crowbar.[ list bytes ] @@ fun input ->
  let parser0 = Unstrctrd_parser.fast_unstrctrd buffer0 in
  let parser1 = Unstrctrd_parser.unstrctrd buffer1 in

  let res0 = let open Rresult in run parser0 input >>= fun (v, rest) -> R.ok (Unstrctrd.to_utf_8_string v, rest) in
  let res1 = let open Rresult in run parser1 input >>= fun (v, rest) -> R.ok (Unstrctrd.to_utf_8_string v, rest) in

  Crowbar.check_eq res0 res1
