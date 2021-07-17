let read_file filename =
  let chan = open_in filename in
  Fun.protect (fun () ->
      let buf = Buffer.create 256 in
      let rec loop () =
        match input_line chan with
        | exception End_of_file -> Buffer.contents buf
        | line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
      in loop())
    ~finally:(fun () -> close_in chan)

let read_plist filename =
  let chan = open_in filename in
  let plist =
    Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_plist_exn plist

let read_yojson_basic filename =
  TmLanguage.of_yojson_exn (Yojson.Basic.from_file filename)

let read_ezjsonm filename =
  let chan = open_in filename in
  let json =
    Fun.protect (fun () -> Ezjsonm.from_channel chan)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_ezjsonm_exn json
