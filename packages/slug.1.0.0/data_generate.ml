let ml_path = "slug_data.ml"

let charmap_path = "data/charmap.json"

let locales_path = "data/locales.json"

let write_charmap ~oc ~variable ~json =
  let buf = Buffer.create 1000 in
  Printf.bprintf buf "let %s = [\n" variable;
  let () =
    match json with
    | `Assoc pairs ->
        List.iter
          (fun (key, value) ->
            if key = "locale" then ()
            else
              let value =
                match value with
                | `String value -> value
                | _ -> failwith "Incorrect pair value. Expect string." in
              Printf.bprintf buf "  ({|%s|}, {|%s|});\n" key value)
          pairs
    | _ -> failwith "Incorrect json structure. Expect object." in
  Buffer.add_string buf "]";
  Printf.fprintf oc "%s\n" (Buffer.contents buf)

let () =
  let oc = open_out ml_path in
  Printf.fprintf oc "%s\n" "(* This file is auto generated *)";
  write_charmap ~oc ~variable:"base" ~json:(Yojson.Basic.from_file charmap_path);

  let locales_map = Yojson.Basic.from_file locales_path in
  let () =
    match locales_map with
    | `Assoc pairs ->
        List.iter
          (fun (locale, charmap) ->
            write_charmap ~oc ~variable:locale ~json:charmap)
          pairs
    | _ -> failwith "Incorrect json structure. Expect object." in
  close_out oc
