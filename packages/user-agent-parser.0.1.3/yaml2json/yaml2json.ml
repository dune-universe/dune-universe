let rec yaml2json : Yaml.value -> Yojson.Safe.t = function
  | `A values -> `List (List.map yaml2json values)
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Null -> `Null
  | `O maps -> `Assoc (List.map (fun (key, value) -> (key, yaml2json value)) maps)
  | `String s -> `String s

let process in_chan =
  let str = Stdio.In_channel.input_all in_chan in
  match Yaml.of_string str with
    | Ok yaml ->
      let json = yaml2json yaml in
      Yojson.Safe.to_channel stdout json;
      Ok ()
    | Error _ ->
      Error ()

let () =
  match Array.length Sys.argv with
  | 2 ->
    begin match Stdio.In_channel.with_file (Array.get Sys.argv 1) ~f:process with
      | Ok () -> ()
      | Error () -> exit 2
    end
  | _ -> exit 1
