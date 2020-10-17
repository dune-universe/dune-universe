let with_in f name =
  let file = open_in name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_in file)

let with_in_bin f name =
  let file = open_in_bin name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_in file)

let with_out f name =
  let file = open_out name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_out file)

let with_out_bin f name =
  let file = open_out_bin name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_out file)

let read_bytes chan =
  let size = in_channel_length chan in
  let bytes = Bytes.create size in
  let rec loop pos =
    let req_len =
      let chunk_len = 16 in
      if pos + chunk_len > size then
        size - pos
      else
        chunk_len
    in
    let actual_len = input chan bytes pos req_len in
    if actual_len = 0 then
      ()
    else
      loop (pos + actual_len)
  in
  loop 0;
  Bytes.unsafe_to_string bytes

let read_lines chan =
  let approx_size = in_channel_length chan in
  let buf = Buffer.create approx_size in
  let rec loop () =
    match
      Buffer.add_string buf (input_line chan);
      Buffer.add_char buf '\n'
    with
    | exception End_of_file -> ()
    | () -> loop ()
  in
  loop ();
  Buffer.contents buf

let read name = with_in read_lines name

let read_bin name = with_in_bin read_bytes name

let mkdir name =
  if not (Sys.file_exists name) then
    Unix.mkdir name 0o777

let fold f start dirname =
  let iterator = Unix.opendir dirname in
  Fun.protect (fun () ->
      let rec loop acc =
        match Unix.readdir iterator with
        | exception End_of_file -> acc
        | exception r -> raise r
        | name ->
           if
             name = Filename.current_dir_name
             || name = Filename.parent_dir_name
           then
             loop acc
           else
             loop (f acc name)
      in loop start
    ) ~finally:(fun () -> Unix.closedir iterator)

let iter f = fold (Fun.const f) ()

let rec remove_dir dirname =
  fold (fun () name ->
      let path = Filename.concat dirname name in
      if Sys.is_directory path then
        remove_dir path
      else
        Unix.unlink path
    ) () dirname;
  Unix.rmdir dirname

let remove name =
  if Sys.is_directory name then
    remove_dir name
  else
    Unix.unlink name
