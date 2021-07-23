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

(* If the directory already exists, does nothing. *)
let touch_dir name =
  if not (Sys.file_exists name) then
    Sys.mkdir name 0o777

let split_re = Re.compile (Re.str Filename.dir_sep)

(* Creates all directories in the current path. *)
let create_dirs path =
  let split = Re.split split_re path in
  ignore (List.fold_left (fun path name ->
      let path = Filename.concat path name in
      touch_dir path;
      path
    ) "" split)

let rec remove_dir dirname =
  Sys.readdir dirname |> Array.iter begin fun name ->
    remove (Filename.concat dirname name)
  end;
  Sys.rmdir dirname

and remove name =
  if Sys.is_directory name then
    remove_dir name
  else
    Sys.remove name
