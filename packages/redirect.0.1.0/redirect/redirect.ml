let read_and_close channel f =
  match
    try Ok (f ())
    with exn -> Error exn
  with
  | Ok result ->
      close_in channel;
      result
  | Error exn ->
      let bt = Printexc.get_raw_backtrace () in
      close_in_noerr channel;
      Printexc.raise_with_backtrace exn bt

let write_and_close channel f =
  match
    try Ok (f ())
    with exn -> Error exn
  with
  | Ok result ->
      close_out channel;
      result
  | Error exn ->
      let bt = Printexc.get_raw_backtrace () in
      close_out_noerr channel;
      Printexc.raise_with_backtrace exn bt

let rec add_channel_to_the_end  ?(chunk_size = 1024) buffer channel =
  match Buffer.add_channel buffer channel chunk_size with
  | () -> add_channel_to_the_end ~chunk_size buffer channel
  | exception End_of_file -> ()

let output_channel_to_the_end ?(chunk_size = 1024) out_channel in_channel =
  let buffer = Bytes.create chunk_size in
  let rec loop () =
    match input in_channel buffer 0 chunk_size with
    | 0 -> ()
    | len ->
        output out_channel buffer 0 len;
        loop () in
  loop ()

let with_temp_file ?(prefix = "temp") ?(suffix = "tmp") contents f =
  let (file, channel) = Filename.open_temp_file prefix suffix in
  Fun.protect begin fun () ->
    write_and_close channel begin fun () ->
      output_string channel contents
    end;
    let channel = open_in file in
    read_and_close channel begin fun () ->
      f file channel
    end
  end
  ~finally:(fun () -> Sys.remove file)

let with_pipe f =
  let (read, write) = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr read
  and out_channel = Unix.out_channel_of_descr write in
  Fun.protect begin fun () ->
    f in_channel out_channel
  end
  ~finally:begin fun () ->
    close_in_noerr in_channel;
    close_out_noerr out_channel
  end

let with_stdin_from channel f =
  let stdin_backup = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel channel) Unix.stdin;
  Fun.protect f
  ~finally:begin fun () ->
    Unix.dup2 stdin_backup Unix.stdin
  end

let with_stdout_to channel f =
  let stdout_backup = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.descr_of_out_channel channel) Unix.stdout;
  Fun.protect f
  ~finally:begin fun () ->
    Unix.dup2 stdout_backup Unix.stdout
  end

let with_channel_from_string s f =
  with_pipe @@ fun in_channel out_channel ->
    output_string out_channel s;
    close_out out_channel;
    f in_channel

let with_channel_to_buffer ?chunk_size buffer f =
  with_pipe @@ fun in_channel out_channel ->
    let _thread = () |> Thread.create @@ fun () ->
      add_channel_to_the_end ?chunk_size buffer in_channel in
    f out_channel

let with_channel_to_string ?(initial_size = 1024) ?chunk_size f =
  let buffer = Buffer.create initial_size in
  let result = with_channel_to_buffer ?chunk_size buffer f in
  Buffer.contents buffer, result

let with_stdin_from_string s f =
  with_channel_from_string s @@ fun channel -> with_stdin_from channel f

let with_stdout_to_buffer ?chunk_size buffer f =
  with_channel_to_buffer ?chunk_size buffer @@ fun channel ->
    with_stdout_to channel f

let with_stdout_to_string ?initial_size ?chunk_size f =
  with_channel_to_string ?initial_size ?chunk_size @@ fun channel ->
    with_stdout_to channel f
