let read_from_file file_path = 
  Lwt_io.with_file ~mode: Input file_path (fun channel -> Lwt_io.read channel)

let append_to_file file_path serialized_record =
  Lwt_io.with_file 
    ~flags:([Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_APPEND; Unix.O_CREAT]) 
    ~mode: Output 
    file_path 
    (fun channel -> Lwt_io.write_line channel serialized_record)