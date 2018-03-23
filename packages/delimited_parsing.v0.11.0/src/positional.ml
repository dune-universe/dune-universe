open! Core
open! Async
open! Shared

type header = (string * int * int) list

let process_header header ~strict =
  let header = List.sort header ~compare:(fun (_,a,_) (_,b,_) -> Int.compare a b) in
  let header_index = String.Table.create () in
  let col2str (name, pos, len) =
    sprintf "(name: %s, start:%i, length:%i)" name pos len
  in
  let rec loop i l =
    match l with
    | [] -> Ok (header,header_index)
    | [(name, _, _)] ->
      begin match Hashtbl.add header_index ~key:name ~data:i with
      | `Ok        -> Ok (header,header_index)
      | `Duplicate -> Or_error.error_string ("Duplicate column name: "^name)
      end
    | (name1, pos1, len1) :: (name2, pos2, len2) :: l ->
      if pos1 + len1 > pos2 then
        Or_error.error_string ("Overlapping columns :"
                               ^ (col2str (name1, pos1, len1))
                               ^ (col2str (name2, pos2, len2)))
      else if strict && pos1 + len1 <> pos2 then
        Or_error.error_string ("Gap between columns :"
                               ^ (col2str (name1, pos1, len1))
                               ^ (col2str (name2, pos2, len2)))
      else
        match Hashtbl.add header_index ~key:name1 ~data:i with
        | `Ok        -> loop (i+1) ((name2, pos2, len2) :: l)
        | `Duplicate -> Or_error.error_string ("Duplicate column name: "^name1)
  in
  loop 0 header
;;

let of_reader
      ?(strip=false)
      ?(skip_lines=0)
      ?(on_parse_error=`Raise)
      ~header
      ?(strict=true)
      reader =
  match process_header header ~strict with
  | Error e -> Error e
  | Ok (header, header_index) ->
    let pipe_r,pipe_w = Pipe.create () in
    let n_cols = List.length header in
    let parse_line line =
      let data = Array.create ~len:n_cols "" in
      try
        List.iter header ~f:(fun (name, pos,len) ->
          let i = Hashtbl.find_exn header_index name in
          if strip then
            data.(i) <- String.strip (String.sub line ~pos ~len)
          else
            data.(i) <- String.sub line ~pos ~len
        );
        Ok (Row.create header_index data)
      with
      | e -> Error e
    in
    let close () =
      don't_wait_for (Reader.close reader);
      Pipe.close pipe_w;
    in
    let rec loop () =
      Reader.read_line reader
      >>> function
      | `Eof -> close ()
      | `Ok line ->
        match parse_line line with
        | Ok row -> Pipe.write pipe_w row >>> loop
        | Error e ->
          match on_parse_error with
          | `Raise -> close (); raise e
          | `Handle f ->
            match f (Queue.create ()) e with
            | `Continue -> loop ()
            | `Finish -> close ()
    in
    upon (drop_lines reader skip_lines) loop;
    Ok pipe_r
;;

let create_reader ?strip ?skip_lines ?on_parse_error ~header ?strict filename =
  Reader.open_file filename >>| fun r ->
  of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r
;;

let rec write_line w header next_pos line =
  match header, line with
  | [], [] -> Writer.write w "\r\n"
  | (_,pos,len)::header, field::line ->
    let pre_fill =
      if next_pos < pos then
        String.init (pos - next_pos) ~f:(const ' ')
      else
        ""
    in
    let fill =
      if String.length field < len then
        String.init (len - String.length field) ~f:(const ' ')
      else
        ""
    in
    Writer.write w (pre_fill ^ field ^ fill);
    write_line w header (pos + len) line
  | [], _ -> raise (Invalid_argument "Too many fields given")
  | _, [] -> raise (Invalid_argument "Too few fields given")
;;

let of_writer writer ?(strict=true) header =
  match process_header header ~strict with
  | Error e -> Error e
  | Ok (header, _) ->
    let pipe_r, pipe_w = Pipe.create () in
    don't_wait_for (Pipe.iter_without_pushback pipe_r ~f:(write_line writer header 0));
    upon (Pipe.closed pipe_w) (fun () -> don't_wait_for (Writer.close writer));
    Ok pipe_w
;;

let create_writer filename ?strict header =
  Writer.open_file filename
  >>| fun w ->
  of_writer w header ?strict
;;
