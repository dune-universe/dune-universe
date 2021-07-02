let inflate_string_de str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.make_window ~bits:15 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  match De.Higher.uncompress ~w ~refill ~flush i o with
  | Ok () ->
    Ok (Buffer.contents r)
  | Error _ as err ->
    err

let deflate_string_de str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    (* assert (buf == i); *)
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    (* assert (buf == o); *)
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  De.Higher.compress ~w ~q ~refill ~flush i o;
  Buffer.contents r

let inflate_string_gz str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ ->
    Ok (Buffer.contents r)
  | Error _ as err ->
    err

let time () = Int32.of_float (Unix.gettimeofday ())

let deflate_string_gz ?(level = 4) str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let cfg = Gz.Higher.configuration Gz.Unix time in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o;
  Buffer.contents r

let inflate_string ~algorithm str =
  match algorithm with
  | `Deflate ->
    inflate_string_de str
  | `Gzip ->
    inflate_string_gz str

let deflate_string ~algorithm str =
  match algorithm with
  | `Deflate ->
    deflate_string_de str
  | `Gzip ->
    deflate_string_gz str

let encoding_of_string = function
  | "deflate" ->
    `Deflate
  | "gzip" ->
    `Gzip
  | s ->
    `Unknown s

let content_encodings request =
  match Dream.header "content-encoding" request with
  | None ->
    None
  | Some s ->
    String.split_on_char ',' s
    |> List.map (fun x -> x |> String.trim |> String.lowercase_ascii)
    |> List.map encoding_of_string
    |> Option.some

let accepted_encodings_with_weights request =
  match Dream.header "accept-encoding" request with
  | None ->
    None
  | Some s ->
    let encodings = Accept.encodings (Some s) |> Accept.qsort in
    Some
      (List.map
         (fun (a, b) ->
           ( (match b with
             | Accept.Any ->
               `Any
             | Accept.Gzip ->
               `Gzip
             | Accept.Compress ->
               `Compress
             | Accept.Deflate ->
               `Deflate
             | Accept.Identity ->
               `Identity
             | Accept.Encoding s ->
               `Unknown s)
           , a ))
         encodings)

let accepted_encodings request =
  match accepted_encodings_with_weights request with
  | None ->
    None
  | Some encodings ->
    Some (List.map (fun (a, _) -> a) encodings)

let preferred_content_encoding request =
  match accepted_encodings request with
  | None ->
    None
  | Some l ->
    let rec aux = function
      | [] ->
        None
      | `Any :: _rest ->
        Some `Gzip
      | `Deflate :: _rest ->
        Some `Deflate
      | `Gzip :: _rest ->
        Some `Gzip
      | _ :: rest ->
        aux rest
    in
    aux l

let algorithm_to_string = function `Deflate -> "deflate" | `Gzip -> "gzip"

let with_encoded_body ?(algorithm = `Deflate) body response =
  match body with
  | "" ->
    response
  | _ ->
    let encoded_body = deflate_string ~algorithm body in
    response
    |> Dream.with_body encoded_body
    |> Dream.with_header "Content-Encoding" (algorithm_to_string algorithm)

let compress handler req =
  let%lwt response = handler req in
  let preferred_algorithm = preferred_content_encoding req in
  match preferred_algorithm with
  | None ->
    Lwt.return response
  | Some algorithm ->
    Dream.log
      "Compressing request with algorithm: %s"
      (algorithm_to_string algorithm);
    let%lwt body = Dream.body response in
    Lwt.return @@ with_encoded_body ~algorithm body response

let request_with_body body request =
  let client = Dream.client request in
  let method_ = Dream.method_ request in
  let target = Dream.target request in
  let version = Dream.version request in
  let headers = Dream.all_headers request in
  Dream.request ~client ~method_ ~target ~version ~headers body

let decompress handler req =
  let rec aux algorithms content =
    match algorithms with
    | [] ->
      Ok content
    | (`Deflate as el) :: rest | (`Gzip as el) :: rest ->
      Result.bind (inflate_string ~algorithm:el content) (aux rest)
    | _ :: _rest ->
      Error (`Msg "Unsopported encoding directive")
  in
  let algorithms = content_encodings req in
  match algorithms with
  | None ->
    handler req
  | Some algorithms ->
    let%lwt body = Dream.body req in
    let body = aux algorithms body in
    (match body with
    | Ok body ->
      let req = request_with_body body req in
      handler req
    | Error (`Msg err) ->
      Dream.respond ~status:`Unsupported_Media_Type err)
