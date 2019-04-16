open Bigarray

let deflate_inflate ?dict ?header ?(window_bits=15) buf =
  let open Zlib in
  let inflate = create_inflate ~window_bits () in
  let deflate =
    create_deflate
      ~level:(-1)
      ~algo:Deflated
      ~window_bits
      ~memory:8
      ~strategy:Default_strategy
      ()
  in
  begin match header with
    | None -> ()
    | Some header -> set_header deflate.state header
  end;
  begin match dict with
    | None -> ()
    | Some (adler,dict) ->
      let adler' = deflate_set_dictionary deflate.state dict in
      if window_bits > 0 then assert (adler = adler');
      if window_bits < 0 then
        assert (inflate_set_dictionary inflate.state dict == Ok);
  end;
  let bound = deflate_bound deflate.state (Array1.dim buf) in
  deflate.in_buf <- buf;
  deflate.out_buf <- Array1.create char c_layout (bound + 768);
  begin match flate deflate Finish with
    | Stream_end -> ()
    | Ok -> failwith "Ok"
    | Need_dict -> failwith "Need_dict"
    | Buf_error -> failwith "Buf_error"
    | Data_error s -> failwith ("Data_error: " ^ s)
  end;
  Printf.eprintf "Deflated %i bytes to %i bytes. Bound was %i. Kind was %s.%!"
    deflate.in_total deflate.out_total bound
    begin match get_data_type deflate with
        Binary -> "binary" |Text -> "text" |Unknown -> "unknown"
    end;
  inflate.in_buf <- deflate.out_buf;
  inflate.out_buf <- Array1.create char c_layout (Array1.dim buf);
  inflate.in_len <- 10;
  inflate.out_len <- 10;
  let rec loop () =
    match flate inflate No_flush with
    | Ok -> 
      if inflate.out_len = 0
      then begin
        inflate.out_len <- Array1.dim inflate.out_buf - inflate.out_ofs;
        if inflate.out_len > 10 then inflate.out_len <- 10;
      end;
      if inflate.in_len = 0
      then begin
        inflate.in_len <- deflate.out_total - inflate.in_ofs;
        if inflate.in_len > 10 then inflate.in_len <- 10;
      end;
      loop ();
    | Stream_end -> ()
    | Need_dict ->
      begin
        match dict with
        | None -> failwith "dictionary required, but no dictionary available."
        | Some (adler,dict) ->
          assert (adler = inflate.cksum);
          match inflate_set_dictionary inflate.state dict with
          | Ok -> loop ()
          | Data_error s -> failwith ("dictionary checksum mismatch: " ^ s)
          | _ -> failwith "inflate_set_dictionary unknown error"
      end
    | Buf_error -> failwith "Buf_error"
    | Data_error s -> failwith ("Data_error: " ^ s)
  in
  loop ();
  Printf.eprintf " Inflated %i bytes to %i bytes.%!"
    inflate.in_total inflate.out_total;
  assert (deflate.in_buf = inflate.out_buf);
  prerr_newline ();
  begin match header with
    | None -> ()
    | Some header ->
      let header' = get_header inflate.state in
      assert begin
        header' = header ||
        header'.name <> header.name &&
        let header'' = { header' with name = header.name } in
        header'' = header
      end;
  end;
;;

let () =
  let filename = "../src/zlib_stubs.c" in
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let dict = "LongstatMEMORYfinalizesystemdataIsruntimearrayreturnValfieldMAXStorememLevelinflatestreamcopysizeofadlervdicttotalfailwithallocvaluevstrmintretflushvaluevwrapZdeflatebawindowBitsbreakCamlmemoryERRORvwrapzlibintCAMLprimvaluezlibifDatacustomflagsINFLATEstreampzstrmstrmcaseZstructwrapZLIBerrorassertIntinret#includeoutzValavailLFieldlongcamlFieldwrapvalvstrmzstrm" in
  let dict = Zlib.adler32 Zlib.adler32_empty dict, dict in
  let header =
    { Zlib.
      text    = true
    ; xflags  = 0
    ; mtime   = Int32.of_float (Unix.time ())
    ; os      = 3
    ; extra   = Some "This is an extra string"
    ; name    = Some ("This is the filename" ^ String.make 576 '#')
    ; comment = Some "This is the comment"
    } in
  let text =
    array1_of_genarray @@
    Unix.map_file fd char c_layout false [|~-1|]
  in
  let binary = Array1.create char c_layout 500_000 in
  let ascii = Array1.create char c_layout 500_000 in
  Random.self_init ();
  for i = 0 to Array1.dim binary - 1 do
    Array1.set binary i (char_of_int (Random.int 256))
  done;
  for i = 0 to Array1.dim ascii - 1 do
    Array1.set ascii i (char_of_int (Random.int 93 + 33))
  done;
  let test ?dict ?header ?window_bits () =
    prerr_string "  Random binary: ";
    deflate_inflate ?dict ?header ?window_bits binary;
    prerr_string "  Random ascii: ";
    deflate_inflate ?dict ?header ?window_bits ascii;
    Gc.full_major ();
    prerr_string "  C code: ";
    deflate_inflate ?dict ?header ?window_bits text;
  in
  prerr_endline "zlib"; test ();
  prerr_endline "zlib with dict"; test ~dict ();
  prerr_endline "raw deflate"; test ~window_bits:(-15) ();
  prerr_endline "raw deflate with dict"; test ~window_bits:(-15) ~dict ();
  prerr_endline "gzip"; test ~window_bits:(16+15) ();
  prerr_endline "gzip with header"; test ~window_bits:(16+15) ~header ();
