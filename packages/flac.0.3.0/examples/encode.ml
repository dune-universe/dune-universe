
let src = ref ""
let dst = ref ""

let buflen = ref 1024

let flush_outchan = flush

let input_string chan len =
  let ans = Bytes.create len in
    really_input chan ans 0 len ;
    Bytes.to_string ans

let input_int chan =
  let buf = input_string chan 4 in
    (int_of_char buf.[0])
    + (int_of_char buf.[1]) lsl 8
    + (int_of_char buf.[2]) lsl 16
    + (int_of_char buf.[3]) lsl 24

let input_short chan =
  let buf = input_string chan 2 in
    (int_of_char buf.[0]) + (int_of_char buf.[1]) lsl 8

let compression = ref 5
let ogg = ref false
let usage = "usage: encode [options] source destination"

let _ =
  Arg.parse
    [
      "--compression", Arg.Int (fun b -> compression := b),
      "Compression level." ;
      "--ogg", Arg.Bool (fun b -> ogg := b),
      "Encoder in ogg format." ;
    ]
    (
      let pnum = ref (-1) in
        (fun s -> incr pnum; match !pnum with
           | 0 -> src := s
           | 1 -> dst := s
           | _ -> Printf.eprintf "Error: too many arguments\n"; exit 1
        )
    ) usage;
  if !src = "" || !dst = "" then
    (
      Printf.printf "%s\n" usage;
      exit 1
    );
  let ic = open_in_bin !src in
    (* TODO: improve! *)
    if input_string ic 4 <> "RIFF" then invalid_arg "No RIFF tag";
    ignore (input_string ic 4);
    if input_string ic 4 <> "WAVE" then invalid_arg "No WAVE tag";
    if input_string ic 4 <> "fmt " then invalid_arg "No fmt tag";
    let _ = input_int ic in
    let _ = input_short ic in (* TODO: should be 1 *)
    let channels = input_short ic in
    let infreq = input_int ic in
    let _ = input_int ic in (* bytes / s *)
    let _ = input_short ic in (* block align *)
    let bits = input_short ic in
    if (bits <> 16) then
      failwith "only s16le is supported for now.." ;
    let params = 
      { Flac.Encoder.
          channels = channels ;
          sample_rate = infreq ;
          bits_per_sample = bits ;
          compression_level = Some !compression;
          total_samples = None
      }
    in
    let comments = [("TITLE","Encoding example")] in
    let encode,finish = 
      if not !ogg then
        let enc = Flac.Encoder.File.create ~comments params !dst in
        let encode buf =
            Flac.Encoder.process enc.Flac.Encoder.File.enc
                                 enc.Flac.Encoder.File.callbacks 
                                 buf
        in
        let finish () = 
          Flac.Encoder.finish enc.Flac.Encoder.File.enc
                              enc.Flac.Encoder.File.callbacks ;
          Unix.close enc.Flac.Encoder.File.fd;
        in
       encode,finish
      else
        let os = Ogg.Stream.create () in
        let oc = open_out !dst in
        let s_o_f (h,b) = h ^ b in
        let flush s =
          let rec f v =
            try
              let v = v ^ (s_o_f (Ogg.Stream.flush_page s)) in
              f v
            with
              | Ogg.Not_enough_data -> v
          in
          f ""
        in
        let pageout s =
          s_o_f (Ogg.Stream.get_page s)
        in
        let pagesout s =
          let rec f v =
            try
              let n = pageout s in
              f (v ^ n)
            with
              | Ogg.Not_enough_data -> v
          in
          f ""
        in
        let enc,p,l = Flac_ogg.Encoder.create ~comments params os in
        Ogg.Stream.put_packet os p;
        output_string oc (flush os) ;
        List.iter (Ogg.Stream.put_packet os) l;
        output_string oc (flush os) ;
        flush_outchan oc ;
        let encode buf =
            Flac.Encoder.process enc Flac_ogg.Encoder.callbacks
                                     buf;
            output_string oc (pagesout os);
        in
        let finish () = 
          Flac.Encoder.finish enc Flac_ogg.Encoder.callbacks;
          output_string oc (flush os) ;
          close_out oc
        in
        encode,finish
    in
    let start = Unix.time () in
      Printf.printf
        "Input detected: PCM WAVE %d channels, %d Hz, %d bits\n%!"
        channels infreq bits;
      Printf.printf
        "Encoding to: %s %d channels, %d Hz, compression level: %d\nPlease wait...\n%!"
        (if !ogg then "OGG/FLAC" else "FLAC") channels infreq !compression;
      while input_string ic 4 <> "data" do
        let len = input_int ic in
        really_input ic (Bytes.create len) 0 len
      done;
      (* This ensures the actual audio data will start on a new page, as per
       * spec. *)
      let buflen = channels*bits/8*(!buflen) in
      let buf = Bytes.create buflen in
        begin try while true do
          really_input ic buf 0 (Bytes.length buf);
          encode (Flac.Encoder.from_s16le (Bytes.to_string buf) channels)
        done;
        with End_of_file -> () end;
        finish ();
        close_in ic;
        Printf.printf "Finished in %.0f seconds.\n" ((Unix.time ())-.start);
        Gc.full_major () ;
        Gc.full_major ()
