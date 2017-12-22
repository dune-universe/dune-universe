let test_one = () => {
  let raw_data = {|
<html>
  <div>
    Hello World World World World
  </div>
</html>
|};
  Printf.sprintf(
    "Encoder version %s, Decoder version %s",
    Brotli.Compress.version,
    Brotli.Decompress.version
  )
  |> print_endline;
  let compressed =
    Brotli.Compress.bytes(
      ~on_part_compressed=
        (piece) => Printf.sprintf("Compressed piece %d", Nativeint.to_int(piece)) |> print_endline,
      raw_data
    );
  let compressed_len = Bytes.length(compressed);
  Printf.sprintf("Compressed length %d", compressed_len) |> print_endline;
  let decompressed =
    Brotli.Decompress.bytes(
      ~on_part_decompressed=
        (piece) => Printf.sprintf("Decompress piece %d", Nativeint.to_int(piece)) |> print_endline,
      compressed
    );
  let decompressed_len = Bytes.length(decompressed);
  Printf.sprintf("Decompressed length %d, data:%s", decompressed_len, decompressed)
  |> print_endline;
  if (String.compare(raw_data, decompressed) == 0) {
    print_endline("Data was correct in roundtrip");
  } else {
    failwith("Data was not equal during roundtrip");
  };
};

let read_file_content = (file_path) => {
  let ic = open_in(file_path);
  let stats = Unix.stat(file_path);
  let buff = Buffer.create(1024);
  Buffer.add_channel(buff, ic, stats.Unix.st_size);
  close_in(ic);
  Buffer.contents(buff);
};

let test_two = () => {
  let cwd = Sys.getcwd();
  let original_alice = Printf.sprintf("%s/example/alice29.txt", cwd);
  let compressed_alice = Printf.sprintf("%s/example/alice.test.compressed", cwd);
  let decompressed_alice = Printf.sprintf("%s/example/alice.test.decompressed", cwd);
  Brotli.Compress.file(
    ~on_part_compressed=
      (part) =>
        Printf.sprintf("Compressed %d bytes of Alice file", Nativeint.to_int(part))
        |> print_endline,
    ~in_filename=original_alice,
    ~out_filename=compressed_alice,
    ()
  );
  Brotli.Decompress.file(~in_filename=compressed_alice, ~out_filename=decompressed_alice, ());
  /* Compare file content of decompressed and original alice */
  let decompressed_content = read_file_content(decompressed_alice);
  let original_content = read_file_content(original_alice);
  Printf.sprintf(
    "Compare test with baseline %b",
    if (String.compare(decompressed_content, original_content) == 0) {
      true;
    } else {
      false;
    }
  )
  |> print_endline;
  Unix.unlink(compressed_alice);
  Unix.unlink(decompressed_alice);
};

let () = {
  test_one();
  test_two();
};
