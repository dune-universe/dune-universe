These are ReasonML bindings to the compression algorithm/code released by
Google called [Brotli](https://github.com/google/brotli).

# Dependencies
Make sure you have the *brotli* library installed. On OS X you can do
`brew install brotli`, as of 20/08/17, Brotli is on debian (Sid).

# Installation

I assume that you have [opam](https://opam.ocaml.org) installed, it is
OCaml's package manager.

```shell
$ opam install brotli
```

if wanting to install locally, then I recommend using an `opam pin`,
assuming you're in the root of the directory, do: 

```shell
$ opam pin add brotli . -y
```

Compile with:

```shell
$ make build
```

Or play with it directly in `utop`:

![](./compressed_image.png)

The API is very simple and limited to compressing, decompressing byte
strings and files; type `make docs` in the source repo and you get
pretty HTML docs in the `docs` repo, just open `index.html`.

See the documentation [online](http://hyegar.com/reasonml-brotli/)

# API 

This is directly the commented `rei`, you can control compression
settings.

```javascript
/** ReasonML bindings to the Brotli compression library */;

/** Provides two functions for decompressing Brotli algorithm
    compressed bytes, files; functions may raise Failure
    exception. Note that on_part_decompressed and on_part_compressed
    require OCaml to acquire and release the runtime lock
    repeatedly.  */
module Decompress: {
  /** Decompress compressed byte string with optional callback */
  let bytes: (~on_part_decompressed: Nativeint.t => unit=?, bytes) => bytes;
  /** Brotli decoder version */
  let version: string;
  /** Decompress the input file to the output file */
  let file:
    (
      ~on_part_decompressed: Nativeint.t => unit=?,
      ~in_filename: string,
      ~out_filename: string,
      unit
    ) =>
    unit;
};

/** Provides functions for compression using the Brotli algorithm with
    adjustable parameters, defaults are what Google uses. Be aware
    that compression may raise Failure exception */
module Compress: {
  /** Brotli encoder version */
  let version: string;
  type mode =
    /** Compression is not aware of any special features of input */ | Generic
    /** Compression knows that input is UTF-8 */ | Text
    /** Compression knows that input is WOFF 2.0 */ | Font;
  /** Controls the compression-speed vs compression-density
      tradeoffs. The higher the quality, the slower the
      compression. Range is `_0 to `_11. */
  type quality = [ | `_0 | `_1 | `_2 | `_3 | `_4 | `_5 | `_6 | `_7 | `_8 | `_9 | `_10 | `_11];
  /** Base 2 logarithm of the sliding window size. Range is `_10 to
      `_24. */
  type lgwin = [
    | `_10
    | `_11
    | `_12
    | `_13
    | `_14
    | `_15
    | `_16
    | `_17
    | `_18
    | `_19
    | `_20
    | `_21
    | `_22
    | `_23
    | `_24
  ];
  /** Base 2 logarithm of the maximum input block size. Range is `_16 to
      `_24. If set to `_0, the value will be set based on the quality. */
  type lgblock = [ | `_0 | `_16 | `_17 | `_18 | `_19 | `_20 | `_21 | `_22 | `_23 | `_24];
  /** Compress the given bytes string to a compressed bytes string */
  let bytes:
    (
      ~mode: mode=?,
      ~quality: quality=?,
      ~lgwin: lgwin=?,
      ~lgblock: lgblock=?,
      ~on_part_compressed: Nativeint.t => unit=?,
      bytes
    ) =>
    bytes;
  /** Compress in the input file to the output file name */
  let file:
    (
      ~mode: mode=?,
      ~quality: quality=?,
      ~lgwin: lgwin=?,
      ~lgblock: lgblock=?,
      ~on_part_compressed: Nativeint.t => unit=?,
      ~in_filename: string,
      ~out_filename: string,
      unit
    ) =>
    unit;
};
```

Here's an example usage:

```javascript
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
```
