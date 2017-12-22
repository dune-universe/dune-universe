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
