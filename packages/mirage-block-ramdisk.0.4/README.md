# mirage-block-ramdisk
A simple in-memory BLOCK device.

Please see [the API documentation](https://mirage.github.io/mirage-block-ramdisk/).

Features
--------

- Can be dynamically resized
- Supports querying sparseness information

Example usage
-------------

In a top-level like utop:
```ocaml
# #require "io-page.unix";;
# #require "mirage-block";;
# #require "mirage-block-ramdisk";;
# #require "lwt.syntax";;

# lwt t_or_error = Ramdisk.create ~name:"hello" ~size_sectors:1024L ~sector_size:512;;
val t_or_error : [ `Error of Ramdisk.error | `Ok of Ramdisk.t ] = `Ok <abstr>

# let t = Mirage_block.Error.ok_exn t_or_error;;
val t : Ramdisk.t = <abstr>

# let page = Io_page.(to_cstruct (get 1));;
val page : Ramdisk.page_aligned_buffer =
  {Cstruct.buffer = <abstr>; off = 0; len = 4096}

# lwt result_or_error = Ramdisk.read t 0L [ page ];;
val result_or_error : [ `Error of Ramdisk.error | `Ok of unit ] = `Ok ()
```
