open Ctypes

let t = T.t
let t_opt = T.t_opt

exception VSI_error

let err = T.err VSI_error

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let memory_buffers : (string, bigstring) Hashtbl.t = Hashtbl.create 1

let from_mem_buffer =
  Lib.c "VSIFileFromMemBuffer"
    (string @-> ptr void @-> int @-> int @-> returning t_opt)

let unlink =
  Lib.c "VSIUnlink"
    (string @-> returning err)

let close =
  Lib.c "VSIFCloseL"
    (t @-> returning err)

let of_buffer filename buffer =
  let buffer_ptr = bigarray_start array1 buffer in
  let buffer_length = Bigarray.Array1.dim buffer in
  match from_mem_buffer filename (to_voidp buffer_ptr) buffer_length 0 with
  | None -> raise VSI_error
  | Some vsf ->
    (* Keep a reference to the underlying buffer *)
    Hashtbl.add memory_buffers filename buffer;
    close vsf

let unlink filename =
  unlink filename;
  Hashtbl.remove memory_buffers filename;
  ()
