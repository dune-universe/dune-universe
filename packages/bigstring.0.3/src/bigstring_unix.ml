
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** {2 I/O} *)

let get_bounds name ?(off=0) ?len t =
  let buffer_len = Bigarray.Array1.dim t in
  let len = match len with
    | Some len -> len
    | None -> buffer_len - off
  in
  if len < 0 || off < 0 || buffer_len - off < len
  then invalid_arg ("Bigstring_unix." ^ name)
  else (off, len)

external read_fd  : Unix.file_descr -> t -> int -> int -> int = "ocaml_bigstring_unix_read"
external write_fd : Unix.file_descr -> t -> int -> int -> int = "ocaml_bigstring_unix_write"

let read fd ?off ?len t =
  let off, len = get_bounds "read" ?off ?len t in
  read_fd fd t off len
and write fd ?off ?len t =
  let off, len = get_bounds "write" ?off ?len t in
  write_fd fd t off len


(** {2 Memory-map} *)

let map_file_descr ?pos ?(shared=false) fd len =
  Bigarray.array1_of_genarray @@
    Bigstring_compat.map_file fd ?pos Bigarray.char Bigarray.c_layout shared [|len|]

let with_map_file ?pos ?len ?(mode=0o644) ?(flags=[Open_rdonly]) ?shared name f =
  let ic = open_in_gen flags mode name in
  let len = match len with
    | None -> in_channel_length ic
    | Some n -> n
  in
  let a = map_file_descr ?pos ?shared (Unix.descr_of_in_channel ic) len in
  try
    let x = f a in
    close_in ic;
    x
  with e ->
    close_in ic;
    raise e
