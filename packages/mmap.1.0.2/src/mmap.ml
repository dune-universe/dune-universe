module V1 = struct
  external init : _ Bigarray.Array1.t -> unit = "caml_mmap_init"

  let init =
    lazy
      (let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 1 in
       init ba)

  external map_internal :
       Unix.file_descr
    -> ('a, 'b) Bigarray.kind
    -> 'c Bigarray.layout
    -> bool
    -> int array
    -> int64
    -> ('a, 'b, 'c) Bigarray.Genarray.t
    = "caml_mmap_map_file_bytecode" "caml_mmap_map_file"

  let map_file fd ?(pos = 0L) kind layout shared dims =
    Lazy.force init;
    map_internal fd kind layout shared dims pos
end
