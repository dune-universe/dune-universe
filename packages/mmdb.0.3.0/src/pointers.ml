open Base

module Int_ptr = struct
  let allocate () : int Ctypes.ptr =
    let array = Ctypes.CArray.make Ctypes.int ~initial:0 1 in
    Ctypes.CArray.start array
end

module Char_ptr = struct
  let of_string string =
    let length = Int.succ @@ String.length string in
    let result = Ctypes.CArray.make Ctypes.char ~initial:'\x00' length in
    let set_character index character = Ctypes.CArray.set result index character in
    String.foldi string ~init:() ~f:(fun index _ character ->
        set_character index character );
    Ctypes.CArray.start result

  let to_string char_ptr =
    let char_arr = Ctypes.CArray.from_ptr char_ptr 0 in
    let initial_size = 64 in
    let buffer = Buffer.create initial_size in
    let rec to_string_from index =
      let char = Ctypes.CArray.unsafe_get char_arr index in
      match char with
      | '\x00' -> ()
      | _ ->
          Buffer.add_char buffer char;
          Int.succ index |> to_string_from
    in
    to_string_from 0; Buffer.contents buffer

  let to_string_of_length length char_ptr =
    let char_arr = Ctypes.CArray.from_ptr char_ptr length in
    let buffer = Buffer.create length in
    let rec add_chars_from index =
      if index < length
      then (
        let char = Ctypes.CArray.get char_arr index in
        Buffer.add_char buffer char;
        Int.succ index |> add_chars_from )
    in
    add_chars_from 0; Buffer.contents buffer
end

module Char_ptr_ptr = struct
  let of_string_list strings =
    let char_ptrs = List.map strings ~f:Char_ptr.of_string in
    let length = Int.succ @@ List.length strings in
    let array = Ctypes.(CArray.make (ptr char) ~initial:(from_voidp char null) length) in
    List.iteri char_ptrs ~f:(fun index char_ptr -> Ctypes.CArray.set array index char_ptr);
    Ctypes.CArray.start array

  let to_string_list count ptrs =
    let array = Ctypes.CArray.from_ptr ptrs count in
    List.init count ~f:(fun index -> Ctypes.CArray.get array index |> Char_ptr.to_string)
end

module Mmdb = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_s" in
    let size = Mmdb_ffi.Helpers.size_of_mmdb_s () |> Unsigned.Size_t.to_int in
    let alignment = Mmdb_ffi.Helpers.alignment_of_mmdb_s () |> Unsigned.Size_t.to_int in
    Ctypes.abstract ~name ~size ~alignment

  let allocate ?(finalise = Fn.ignore) () : Mmdb_types.Mmdb.t =
    let finalise value = Ctypes.to_voidp value |> finalise in
    Ctypes.allocate_n ~finalise t ~count:1 |> Ctypes.to_voidp
end

module Entry_data = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_entry_data_s" in
    let size = Mmdb_ffi.Helpers.size_of_mmdb_entry_data_s () |> Unsigned.Size_t.to_int in
    let alignment =
      Mmdb_ffi.Helpers.alignment_of_mmdb_entry_data_s () |> Unsigned.Size_t.to_int
    in
    Ctypes.abstract ~name ~size ~alignment

  let allocate () : Mmdb_types.Entry_data.t =
    Ctypes.allocate_n t ~count:1 |> Ctypes.to_voidp
end
