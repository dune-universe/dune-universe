
(** A Mirage block module backed by a Cstruct for unit testing the NBD server *)
module Block : (Mirage_block_lwt.S with type t = Cstruct.t) = struct
  type page_aligned_buffer = Cstruct.t
  type error = Mirage_block.error
  let pp_error = Mirage_block.pp_error
  type write_error = Mirage_block.write_error
  let pp_write_error = Mirage_block.pp_write_error
  type 'a io = 'a Lwt.t
  type t = Cstruct.t
  let disconnect _ = Lwt.return_unit
  let get_info contents = Lwt.return Mirage_block.{ read_write = true; sector_size = 1; size_sectors = (Cstruct.len contents |> Int64.of_int) }
  let read contents sector_start buffers =
    let sector_start = Int64.to_int sector_start in
    List.fold_left
      (fun contents buffer -> Cstruct.fillv ~src:[contents] ~dst:buffer |> ignore; Cstruct.shift contents (Cstruct.len buffer))
      (Cstruct.shift contents sector_start)
      buffers
    |> ignore; Lwt.return_ok ()
  let write contents sector_start buffers =
    let sector_start = Int64.to_int sector_start in
    Cstruct.fillv ~src:buffers ~dst:(Cstruct.shift contents sector_start)
    |> ignore; Lwt.return_ok ()
end
