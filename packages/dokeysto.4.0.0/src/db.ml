
type position = Common.position

type filename = Common.filename

module RO = struct

  type t = Internal.db

  let open_existing fn =
    Internal.open_ro fn

  let dummy () =
    Internal.dummy ()

  let close db =
    Internal.close_simple db

  let mem db k =
    Internal.mem db k

  let find db k =
    Internal.find db k

  let raw_read db pos =
    Internal.raw_read db pos

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end

module RW = struct

  type t = Internal.db

  let create fn =
    Internal.create fn

  let open_existing fn =
    Internal.open_rw fn

  let dummy () =
    Internal.dummy ()

  let close db =
    Internal.close_sync_index db

  let sync db =
    Internal.sync db

  let destroy db =
    Internal.destroy db

  let mem db k =
    Internal.mem db k

  let add db k str =
    Internal.add db k str

  let replace db k str =
    Internal.replace db k str

  let remove db k =
    Internal.remove db k

  let find db k =
    Internal.find db k

  let raw_read db pos =
    Internal.raw_read db pos

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end
