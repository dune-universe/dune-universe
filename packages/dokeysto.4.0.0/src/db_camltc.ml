
module Bdb = Camltc.Bdb

type filename = string

module RW = struct

  type t = Internal_camltc.db

  let create fn =
    let mode = Bdb.(oreader lor owriter lor ocreat lor otrunc lor onolck) in
    let bdb = Bdb.create ~mode fn [] in
    Internal_camltc.{ fn; bdb }

  let open_existing fn =
    let mode = Bdb.(oreader lor owriter lor onolck) in
    let bdb = Bdb.create ~mode fn [] in
    Internal_camltc.{ fn; bdb }

  let dummy () =
    let dummy_fn = Filename.temp_file "" "" in
    create dummy_fn

  let sync db =
    Bdb.sync Internal_camltc.(db.bdb)

  let close db =
    Internal_camltc.close db

  let destroy db =
    Internal_camltc.(
      Bdb.close db.bdb;
      Bdb.delete db.bdb;
      Sys.remove db.fn
    )

  let mem db k =
    Internal_camltc.mem db k

  let find db k =
    Internal_camltc.find db k

  let add db k str =
    Bdb.put Internal_camltc.(db.bdb) k str

  let replace db k str =
    Internal_camltc.(
      Bdb.out db.bdb k;
      Bdb.put db.bdb k str
    )

  let remove db k =
    Bdb.out Internal_camltc.(db.bdb) k

  let iter f db =
    Internal_camltc.iter f db

  let fold f db init =
    Internal_camltc.fold f db init

end

module RO = struct

  type t = Internal_camltc.db

  let open_existing fn =
    let mode = Bdb.(oreader lor onolck) in
    let bdb = Bdb.create ~mode fn [] in
    Internal_camltc.{ fn; bdb }

  let dummy () =
    let db = RW.dummy () in
    RW.close db;
    open_existing db.fn

  let close db =
    Internal_camltc.close db

  let mem db k =
    Internal_camltc.mem db k

  let find db k =
    Internal_camltc.find db k

  let iter f db =
    Internal_camltc.iter f db

  let fold f db init =
    Internal_camltc.fold f db init

end
