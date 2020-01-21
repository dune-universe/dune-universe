
module Db_camltc_RW = Db_camltc.RW
module Db_camltc_RO = Db_camltc.RO

type filename = string

module RW (KV: Dokeysto.Db_gen.Key_val) = struct

  type t = Db_camltc_RW.t

  let create fn =
    Db_camltc_RW.create fn

  let open_existing fn =
    Db_camltc_RW.open_existing fn

  let dummy () =
    Db_camltc_RW.dummy ()

  let sync db =
    Db_camltc_RW.sync db

  let close db =
    Db_camltc_RW.close db

  let destroy db =
    Db_camltc_RW.destroy db

  let mem db k =
    Db_camltc_RW.mem db (KV.string_of_key k)

  let find db k =
    KV.value_of_string (Db_camltc_RW.find db (KV.string_of_key k))

  let add db k v =
    Db_camltc_RW.add db (KV.string_of_key k) (KV.string_of_value v)

  let replace db k v =
    Db_camltc_RW.replace db (KV.string_of_key k) (KV.string_of_value v)

  let remove db k =
    Db_camltc_RW.remove db (KV.string_of_key k)

  let iter f db =
    Db_camltc_RW.iter (fun k_str v_str ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str)
      ) db

  let fold f db init =
    Db_camltc_RW.fold (fun k_str v_str acc ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str) acc
      ) db init

end

module RO (KV: Dokeysto.Db_gen.Key_val) = struct

  type t = Db_camltc_RO.t

  let open_existing fn =
    Db_camltc_RO.open_existing fn

  let dummy () =
    Db_camltc_RO.dummy ()

  let close db =
    Db_camltc_RO.close db

  let mem db k =
    Db_camltc_RO.mem db (KV.string_of_key k)

  let find db k =
    KV.value_of_string (Db_camltc_RO.find db (KV.string_of_key k))

  let iter f db =
    Db_camltc_RO.iter (fun k_str v_str ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str)
      ) db

  let fold f db init =
    Db_camltc_RO.fold (fun k_str v_str acc ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str) acc
      ) db init

end
