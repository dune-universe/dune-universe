
type filename = Common.filename

module type Key_val = sig
  val string_of_key: 'a -> string
  val key_of_string: string -> 'a

  val string_of_value: 'b -> string
  val value_of_string: string -> 'b
end

module RO (KV: Key_val) = struct

  type t = Internal.db

  let open_existing fn =
    Internal.open_ro fn

  let dummy () =
    Internal.dummy ()

  let close db =
    Internal.close_simple db

  let mem db k =
    Internal.mem db (KV.string_of_key k)

  let find db k =
    KV.value_of_string (Internal.find db (KV.string_of_key k))

  let iter f db =
    Internal.iter (fun k_str v_str ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str)
      ) db

  let fold f db init =
    Internal.fold (fun k_str v_str acc ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str) acc
      ) db init

end

module RW (KV: Key_val) = struct

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
    Internal.mem db (KV.string_of_key k)

  let add db k v =
    Internal.add db (KV.string_of_key k) (KV.string_of_value v)

  let replace db k v =
    Internal.replace db (KV.string_of_key k) (KV.string_of_value v)

  let remove db k =
    Internal.remove db (KV.string_of_key k)

  let find db k =
    KV.value_of_string (Internal.find db (KV.string_of_key k))

  let iter f db =
    Internal.iter (fun k_str v_str ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str)
      ) db

  let fold f db init =
    Internal.fold (fun k_str v_str acc ->
        f (KV.key_of_string k_str) (KV.value_of_string v_str) acc
      ) db init

end
