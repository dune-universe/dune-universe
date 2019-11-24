type 'a hash_consed = {node: 'a; tag: int}

let get_initial_cache_size, set_initial_cache_size, reset_initial_cache_size =
  let default = 512 in
  let initial_cache_size = ref default in
  ( (fun () -> !initial_cache_size)
  , (fun size -> initial_cache_size := size)
  , fun () -> initial_cache_size := default )

module Mk (Cache : Hashtbl.S) = struct
  type t = Cache.key hash_consed Cache.t

  let tbl = Cache.create (get_initial_cache_size ())

  let clear () = Cache.clear tbl

  let iter f = Cache.iter f tbl

  let stats () = Cache.stats tbl

  let hashcons =
    let gen =
      let count = ref (-1) in
      fun () -> incr count ; !count
    in
    fun k ->
      try Cache.find tbl k
      with Not_found ->
        let v = {tag= gen (); node= k} in
        Cache.add tbl k v ; v
end

module Make (H : Hashtbl.HashedType) = struct
  include Mk (Ephemeron.K1.Make (H))
end

module MakeStrong (H : Hashtbl.HashedType) = struct
  include Mk (Hashtbl.Make (H))
end
