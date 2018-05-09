
type filename = string

let string_of_key (k: 'a): string =
  Marshal.(to_string k [No_sharing])

let string_of_value (v: 'b): string =
  Marshal.(to_string v [No_sharing])

let key_of_string (k_str: string): 'a =
  (Marshal.from_string k_str 0: 'a)

let value_of_string (v_str: string): 'b =
  (Marshal.from_string v_str 0: 'b)

(* val find: t -> string -> 'b
   we have to marshal/unmarshal values *)
module StrKeyToGenVal = struct

  type 'b t = Dbm.t

  let open_new fn =
    Dbm.(opendbm fn [Dbm_rdwr; Dbm_create] 0o600)

  let open_existing fn =
    Dbm.(opendbm fn [Dbm_rdwr] 0o600)

  let close db =
    Dbm.close db

  let mem db k =
    try let _ = Dbm.find db k in true
    with Not_found -> false

  let add db k v =
    Dbm.add db k (string_of_value v)

  let replace db k v =
    Dbm.replace db k (string_of_value v)

  let remove db k =
    Dbm.remove db k

  let find db k =
    value_of_string (Dbm.find db k)

  let iter f db =
    Dbm.iter (fun k v_str ->
        f k (value_of_string v_str)
      ) db

  let fold f db init =
    let acc = ref init in
    iter (fun k v ->
        acc := f k v !acc
      ) db;
    !acc

end

(* val find: t -> string -> string
   this is what dbm provides in fact; not any marshal/unmarshal needed *)
module StrKeyToStrVal = struct

  type t = Dbm.t

  let open_new fn =
    Dbm.(opendbm fn [Dbm_rdwr; Dbm_create] 0o600)

  let open_existing fn = 
    Dbm.(opendbm fn [Dbm_rdwr] 0o600)

  let close db =
    Dbm.close db

  let mem db k =
    try let _ = Dbm.find db k in true
    with Not_found -> false

  let add db k v =
    Dbm.add db k v

  let replace db k v =
    Dbm.replace db k v

  let remove db k =
    Dbm.remove db k

  let find db k =
    Dbm.find db k

  let iter f db =
    Dbm.iter (fun k v ->
        f k v
      ) db

  let fold f db init =
    let acc = ref init in
    iter (fun k v ->
        acc := f k v !acc
      ) db;
    !acc

end

(* val find: t -> 'a -> 'b
   we have to marshal/unmarshal keys and values *)
module GenKeyToGenVal = struct

  type ('a, 'b) t = Dbm.t

  let open_new fn =
    Dbm.(opendbm fn [Dbm_rdwr; Dbm_create] 0o600)

  let open_existing fn =
    Dbm.(opendbm fn [Dbm_rdwr] 0o600)

  let close db =
    Dbm.close db

  let mem db k =
    try let _ = Dbm.find db (string_of_key k) in true
    with Not_found -> false

  let add db k v =
    Dbm.add db (string_of_key k) (string_of_value v)

  let replace db k v =
    Dbm.replace db (string_of_key k) (string_of_value v)

  let remove db k =
    Dbm.remove db (string_of_key k)

  let find db k =
    value_of_string (Dbm.find db (string_of_key k))

  let iter f db =
    Dbm.iter (fun k_str v_str ->
        f (key_of_string k_str) (value_of_string v_str)
      ) db

  let fold f db init =
    let acc = ref init in
    iter (fun k v ->
        acc := f k v !acc
      ) db;
    !acc

end

(* val find: t -> 'a -> string
   we have to marshal/unmarshal keys *)
module GenKeyToStrVal = struct

  type 'a t = Dbm.t

  let open_new fn =
    Dbm.(opendbm fn [Dbm_rdwr; Dbm_create] 0o600)

  let open_existing fn =
    Dbm.(opendbm fn [Dbm_rdwr] 0o600)

  let close db =
    Dbm.close db

  let mem db k =
    try let _ = Dbm.find db (string_of_key k) in true
    with Not_found -> false

  let add db k v =
    Dbm.add db (string_of_key k) v

  let replace db k v =
    Dbm.replace db (string_of_key k) v

  let remove db k =
    Dbm.remove db (string_of_key k)

  let find db k =
    Dbm.find db (string_of_key k)

  let iter f db =
    Dbm.iter (fun k_str v ->
        f (key_of_string k_str) v
      ) db

  let fold f db init =
    let acc = ref init in
    iter (fun k v ->
        acc := f k v !acc
      ) db;
    !acc

end
