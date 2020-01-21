
module Bdb = Camltc.Bdb

type filename = string

type db = { fn: filename ;
            bdb: Bdb.bdb }

let close db =
  Bdb.close db.bdb

let mem db k =
  Bdb.exists db.bdb k

let find db k =
  Bdb.get db.bdb k

let iter f db =
  if Bdb.get_key_count db.bdb > Int64.one then
    try
      let cur = Bdb.get_cursor db.bdb in
      Bdb.first db.bdb cur;
      while true do
        let k = Bdb.key db.bdb cur in
        let v = Bdb.value db.bdb cur in
        f k v;
        Bdb.next db.bdb cur
      done
    with Not_found -> () (* end of db *)

let fold (f: string -> string -> 'a -> 'a) db init =
  let acc = ref init in
  iter (fun k v ->
      acc := f k v !acc
    ) db;
  !acc
