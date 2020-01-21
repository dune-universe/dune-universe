open Printf
open Common

module Ht = Hashtbl

type db = { data_fn: filename;
            index_fn: filename;
            data: Unix.file_descr;
            index: (string, position) Ht.t }

let create fn =
  let data_fn = fn in
  let index_fn = fn ^ ".idx" in
  let data =
    Unix.(openfile data_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
  (* we just check there is not already an index file *)
  let index_file =
    Unix.(openfile index_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
  Unix.close index_file;
  let index = Ht.create 11 in
  { data_fn; index_fn; data; index }

let open_rw fn =
  let data_fn = fn in
  let index_fn = fn ^ ".idx" in
  let data =
    Unix.(openfile data_fn [O_RDWR] 0o600) in
  let index = Utls.restore index_fn in
  { data_fn; index_fn; data; index }

let open_ro fn =
  let data_fn = fn in
  let index_fn = fn ^ ".idx" in
  let data =
    Unix.(openfile data_fn [O_RDONLY] 0o600) in
  let index = Utls.restore index_fn in
  { data_fn; index_fn; data; index }

let dummy () =
  { data_fn = "/dev/null";
    index_fn = "/dev/null.idx";
    data = Unix.(openfile "/dev/null" [O_RDWR] 0o600);
    index = Ht.create 0 }

let close_simple db =
  Unix.close db.data

let close_sync_index db =
  Unix.close db.data;
  Utls.save db.index_fn db.index

let sync db =
  ExtUnix.All.fsync db.data;
  Utls.save db.index_fn db.index

let destroy db =
  Ht.reset db.index;
  Unix.close db.data;
  Sys.remove db.data_fn;
  Sys.remove db.index_fn

let mem db k =
  Ht.mem db.index k

let add db k str =
  (* go to end of data file *)
  let off = Unix.(lseek db.data 0 SEEK_END) in
  let len = String.length str in
  let written = Unix.write_substring db.data str 0 len in
  begin
    if written <> len then
      let err_msg =
        sprintf
          "Db.Internal.add: db: %s k: %s str: %s written: %d len: %d"
          db.data_fn k str written len in
      failwith err_msg
  end;
  Ht.add db.index k { off; len }

let replace db k str =
  (* go to end of data file *)
  let off = Unix.(lseek db.data 0 SEEK_END) in
  let len = String.length str in
  let written = Unix.write_substring db.data str 0 len in
  begin
    if written <> len then
      let err_msg =
        sprintf
          "Db.Internal.replace: db: %s k: %s str: %s written: %d len: %d"
          db.data_fn k str written len in
      failwith err_msg
  end;
  Ht.replace db.index k { off; len }

let remove db k =
  (* we just remove it from the index, not from the data file *)
  Ht.remove db.index k

let raw_read db pos =
  let off = pos.off in
  let len = pos.len in
  let buff = Bytes.create len in
  let off' = Unix.(lseek db.data off SEEK_SET) in
  begin
    if off' <> off then
      let err_msg =
        sprintf "Db.Internal.raw_read: db: %s off: %d len: %d off': %d"
          db.data_fn off len off' in
      failwith err_msg
  end;
  let read = Unix.read db.data buff 0 len in
  begin
    if read <> len then
      let err_msg =
        sprintf "Db.Internal.raw_read: db: %s off: %d len: %d read: %d"
          db.data_fn off len read in
      failwith err_msg
  end;
  Bytes.unsafe_to_string buff

let find db k =
  let v_addr = Ht.find db.index k in
  raw_read db v_addr

let iter f db =
  Ht.iter (fun k v ->
      f k (raw_read db v)
    ) db.index

let fold f db init =
  Ht.fold (fun k v acc ->
      f k (raw_read db v) acc
    ) db.index init
