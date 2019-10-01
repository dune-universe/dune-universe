
(** {4 Read-only persistent string key to string value hash table
    (values are uncompressed on the fly using LZ4)} *)

module ROZ: module type of Dokeysto.Db.RO

(** {4 Read-write persistent string key to string value hash table
    (values are compressed/uncompressed on the fly using LZ4)} *)

module RWZ: module type of Dokeysto.Db.RW
