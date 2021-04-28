type 'a tag = { name : string; pp : 'a Fmt.t }

module Info = struct
  type 'a t = 'a tag = { name : string; pp : 'a Fmt.t }
end

include Hmap.Make (Info)

module K = struct
  open Value

  let v : version key = Key.create { name = "version"; pp = Fmt.int }

  let a : (algorithm * hash) key =
    Key.create
      {
        name = "algorithm";
        pp = Fmt.Dump.pair Value.pp_algorithm Value.pp_hash;
      }

  let b : base64 key = Key.create { name = "signature"; pp = Fmt.string }

  let bh : base64 key = Key.create { name = "hash"; pp = Fmt.string }

  let c : (canonicalization * canonicalization) key =
    Key.create
      {
        name = "canonicalization";
        pp = Fmt.Dump.pair Value.pp_canonicalization Value.pp_canonicalization;
      }

  let d : domain_name key =
    Key.create { name = "domain"; pp = Value.pp_domain_name }

  let h : Mrmime.Field_name.t list key =
    Key.create { name = "field"; pp = Fmt.Dump.list Mrmime.Field_name.pp }

  let i : auid key = Key.create { name = "auid"; pp = Value.pp_auid }

  let l : int key = Key.create { name = "length"; pp = Fmt.int }

  let q : query list key =
    Key.create { name = "query"; pp = Fmt.Dump.list Value.pp_query }

  let s : selector key =
    Key.create { name = "selector"; pp = Value.pp_selector }

  let t : int64 key = Key.create { name = "timestamp"; pp = Fmt.int64 }

  let x : int64 key = Key.create { name = "expiration"; pp = Fmt.int64 }

  let z : copies key =
    Key.create { name = "copies"; pp = Fmt.Dump.list Value.pp_copy }

  let sv : server_version key =
    Key.create { name = "server-version"; pp = Fmt.string }

  let sh : hash list key =
    Key.create { name = "hashes"; pp = Fmt.Dump.list Value.pp_hash }

  let k : algorithm key =
    Key.create { name = "algorithm"; pp = Value.pp_algorithm }

  let p : base64 key = Key.create { name = "public-key"; pp = Fmt.string }

  let n : string key = Key.create { name = "notes"; pp = Fmt.string }

  let ss : service list key =
    Key.create { name = "services"; pp = Fmt.Dump.list Value.pp_service }

  let st : name list key =
    Key.create { name = "names"; pp = Fmt.Dump.list Value.pp_name }
end
