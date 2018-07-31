open Reed_solomon_erasure

let () =
  let r = ReedSolomon.make 3 2 in

  let sbs = ShardByShard.make r in

  let shards = [|Bytes.of_string "\000\001\002\003\004";
                 Bytes.of_string "\005\006\007\008\009";
                 (* say we don't have the 3rd data shard yet
                    and we want to fill it in later *)
                 Bytes.of_string "\000\000\000\000\000";
                 Bytes.of_string "\000\000\000\000\000";
                 Bytes.of_string "\000\000\000\000\000"|] in

  (* encode 1st and 2nd data shard *)
  ShardByShard.encode_bytes sbs shards;
  ShardByShard.encode_bytes sbs shards;

  (* fill in 3rd data shard *)
  Bytes.set shards.(2) 0 '\010';
  Bytes.set shards.(2) 1 '\011';
  Bytes.set shards.(2) 2 '\012';
  Bytes.set shards.(2) 3 '\013';
  Bytes.set shards.(2) 4 '\014';

  (* now do the encoding*)
  ShardByShard.encode_bytes sbs shards;

  (* above is equivalent to doing ReedSolomon.encode_bytes shards *)

  assert (ReedSolomon.verify_bytes r shards)
