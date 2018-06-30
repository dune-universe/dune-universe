open Reed_solomon_erasure

let () =
  let r = ReedSolomon.make 3 2 in (* 3 data shards, 2 parity shards *)

  let master_copy = [|"\000\001\002\003";
                      "\004\005\006\007";
                      "\008\009\010\011";
                      "\000\000\000\000"; (* last 2 rows are parity shards *)
                      "\000\000\000\000"|] in

  (* Construct the parity shards *)
  ReedSolomon.encode_str r master_copy;

  (* Make a copy and transform it into option shards arrangement
     for feeding into reconstruct_opt_bytes *)
  let shards = RS_Shard_utils.shards_to_option_shards_str master_copy in

  (* We can remove up to 2 shards, which may be data or parity shards *)
  shards.(0) <- None;
  shards.(4) <- None;

  (* Try to reconstruct missing shards *)
  ReedSolomon.reconstruct_opt_str r shards;

  (* Convert back to normal shard arrangement *)
  let result = RS_Shard_utils.option_shards_to_shards_str shards in

  assert (ReedSolomon.verify_str r result);
  assert (master_copy = result)
