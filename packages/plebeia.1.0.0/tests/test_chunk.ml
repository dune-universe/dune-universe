(* node storage test *)
open Plebeia.Internal
open Test_utils
    
module RS = Random.State

let () = 
  let st = Random.State.make_self_init () in
  for _ = 1 to 10000 do
    test_with_context 1000000 @@ fun c ->
    Storage.Chunk.test_write_read st c.Context.storage
  done
