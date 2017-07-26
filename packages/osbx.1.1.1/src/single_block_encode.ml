open Sbx_block
open Stdint

let test () : unit =
  let common     = Header.make_common_fields `V1 in
  let data_block = Block.make_data_block ~seq_num:(Uint32.of_int 1) common ~data:(Bytes.make 496 'a') in

  for _ = 0 to 21150 do
    Block.to_bytes data_block |> ignore
  done
;;

test ()
