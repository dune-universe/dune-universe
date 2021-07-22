module Qc = struct
  let encode_decode_systematic_no_erasure =
    QCheck.Test.make ~count:10_000 ~name:"encode_decode_systematic_no_erasure"
      QCheck.(
        pair
          (array_of_size (Gen.int_range 1 50) (string_of_size (Gen.return 10)))
          (int_bound 70))
      (fun (data_blocks, drop_count_offset) ->
        QCheck.assume (Array.length data_blocks > 0);
        QCheck.assume (Array.for_all (fun x -> String.length x > 0) data_blocks);
        let data_blocks = Array.map Cstruct.of_string data_blocks in
        let max_drop_count = Array.length data_blocks + drop_count_offset in
        match Lt_code.encode ~systematic:true ~max_drop_count data_blocks with
        | Error _ -> false
        | Ok (ctx, drops) -> (
            match
              Lt_code.decode ctx (Lt_code.Drop_set.of_seq @@ Array.to_seq drops)
            with
            | Error _ -> false
            | Ok data_blocks' ->
                let _, has_mismatch =
                  Array.fold_left
                    (fun (i, has_mismatch) data' ->
                      let has_mismatch =
                        has_mismatch
                        || not (Cstruct.equal data' data_blocks.(i))
                      in
                      (succ i, has_mismatch))
                    (0, false) data_blocks'
                in
                not has_mismatch))

  let suite = [ encode_decode_systematic_no_erasure ]
end
