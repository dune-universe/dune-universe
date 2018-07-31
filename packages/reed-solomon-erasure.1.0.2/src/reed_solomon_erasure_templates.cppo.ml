#define def_encode_single_sep(ty) \
  let encode_single_sep \
      (r           : t) \
      (i_data      : int) \
      (single_data : ty) \
      (parity      : ty array) \
    : (unit, error) result = \
    let single_data_bigstr = Bigstring.CONCAT(of_, ty) single_data in \
    let parity_bigstr      = Helper.CONCAT(ty, _array_to_bigstring_array) parity in \
    match BigstringInOut.encode_single_sep r i_data single_data_bigstr parity_bigstr with \
    | Error _ as e -> e \
    | Ok _ -> \
      begin \
        for i = 0 to (r.parity_shard_count) - 1 do \
          parity.(i) <- Bigstring.CONCAT(to_, ty) parity_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_encode_sep(ty) \
  let encode_sep \
      (r      : t) \
      (data   : ty array) \
      (parity : ty array) \
    : (unit, error) result = \
    let data_bigstr   = Helper.CONCAT(ty, _array_to_bigstring_array) data in \
    let parity_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) parity in \
    match BigstringInOut.encode_sep r data_bigstr parity_bigstr with \
    | Error _ as e -> e \
    | Ok _ -> \
      begin \
        for i = 0 to (r.parity_shard_count) - 1 do \
          parity.(i) <- Bigstring.CONCAT(to_, ty) parity_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_encode_single(ty) \
  let encode_single \
      (r           : t) \
      (i_data      : int) \
      (shards      : ty array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) shards in \
    match BigstringInOut.encode_single r i_data shards_bigstr with \
    | Error _ as e -> e \
    | Ok _ -> \
      begin \
        for i = r.data_shard_count to (r.total_shard_count) - 1 do \
          shards.(i) <- Bigstring.CONCAT(to_, ty) shards_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_encode(ty) \
  let encode \
      (r      : t) \
      (shards : ty array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) shards in \
    match BigstringInOut.encode r shards_bigstr with \
    | Error _ as e -> e \
    | Ok _ -> \
      begin \
        for i = r.data_shard_count to (r.total_shard_count) - 1 do \
          shards.(i) <- Bigstring.CONCAT(to_, ty) shards_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_verify_ret_buffer(ty) \
  let verify_ret_buffer \
      (r      : t) \
      (shards : ty array) \
    : (bool * ty array, error) result = \
    match BigstringInOut.verify_ret_buffer r (Helper.CONCAT(ty, _array_to_bigstring_array) shards) with \
    | Error _ as e     -> e \
    | Ok (res, buffer) -> Ok (res, Helper.CONCAT(CONCAT(bigstring_array_to_, ty), _array) buffer)

#define def_verify_with_buffer(ty) \
  let verify_with_buffer \
      (r      : t) \
      (shards : ty array) \
      (buffer : ty array) \
    : (bool, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) shards in \
    let buffer_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) buffer in \
    match BigstringInOut.verify_with_buffer r shards_bigstr buffer_bigstr with \
    | Error _ as e -> e \
    | Ok res -> \
      begin \
        for i = 0 to (Array.length buffer) - 1 do \
          buffer.(i) <- Bigstring.CONCAT(to_, ty) buffer_bigstr.(i) \
        done; \
        \
        Ok res \
      end

#define def_verify(ty) \
  let verify \
      (r      : t) \
      (shards : ty array) \
    : (bool, error) result = \
    BigstringInOut.verify r (Helper.CONCAT(ty, _array_to_bigstring_array) shards)

#define def_reconstruct(ty) \
  let reconstruct \
      (r             : t) \
      (shards        : ty array) \
      (shard_present : bool array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) shards in \
    match BigstringInOut.reconstruct r shards_bigstr shard_present with \
    | Error _ as e -> e \
    | Ok _ -> \
      begin \
        for i = 0 to (r.total_shard_count) - 1 do \
          shards.(i) <- Bigstring.CONCAT(to_, ty) shards_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_reconstruct_data(ty) \
  let reconstruct_data \
      (r             : t) \
      (shards        : ty array) \
      (shard_present : bool array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _array_to_bigstring_array) shards in \
    match BigstringInOut.reconstruct_data r shards_bigstr shard_present with \
    | Error _ as e -> e \
    | Ok _         -> \
      begin \
        for i = 0 to (r.data_shard_count) - 1 do \
          shards.(i) <- Bigstring.CONCAT(to_, ty) shards_bigstr.(i) \
        done; \
        \
        Ok () \
      end

#define def_reconstruct_opt(ty) \
  let reconstruct_opt \
      (r             : t) \
      (shards        : ty option array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _opt_array_to_bigstring_opt_array) shards in \
    match BigstringInOut.reconstruct_opt r shards_bigstr with \
    | Error _ as e -> e \
    | Ok _         -> \
      begin \
        for i = 0 to (r.total_shard_count) - 1 do \
          shards.(i) <- \
            match shards_bigstr.(i) with \
            | None   -> None \
            | Some x -> Some (Bigstring.CONCAT(to_, ty) x) \
        done; \
        \
        Ok () \
      end

#define def_reconstruct_data_opt(ty) \
  let reconstruct_data_opt \
      (r             : t) \
      (shards        : ty option array) \
    : (unit, error) result = \
    let shards_bigstr = Helper.CONCAT(ty, _opt_array_to_bigstring_opt_array) shards in \
    match BigstringInOut.reconstruct_data_opt r shards_bigstr with \
    | Error _ as e -> e \
    | Ok _         -> \
      begin \
        for i = 0 to (r.data_shard_count) - 1 do \
          shards.(i) <- \
            match shards_bigstr.(i) with \
            | None   -> None \
            | Some x -> Some (Bigstring.CONCAT(to_, ty) x) \
        done; \
        \
        Ok () \
      end
