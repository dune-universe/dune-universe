(* AUDIT
 *
 * Thread safety
 *
 *   The codec has a component, `tree : Inversion_tree.t`, that may
 *   mutate during operation, the component is a cache of reconstruction
 *   matrices.
 *
 *   The component is currently NOT protected by any locks,
 *   this the entire codec is not thread safe.
 *
 *   Protection will be added when OCaml is multicore enabled
 *   and then it can be decided which concurrency primitives
 *   are appropriate.
 *
 * Use of `Array.sub`
 *
 *   `Array.sub` is similar to mutable slicing in Rust, and when
 *   used with `bytes array` or `bigstring array`, it equates to shared
 *   mutable ownership to the data.
 *
 *   This is not safe in general, but these calls are only
 *   used to generate parameters for tail calls to other
 *   functions in this library or used in small and reconstricted
 *   code sections. So the biggest risk would be incorrect indexing,
 *   this is largely mitigated by `Helper.array_split_at` *)

#include "reed_solomon_erasure_templates.cppo.ml"

open Ops
module Bigstring = Core_kernel.Bigstring

type bigstring = Bigstring.t

let dummy_bigstring = Bigstring.create 0

let build_matrix (data_shards : int) (total_shards : int) : Matrix.t =
  let vandermonde = Matrix.vandermonde total_shards data_shards in

  let top = Matrix.sub_matrix vandermonde 0 0 data_shards data_shards in

  Matrix.multiply vandermonde (Matrix.Error.unwrap (Matrix.invert top))

module RS_Error = struct
  type t = TooFewShards
         | TooManyShards
         | TooFewDataShards
         | TooManyDataShards
         | TooFewParityShards
         | TooManyParityShards
         | TooFewBufferShards
         | TooManyBufferShards
         | IncorrectShardSize
         | TooFewShardsPresent
         | EmptyShard
         | InvalidShardFlags
         | InvalidIndex

  module Exn = struct
    exception TooFewShards
    exception TooManyShards
    exception TooFewDataShards
    exception TooManyDataShards
    exception TooFewParityShards
    exception TooManyParityShards
    exception TooFewBufferShards
    exception TooManyBufferShards
    exception IncorrectShardSize
    exception TooFewShardsPresent
    exception EmptyShard
    exception InvalidShardFlags
    exception InvalidIndex
  end

  let to_exn (err : t) : exn =
    match err with
    | TooFewShards        -> Exn.TooFewShards
    | TooManyShards       -> Exn.TooManyShards
    | TooFewDataShards    -> Exn.TooFewDataShards
    | TooManyDataShards   -> Exn.TooManyDataShards
    | TooFewParityShards  -> Exn.TooFewParityShards
    | TooManyParityShards -> Exn.TooManyParityShards
    | TooFewBufferShards  -> Exn.TooFewBufferShards
    | TooManyBufferShards -> Exn.TooManyBufferShards
    | IncorrectShardSize  -> Exn.IncorrectShardSize
    | TooFewShardsPresent -> Exn.TooFewShardsPresent
    | EmptyShard          -> Exn.EmptyShard
    | InvalidShardFlags   -> Exn.InvalidShardFlags
    | InvalidIndex        -> Exn.InvalidIndex

  let unwrap (res : ('a, t) result) : 'a =
    match res with
    | Ok x    -> x
    | Error e -> raise (to_exn e)
end

type error = RS_Error.t

module RS_SBS_Error = struct
  type t = TooManyCalls
         | LeftoverShards
         | RSError of RS_Error.t

  module Exn = struct
    exception TooManyCalls
    exception LeftoverShards
    exception RSError of RS_Error.t
  end

  let to_exn (err : t) : exn =
    match err with
    | TooManyCalls   -> Exn.TooManyCalls
    | LeftoverShards -> Exn.LeftoverShards
    | RSError e      -> Exn.RSError e

  let unwrap (res : ('a, t) result) : 'a =
    match res with
    | Ok x    -> x
    | Error e -> raise (to_exn e)
end

type sbs_error = RS_SBS_Error.t

module Helper = struct
  let array_split_at (arr : 'a array) (split_at : int) : 'a array * 'a array =
    (Array.sub arr 0        split_at,
     Array.sub arr split_at ((Array.length arr) - split_at))

  let make_buffer (rows : int) (length : int) : bigstring array =
    Array.init
      rows
      (fun _ -> Bigstring.create length)

  let bytes_array_to_bigstring_array (arr : bytes array) : bigstring array =
    Array.map Bigstring.of_bytes arr

  let string_array_to_bigstring_array (arr : string array) : bigstring array =
    Array.map Bigstring.of_string arr

  let bigstring_array_to_bytes_array (arr : bigstring array) : bytes array =
    Array.map Bigstring.to_bytes arr

  let bigstring_array_to_string_array (arr : bigstring array) : string array =
    Array.map Bigstring.to_string arr

  let bytes_opt_array_to_bigstring_opt_array (arr : bytes option array) : bigstring option array =
    Array.map
      (function
        | Some x -> Some (Bigstring.of_bytes x)
        | None   -> None)
      arr

  let string_opt_array_to_bigstring_opt_array (arr : string option array) : bigstring option array =
    Array.map
      (function
        | Some x -> Some (Bigstring.of_string x)
        | None   -> None)
      arr
end

module RS_Shard_utils = struct
  let copy_bigstr (x : bigstring) =
    x |> Bigstring.to_string |> Bigstring.of_string

  let option_shards_to_shards_bytes (shards : bytes option array) : bytes array =
    Array.map
      (function
        | None   -> failwith "Option shards slot is None"
        | Some s -> Bytes.copy s
      )
      shards

  let shards_to_option_shards_bytes (shards : bytes array) : bytes option array =
    Array.map
      (fun x -> Some (Bytes.copy x))
      shards

  let option_shards_to_shards_str (shards : string option array) : string array =
    Array.map
      (function
        | None   -> failwith "Option shards slot is None"
        | Some s -> s
      )
      shards

  let shards_to_option_shards_str (shards : string array) : string option array =
    Array.map
      (fun x -> Some x)
      shards

  let option_shards_to_shards_bigstr (shards : bigstring option array) : bigstring array =
    Array.map
      (function
        | None   -> failwith "Option shards slot is None"
        | Some s -> s |> Bigstring.to_string |> Bigstring.of_string
      )
      shards

  let shards_to_option_shards_bigstr (shards : bigstring array) : bigstring option array =
    Array.map
      (fun x -> Some (copy_bigstr x))
      shards

  let make_blank_shards_bigstr ~(count : int) ~(size_per_shard : int) : bigstring array =
    Array.init count (fun _ -> Bigstring.init size_per_shard ~f:(fun _ -> '\000'))

  let make_blank_shards_str ~(count : int) ~(size_per_shard : int) : string array =
    Array.init count (fun _ -> String.make size_per_shard '\000')

  let make_blank_shards_bytes ~(count : int) ~(size_per_shard : int) : bytes array =
    Array.init count (fun _ -> Bytes.make size_per_shard '\000')

  let option_shards_to_shards_unsafe (shards : 'a option array) : 'a array =
    Array.map
      (function
        | None   -> failwith "Option shards slot is None"
        | Some s -> s
      )
      shards

  let copy_shards_bytes shards =
    Array.map
      Bytes.copy
      shards

  let copy_shards_str shards =
    Array.map
      (fun x -> x |> Bytes.unsafe_of_string |> Bytes.to_string)
      shards

  let copy_shards_bigstr shards =
    Array.map
      (fun x -> copy_bigstr x)
      shards
end

module ReedSolomon = struct
  type t = {
    data_shard_count   : int;
    parity_shard_count : int;
    total_shard_count  : int;
    matrix             : Matrix.t;
    parity_rows        : bytes array;
    tree               : Inversion_tree.t;
  }

  let make_no_exn (data_shards : int) (parity_shards : int) : (t, error) result =
    let calc_parity_rows (data_shards : int) (parity_shards : int) (matrix : Matrix.t) : bytes array =
      let total_shards = data_shards + parity_shards in
      let parity_rows = Array.make parity_shards Bytes.empty in

      for i = data_shards to (total_shards) - 1 do
        parity_rows.(i - data_shards) <- Matrix.get_row matrix i
      done;

      parity_rows
    in

    if      data_shards   <= 0 then
      Error TooFewDataShards
    else if parity_shards <= 0 then
      Error TooFewParityShards
    else if data_shards + parity_shards > 256 then
      Error TooManyShards
    else (
      let total_shards = data_shards + parity_shards in
      let matrix       = build_matrix data_shards total_shards in
      let parity_rows  = calc_parity_rows data_shards parity_shards matrix in

      Ok { data_shard_count   = data_shards;
           parity_shard_count = parity_shards;
           total_shard_count  = total_shards;
           matrix;
           parity_rows;
           tree               = Inversion_tree.make data_shards parity_shards; }
    )

  let make data_shards parity_shards =
    RS_Error.unwrap
      (make_no_exn data_shards parity_shards)

  let data_shard_count (r : t) : int =
    r.data_shard_count

  let parity_shard_count (r : t) : int =
    r.parity_shard_count

  let total_shard_count (r : t) : int =
    r.total_shard_count

  module Internal_coder = struct
    let code_single_shard
        (matrix_rows : bytes array)
        (i_input     : int)
        (input       : bigstring)
        (outputs     : bigstring array)
      : unit =
      for i_row = 0 to (Array.length outputs) - 1 do
        let matrix_row_to_use = matrix_rows.(i_row).%(i_input) in

        if i_input = 0 then
          Galois.mul_slice     matrix_row_to_use input outputs.(i_row)
        else
          Galois.mul_slice_xor matrix_row_to_use input outputs.(i_row)
      done

    let code_some_shards
        (r           : t)
        (matrix_rows : bytes array)
        (inputs      : bigstring array)
        (outputs     : bigstring array)
      : unit =
      for i_input = 0 to (r.data_shard_count) - 1 do
        code_single_shard matrix_rows i_input inputs.(i_input) outputs
      done

    let check_some_shards_with_buffer
        (r           : t)
        (matrix_rows : bytes array)
        (inputs      : bigstring array)
        (to_check    : bigstring array)
        (buffer      : bigstring array)
      : bool =
      code_some_shards r matrix_rows inputs buffer;

      let no_mismatch = ref true in

      for i = 0 to (Array.length buffer) - 1 do
        no_mismatch := !no_mismatch && to_check.(i) = buffer.(i)
      done;

      !no_mismatch
  end

  module Checker = struct
    type shard_type           = Bigstr of bigstring | Str of string | Bytes of bytes
    type shards_type          = Bigstr of bigstring array | Str of string array | Bytes of bytes array
    type check_shard_index_op = All | Data | Parity
    type check_piece_count_op = All | Data | Parity | ParityBuf

    let shard_length (shard : shard_type) : int =
      match shard with
      | Bigstr s -> Bigstring.length s
      | Str s    -> String.length s
      | Bytes s  -> Bytes.length s

    let index_into (shards : shards_type) (index : int) : shard_type =
      match shards with
      | Bigstr shards -> Bigstr shards.(index)
      | Str shards    -> Str shards.(index)
      | Bytes shards  -> Bytes shards.(index)

    let check_option_shards_and_get_size (shards : bigstring option array) : (int, error) result =
      (* record size of first shard *)
      let size                = ref None in
      for i = 0 to (Array.length shards) - 1 do
        match shards.(i) with
        | None -> ()
        | Some b ->
          match !size with
          | None   -> size := Some (Bigstring.length b)
          | Some _ -> ()
      done;

      let check_size_same (size : int) (shard : bigstring option) =
        match shard with
        | None   -> true
        | Some b -> size = Bigstring.length b
      in

      match !size with
      | None      -> Error TooFewShardsPresent
      | Some size ->
        if size = 0 then
          Error EmptyShard
        else (
          let all_sizes_same = ref true in

          for i = 0 to (Array.length shards) - 1 do
            all_sizes_same :=
              !all_sizes_same && (check_size_same size shards.(i))
          done;

          if !all_sizes_same then
            Ok size
          else
            Error IncorrectShardSize
        )

    let check_shard_index
        (r     : t)
        (op    : check_shard_index_op)
        (index : int)
      : (unit, error) result =
      let ub = match op with
        | All    -> r.total_shard_count
        | Data   -> r.data_shard_count
        | Parity -> r.parity_shard_count
      in

      if 0 <= index && index < ub then
        Ok ()
      else
        Error InvalidIndex

    let check_piece_count
        (r      : t)
        (op     : check_piece_count_op)
        (pieces : 'a array)
      : (unit, error) result =
      let exact_match = match op with
        | All       -> r.total_shard_count
        | Data      -> r.data_shard_count
        | Parity    -> r.parity_shard_count
        | ParityBuf -> r.parity_shard_count
      in

      let too_few_error = match op with
        | All       -> RS_Error.TooFewShards
        | Data      -> RS_Error.TooFewDataShards
        | Parity    -> RS_Error.TooFewParityShards
        | ParityBuf -> RS_Error.TooFewBufferShards
      in

      let too_many_error = match op with
        | All       -> RS_Error.TooManyShards
        | Data      -> RS_Error.TooManyDataShards
        | Parity    -> RS_Error.TooManyParityShards
        | ParityBuf -> RS_Error.TooManyBufferShards
      in

      let piece_count = Array.length pieces in

      if      piece_count < exact_match then
        Error too_few_error
      else if piece_count > exact_match then
        Error too_many_error
      else
        Ok ()

    let check_piece_count_shards_type
        (r      : t)
        (op     : check_piece_count_op)
        (pieces : shards_type)
      : (unit, error) result =
      match pieces with
      | Bigstr pieces -> check_piece_count r op pieces
      | Str pieces    -> check_piece_count r op pieces
      | Bytes pieces  -> check_piece_count r op pieces

    let check_shards_multi
        (shards : shards_type)
      : (unit, error) result =
      let size =
        match shards with
        | Bigstr shards -> Bigstring.length shards.(0)
        | Str shards    -> String.length shards.(0)
        | Bytes shards  -> Bytes.length shards.(0)
      in
      if size = 0 then
        Error EmptyShard
      else
        let all_sizes_same = ref true in

        (match shards with
         | Bigstr shards ->
           for i = 0 to (Array.length shards) - 1 do
             all_sizes_same := !all_sizes_same && size = Bigstring.length shards.(i)
           done
         | Str shards ->
           for i = 0 to (Array.length shards) - 1 do
             all_sizes_same := !all_sizes_same && size = String.length shards.(i)
           done
         | Bytes shards ->
           for i = 0 to (Array.length shards) - 1 do
             all_sizes_same := !all_sizes_same && size = Bytes.length shards.(i)
           done
        );

        if !all_sizes_same then
          Ok ()
        else
          Error IncorrectShardSize

    let check_shards_single
        (shard_left  : shard_type)
        (shard_right : shard_type)
      : (unit, error) result =
      if shard_length shard_left = shard_length shard_right then
        Ok ()
      else
        Error IncorrectShardSize

    let check_shards_multi_single
        (shards : shards_type)
        (shard  : shard_type)
      : (unit, error) result =
      match check_shards_multi shards with
      | Error _ as e -> e
      | Ok _         -> check_shards_single (index_into shards 0) shard

    let check_shards_multi_multi
        (shards_left  : shards_type)
        (shards_right : shards_type)
      : (unit, error) result =
      match check_shards_multi shards_left with
      | Error _ as e -> e
      | Ok _ ->
        match check_shards_multi shards_right with
        | Error _ as e -> e
        | Ok _ ->
          check_shards_single
            (index_into shards_left  0)
            (index_into shards_right 0)
  end

  module Encode = struct
    module BigstringInOut = struct
      (* AUDIT
       *
       * Error detection responsibilities
       * Terminologies and symbols :
       *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
       *   X := A, B, C   : X needs to handle responsibilities A, B, C
       *
       * Encode methods
       *
       * `encode_single` :=
       *   - check index `i_data` within range [0, data shard count)
       *   - check length of `shards` matches total shard count exactly
       *   - check consistency of length of individual shards
       * `encode_single_sep` :=
       *   - check index `i_data` within range [0, data shard count)
       *   - check length of `parity` matches parity shard count exactly
       *   - check consistency of length of individual parity shards
       *   - check length of `single_data` matches length of first parity shard
       * `encode` :=
       *   - check length of `shards` matches total shard count exactly
       *   - check consistency of length of individual shards
       * `encode_sep` :=
       *   - check length of `data` matches data shard count exactly
       *   - check length of `parity` matches parity shard count exactly
       *   - check consistency of length of individual data shards
       *   - check consistency of length of individual parity shards
       *   - check length of first parity shard matches length of first data shard *)

      let encode_single_sep
          (r           : t)
          (i_data      : int)
          (single_data : bigstring)
          (parity      : bigstring array)
        : (unit, error) result =
        let index_check_result       = Checker.check_shard_index r Checker.Data   i_data in
        let piece_count_check_result = Checker.check_piece_count r Checker.Parity parity in
        let shards_check_result      = Checker.check_shards_multi_single (Bigstr parity) (Bigstr single_data) in
        match index_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match piece_count_check_result with
          | Error _ as e -> e
          | Ok _ ->
            match shards_check_result with
            | Error _ as e -> e
            | Ok _ ->
              begin
                Ok (Internal_coder.code_single_shard
                      r.parity_rows
                      i_data
                      single_data
                      parity)
              end

      let encode_sep
          (r      : t)
          (data   : bigstring array)
          (parity : bigstring array)
        : (unit, error) result =
        let piece_count_check_result_data   = Checker.check_piece_count r Checker.Data   data in
        let piece_count_check_result_parity = Checker.check_piece_count r Checker.Parity parity in
        let shards_check_result             = Checker.check_shards_multi_multi (Bigstr data) (Bigstr parity) in
        match piece_count_check_result_data with
        | Error _ as e -> e
        | Ok _ ->
          match piece_count_check_result_parity with
          | Error _ as e -> e
          | Ok _ ->
            match shards_check_result with
            | Error _ as e -> e
            | Ok _ ->
              begin
                Ok (Internal_coder.code_some_shards
                      r
                      r.parity_rows
                      data
                      parity)
              end

      let encode_single
          (r           : t)
          (i_data      : int)
          (shards      : bigstring array)
        : (unit, error) result =
        let index_check_result       = Checker.check_shard_index r Checker.Data i_data in
        let piece_count_check_result = Checker.check_piece_count r Checker.All  shards in
        let shards_check_result      = Checker.check_shards_multi (Bigstr shards) in
        match index_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match piece_count_check_result with
          | Error _ as e -> e
          | Ok _ ->
            match shards_check_result with
            | Error _ as e -> e
            | Ok _ ->
              begin
                let (input, output) =
                  Helper.array_split_at shards r.data_shard_count in
                let input = input.(i_data) in
                encode_single_sep r i_data input output
              end

      let encode
          (r      : t)
          (shards : bigstring array)
        : (unit, error) result =
        let piece_count_check_result = Checker.check_piece_count r Checker.All shards in
        let shards_check_result      = Checker.check_shards_multi (Bigstr shards) in
        match piece_count_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match shards_check_result with
          | Error _ as e -> e
          | Ok _ ->
            begin
              let (input, output) =
                Helper.array_split_at shards r.data_shard_count in
              encode_sep r input output
            end
    end

    module StringInOut = struct
      def_encode_single_sep(string)

      def_encode_sep(string)

      def_encode_single(string)

      def_encode(string)
    end

    module BytesInOut = struct
      def_encode_single_sep(bytes)

      def_encode_sep(bytes)

      def_encode_single(bytes)

      def_encode(bytes)
    end
  end

  module Verify = struct
    module BigstringInOut = struct
      (* AUDIT
       *
       * Error detection responsibilities
       * Terminologies and symbols :
       *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
       *   X := A, B, C   : X needs to handle responsibilities A, B, C
       *
       * Verify methods
       *
       * `verify` =ALL=> `verify_ret_buffer`
       *
       * `verify_ret_buffer` :=
       *   - check length of `shards` matches total shard count exactly
       *   - check consistency of length of individual shards
       *
       *   Generates buffer then passes control to verify_with_buffer
       * `verify_with_buffer` :=
       *   - check length of `shards` matches total shard count exactly
       *   - check length of `buffer` matches parity shard count exactly
       *   - check consistency of length of individual shards
       *   - check consistency of length of individual shards in buffer
       *   - check length of first shard in buffer matches length of first shard *)

      let verify_with_buffer
          (r      : t)
          (shards : bigstring array)
          (buffer : bigstring array)
        : (bool, error) result =
        let piece_count_check_result_all        = Checker.check_piece_count r Checker.All       shards in
        let piece_count_check_result_parity_buf = Checker.check_piece_count r Checker.ParityBuf buffer in
        let shards_check_result                 = Checker.check_shards_multi_multi (Bigstr shards) (Bigstr buffer) in
        match piece_count_check_result_all with
        | Error _ as e -> e
        | Ok _ ->
          match piece_count_check_result_parity_buf with
          | Error _ as e -> e
          | Ok _ ->
            match shards_check_result with
            | Error _ as e -> e
            | Ok _ ->
              begin
                let (data, to_check) = Helper.array_split_at shards r.data_shard_count in
                Ok (Internal_coder.check_some_shards_with_buffer
                      r
                      r.parity_rows
                      data
                      to_check
                      buffer)
              end

      let verify_ret_buffer
          (r      : t)
          (shards : bigstring array)
        : (bool * bigstring array, error) result =
        let piece_count_check_result   = Checker.check_piece_count r Checker.All   shards in
        let shards_check_result        = Checker.check_shards_multi (Bigstr shards) in
        match piece_count_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match shards_check_result with
          | Error _ as e -> e
          | Ok _ ->
            begin
              let buffer =
                Helper.make_buffer r.parity_shard_count (Bigstring.length shards.(0)) in
              match verify_with_buffer
                      r
                      shards
                      buffer with
              | Error _ as e -> e
              | Ok res       -> Ok (res, buffer)
            end

      let verify
          (r      : t)
          (shards : bigstring array)
        : (bool, error) result =
        match verify_ret_buffer r shards with
        | Error _ as e -> e
        | Ok (res, _)  -> Ok res
    end

    module StringInOut = struct
      def_verify_ret_buffer(string)

      def_verify_with_buffer(string)

      def_verify(string)
    end

    module BytesInOut = struct
      def_verify_ret_buffer(bytes)

      def_verify_with_buffer(bytes)

      def_verify(bytes)
    end
  end

  module Reconstruct = struct
    let get_data_decode_matrix
        (r               : t)
        (valid_indices   : int array)
        (invalid_indices : int array)
      : Matrix.t =
      (* Attempt to get the cached inverted matrix out of the tree
       * based on the indices of the invalid rows. *)
      match Inversion_tree.get_inverted_matrix r.tree invalid_indices with
      (* If the inverted matrix isn't cached in the tree yet we must
       * construct it ourselves and insert it into the tree for the
       * future.  In this way the inversion tree is lazily loaded. *)
      | None -> (
          (* Pull out the rows of the matrix that correspond to the
           * shards that we have and build a square matrix.  This
           * matrix could be used to generate the shards that we have
           * from the original data. *)
          let sub_matrix = Matrix.make r.data_shard_count r.data_shard_count in

          for sub_matrix_row = 0 to (Array.length valid_indices) - 1 do
            let valid_index = valid_indices.(sub_matrix_row) in

            for c = 0 to (r.data_shard_count) - 1 do
              Matrix.set
                sub_matrix
                sub_matrix_row
                c
                (Matrix.get r.matrix valid_index c)
            done
          done;

          (* Invert the matrix, so we can go from the encoded shards
           * back to the original data.  Then pull out the row that
           * generates the shard that we want to decode.  Note that
           * since this matrix maps back to the original data, it can
           * be used to create a data shard, but not a parity shard. *)
          let data_decode_matrix = Matrix.Error.unwrap (Matrix.invert sub_matrix) in

          (* Cache the inverted matrix in the tree for future use keyed on the
           * indices of the invalid rows. *)
          Inversion_tree.Error.unwrap
            (Inversion_tree.insert_inverted_matrix
               r.tree invalid_indices data_decode_matrix);

          data_decode_matrix
        )
      | Some m -> m

    module BigstringInOut : sig
      val reconstruct : t -> bigstring array -> bool array -> (unit, error) result

      val reconstruct_data : t -> bigstring array -> bool array -> (unit, error) result

      val reconstruct_opt : t -> bigstring option array -> (unit, error) result

      val reconstruct_data_opt : t -> bigstring option array -> (unit, error) result
    end = struct
      (* AUDIT
       *
       * Error detection responsibilities
       * Terminologies and symbols :
       *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
       *   X := A, B, C   : X needs to handle responsibilities A, B, C
       *
       * Reconstruct methods
       *
       * `reconstruct`          =ALL=> `reconstruct_internal`
       * `reconstruct_data`     =ALL=> `reconstruct_internal`
       * `reconstruct_opt`      =ALL=> `reconstruct_opt_internal`
       * `reconstruct_data_opt` =ALL=> `reconstruct_opt_internal`
       * `reconstruct_opt_internal` :=
       *   - check length of `shards` matches total shard count exactly
       *   - check at least one option shard is not `None`
       *   - check consistency of length of individual option shards if exist
       * `reconstruct_internal` :=
       *   - check length of `shards` matches total shard count exactly
       *   - check consistency of length of individual shards
       *   - check length of `shard_present` matches length of `shards` *)

      let reconstruct_internal
          (r             : t)
          (shards        : bigstring array)
          (shard_present : bool array)
          (data_only     : bool)
        : (unit, error) result =
        let piece_count_check_result = Checker.check_piece_count r Checker.All shards in
        let shards_check_result      = Checker.check_shards_multi (Bigstr shards) in
        match piece_count_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match shards_check_result with
          | Error _ as e -> e
          | Ok _ ->
            if Array.length shards <> Array.length shard_present then
              Error InvalidShardFlags
            else begin
              (* Quick check: are all of the shards present?  If so, there's
               * nothing to do. *)
              let number_present       = ref 0 in
              let data_present   = ref 0 in
              let parity_present = ref 0 in
              for i = 0 to (Array.length shards) - 1 do
                if shard_present.(i) then (
                  if i < r.data_shard_count then (
                    data_present := !data_present + 1;
                  ) else (
                    parity_present := !parity_present + 1;
                  );
                  number_present := !number_present + 1
                )
              done;
              let number_present = !number_present in
              let data_present   = !data_present in
              let parity_present = !parity_present in
              let number_missing = r.total_shard_count  - number_present in
              let data_missing   = r.data_shard_count   - data_present in
              let parity_missing = r.parity_shard_count - parity_present in

              if number_present = r.total_shard_count then
                (* Cool.  All of the shards are there.  We don't
                 * need to do anything. *)
                Ok ()

              (* More complete sanity check *)
              else if number_present < r.data_shard_count then
                Error TooFewShardsPresent

              else (
                (* Pull out a list holding just the shards that
                 * correspond to the rows of the submatrix.  These shards
                 * will be the input to the decoding process that re-creates
                 * the missing data shards.
                 *
                 * Also, create a list of indices of the valid rows we do have
                 * and the invalid rows we don't have.
                 *
                 * The valid indices are used to construct the data decode matrix,
                 * the invalid indices are used to key the data decode matrix
                 * in the inversion tree.
                 *
                 * We only need exactly N valid indices, where N = `data_shard_count`,
                 * as the data decode matrix is a N x N matrix, thus only needs
                 * N valid indices for determining the N rows to pick from
                 * `self.matrix`. *)
                let sub_shards            = Array.make r.data_shard_count dummy_bigstring in
                let missing_data_shards   = Array.make data_missing       dummy_bigstring in
                let missing_parity_shards = Array.make parity_missing     dummy_bigstring in

                let valid_indices   = Array.make r.data_shard_count 0 in
                let invalid_indices = Array.make number_missing     0 in

                let i_sub_shard            = ref 0 in
                let i_missing_data_shard   = ref 0 in
                let i_missing_parity_shard = ref 0 in
                let i_valid_index          = ref 0 in
                let i_invalid_index        = ref 0 in

                for i = 0 to (Array.length shards) - 1 do
                  let shard = shards.(i) in
                  if shard_present.(i) then (
                    if !i_valid_index < r.data_shard_count then (
                      sub_shards.(!i_sub_shard) <- shard;
                      valid_indices.(!i_valid_index) <- i;

                      i_sub_shard   := !i_sub_shard + 1;
                      i_valid_index := !i_valid_index + 1;
                    ) else (
                      (* Already have enough shards in `sub_shards`
                       * as we only need N shards, where N = `data_shard_count`,
                       * for the data decode matrix
                       *
                       * So nothing to do here *)
                      ()
                    )
                  ) else (
                    if i < r.data_shard_count then (
                      missing_data_shards.(!i_missing_data_shard) <- shard;
                      i_missing_data_shard := !i_missing_data_shard + 1;
                    ) else (
                      missing_parity_shards.(!i_missing_parity_shard) <- shard;
                      i_missing_parity_shard := !i_missing_parity_shard + 1;
                    );
                    invalid_indices.(!i_invalid_index) <- i;
                    i_invalid_index := !i_invalid_index + 1;
                  );
                done;

                let data_decode_matrix =
                  get_data_decode_matrix r valid_indices invalid_indices
                in

                (* Re-create any data shards that were missing.
                 *
                 * The input to the coding is all of the shards we actually
                 * have, and the output is the missing data shards. The computation
                 * is done using the special decode matrix we just built. *)
                let matrix_rows  = Array.make number_missing Bytes.empty in
                let i_matrix_row = ref 0 in

                for i_shard = 0 to (r.data_shard_count) - 1 do
                  if not shard_present.(i_shard) then (
                    matrix_rows.(!i_matrix_row) <-
                      Matrix.get_row data_decode_matrix i_shard;
                    i_matrix_row := !i_matrix_row + 1;
                  )
                done;

                Internal_coder.code_some_shards
                  r
                  matrix_rows
                  sub_shards
                  missing_data_shards;

                if data_only then
                  Ok ()
                else (
                  (* Now that we have all of the data shards intact, we can
                   * compute any of the parity that is missing.
                   *
                   * The input to the coding is ALL of the data shards, including
                   * any that we just calculated.  The output is whichever of the
                   * parity shards were missing. *)
                  let matrix_rows  = Array.make parity_missing Bytes.empty in
                  let i_matrix_row = ref 0 in

                  for i_shard = r.data_shard_count to (r.total_shard_count) - 1 do
                    if not shard_present.(i_shard) then (
                      matrix_rows.(!i_matrix_row) <-
                        r.parity_rows.(i_shard - r.data_shard_count);
                      i_matrix_row := !i_matrix_row + 1;
                    )
                  done;

                  begin
                    (* Gather up all the data shards *)
                    let all_data_shards  = Array.sub shards 0 r.data_shard_count in

                    (* Now do the actual computation for the missing
                     * parity shards *)
                    Internal_coder.code_some_shards
                      r
                      matrix_rows
                      all_data_shards
                      missing_parity_shards
                  end;

                  Ok ()
                )
              )
            end

      let reconstruct_opt_internal
          (r          : t)
          (opt_shards : bigstring option array)
          (data_only  : bool)
        : (unit, error) result =
        let piece_count_check_result = Checker.check_piece_count r Checker.All opt_shards in
        match piece_count_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match Checker.check_option_shards_and_get_size opt_shards with
          | Error _ as e  -> e
          | Ok shard_size ->
            begin
              (* Quick check: are all of the shards present?  If so, there's
               * nothing to do. *)
              let number_present = ref 0 in
              let shard_present  = Array.make (Array.length opt_shards) false in

              for i = 0 to (Array.length opt_shards) - 1 do
                match opt_shards.(i) with
                | None   -> (shard_present.(i) <- false)
                | Some _ -> (number_present := !number_present + 1;
                             shard_present.(i) <- true)
              done;

              if      !number_present = r.total_shard_count then
                (* Cool.  All of the shards data data.  We don't
                 * need to do anything. *)
                Ok ()

              else if !number_present < r.data_shard_count then
                (* Cool.  All of the shards data data.  We don't
                 * need to do anything. *)
                Error TooFewShardsPresent

              else (
                (* Fill in new shards *)
                for i = 0 to (Array.length opt_shards) - 1 do
                  if not shard_present.(i) then
                    opt_shards.(i) <- Some (Bigstring.create shard_size)
                done;

                begin
                  let shards =
                    RS_Shard_utils.option_shards_to_shards_unsafe opt_shards
                  in

                  (* AUDIT
                   *
                   * The above checks cover all the checks done in
                   * `reconstruct_internal` already, so the result
                   * should `Ok _`, thus safe to unwrap *)

                  RS_Error.unwrap
                    (reconstruct_internal
                       r
                       shards
                       shard_present
                       data_only)
                end;

                if data_only then (
                  (* Remove filled in parity shards *)
                  for i = r.data_shard_count to (r.total_shard_count) - 1 do
                    if not shard_present.(i) then
                      opt_shards.(i) <- None
                  done
                );

                Ok ()
              )
            end

      let reconstruct r shards shard_present = reconstruct_internal r shards shard_present false

      let reconstruct_data r shards shard_present = reconstruct_internal r shards shard_present true

      let reconstruct_opt r shards = reconstruct_opt_internal r shards false

      let reconstruct_data_opt r shards = reconstruct_opt_internal r shards true
    end

    module BytesInOut = struct
      def_reconstruct(bytes)

      def_reconstruct_data(bytes)

      def_reconstruct_opt(bytes)

      def_reconstruct_data_opt(bytes)
    end

    module StringInOut = struct
      def_reconstruct(string)

      def_reconstruct_data(string)

      def_reconstruct_opt(string)

      def_reconstruct_data_opt(string)
    end
  end

  let encode_sep_bigstr_no_exn                      = Encode.BigstringInOut.encode_sep
  let encode_single_sep_bigstr_no_exn               = Encode.BigstringInOut.encode_single_sep
  let encode_single_bigstr_no_exn                   = Encode.BigstringInOut.encode_single
  let encode_bigstr_no_exn                          = Encode.BigstringInOut.encode
  let encode_sep_str_no_exn                         = Encode.StringInOut.encode_sep
  let encode_single_sep_str_no_exn                  = Encode.StringInOut.encode_single_sep
  let encode_single_str_no_exn                      = Encode.StringInOut.encode_single
  let encode_str_no_exn                             = Encode.StringInOut.encode
  let encode_sep_bytes_no_exn                       = Encode.BytesInOut.encode_sep
  let encode_single_sep_bytes_no_exn                = Encode.BytesInOut.encode_single_sep
  let encode_single_bytes_no_exn                    = Encode.BytesInOut.encode_single
  let encode_bytes_no_exn                           = Encode.BytesInOut.encode

  let encode_sep_bigstr        r data parity        = RS_Error.unwrap (encode_sep_bigstr_no_exn        r data parity)
  let encode_single_sep_bigstr r i_data data parity = RS_Error.unwrap (encode_single_sep_bigstr_no_exn r i_data data parity)
  let encode_single_bigstr     r i_data shards      = RS_Error.unwrap (encode_single_bigstr_no_exn     r i_data shards)
  let encode_bigstr            r shards             = RS_Error.unwrap (encode_bigstr_no_exn            r shards)
  let encode_sep_str           r data parity        = RS_Error.unwrap (encode_sep_str_no_exn           r data parity)
  let encode_single_sep_str    r i_data data parity = RS_Error.unwrap (encode_single_sep_str_no_exn    r i_data data parity)
  let encode_single_str        r i_data shards      = RS_Error.unwrap (encode_single_str_no_exn        r i_data shards)
  let encode_str               r shards             = RS_Error.unwrap (encode_str_no_exn               r shards)
  let encode_sep_bytes         r data parity        = RS_Error.unwrap (encode_sep_bytes_no_exn         r data parity)
  let encode_single_sep_bytes  r i_data data parity = RS_Error.unwrap (encode_single_sep_bytes_no_exn  r i_data data parity)
  let encode_single_bytes      r i_data shards      = RS_Error.unwrap (encode_single_bytes_no_exn      r i_data shards)
  let encode_bytes             r shards             = RS_Error.unwrap (encode_bytes_no_exn             r shards)

  let verify_bigstr_no_exn                      = Verify.BigstringInOut.verify
  let verify_ret_buffer_bigstr_no_exn           = Verify.BigstringInOut.verify_ret_buffer
  let verify_with_buffer_bigstr_no_exn          = Verify.BigstringInOut.verify_with_buffer
  let verify_str_no_exn                         = Verify.StringInOut.verify
  let verify_ret_buffer_str_no_exn              = Verify.StringInOut.verify_ret_buffer
  let verify_with_buffer_str_no_exn             = Verify.StringInOut.verify_with_buffer
  let verify_bytes_no_exn                       = Verify.BytesInOut.verify
  let verify_ret_buffer_bytes_no_exn            = Verify.BytesInOut.verify_ret_buffer
  let verify_with_buffer_bytes_no_exn           = Verify.BytesInOut.verify_with_buffer

  let verify_bigstr             r shards        = RS_Error.unwrap (verify_bigstr_no_exn             r shards)
  let verify_ret_buffer_bigstr  r shards        = RS_Error.unwrap (verify_ret_buffer_bigstr_no_exn  r shards)
  let verify_with_buffer_bigstr r shards buffer = RS_Error.unwrap (verify_with_buffer_bigstr_no_exn r shards buffer)
  let verify_str                r shards        = RS_Error.unwrap (verify_str_no_exn                r shards)
  let verify_ret_buffer_str     r shards        = RS_Error.unwrap (verify_ret_buffer_str_no_exn     r shards)
  let verify_with_buffer_str    r shards buffer = RS_Error.unwrap (verify_with_buffer_str_no_exn    r shards buffer)
  let verify_bytes              r shards        = RS_Error.unwrap (verify_bytes_no_exn              r shards)
  let verify_ret_buffer_bytes   r shards        = RS_Error.unwrap (verify_ret_buffer_bytes_no_exn   r shards)
  let verify_with_buffer_bytes  r shards buffer = RS_Error.unwrap (verify_with_buffer_bytes_no_exn  r shards buffer)

  let reconstruct_bigstr_no_exn                          = Reconstruct.BigstringInOut.reconstruct
  let reconstruct_data_bigstr_no_exn                     = Reconstruct.BigstringInOut.reconstruct_data
  let reconstruct_opt_bigstr_no_exn                      = Reconstruct.BigstringInOut.reconstruct_opt
  let reconstruct_data_opt_bigstr_no_exn                 = Reconstruct.BigstringInOut.reconstruct_data_opt
  let reconstruct_bytes_no_exn                           = Reconstruct.BytesInOut.reconstruct
  let reconstruct_data_bytes_no_exn                      = Reconstruct.BytesInOut.reconstruct_data
  let reconstruct_opt_bytes_no_exn                       = Reconstruct.BytesInOut.reconstruct_opt
  let reconstruct_data_opt_bytes_no_exn                  = Reconstruct.BytesInOut.reconstruct_data_opt
  let reconstruct_str_no_exn                             = Reconstruct.StringInOut.reconstruct
  let reconstruct_data_str_no_exn                        = Reconstruct.StringInOut.reconstruct_data
  let reconstruct_opt_str_no_exn                         = Reconstruct.StringInOut.reconstruct_opt
  let reconstruct_data_opt_str_no_exn                    = Reconstruct.StringInOut.reconstruct_data_opt

  let reconstruct_data_opt_bigstr r shards               = RS_Error.unwrap (reconstruct_data_opt_bigstr_no_exn r shards)
  let reconstruct_opt_bigstr      r shards               = RS_Error.unwrap (reconstruct_opt_bigstr_no_exn      r shards)
  let reconstruct_data_bigstr     r shards shard_present = RS_Error.unwrap (reconstruct_data_bigstr_no_exn     r shards shard_present)
  let reconstruct_bigstr          r shards shard_present = RS_Error.unwrap (reconstruct_bigstr_no_exn          r shards shard_present)
  let reconstruct_data_opt_str    r shards               = RS_Error.unwrap (reconstruct_data_opt_str_no_exn    r shards)
  let reconstruct_opt_str         r shards               = RS_Error.unwrap (reconstruct_opt_str_no_exn         r shards)
  let reconstruct_data_str        r shards shard_present = RS_Error.unwrap (reconstruct_data_str_no_exn        r shards shard_present)
  let reconstruct_str             r shards shard_present = RS_Error.unwrap (reconstruct_str_no_exn             r shards shard_present)
  let reconstruct_data_opt_bytes  r shards               = RS_Error.unwrap (reconstruct_data_opt_bytes_no_exn  r shards)
  let reconstruct_opt_bytes       r shards               = RS_Error.unwrap (reconstruct_opt_bytes_no_exn       r shards)
  let reconstruct_data_bytes      r shards shard_present = RS_Error.unwrap (reconstruct_data_bytes_no_exn      r shards shard_present)
  let reconstruct_bytes           r shards shard_present = RS_Error.unwrap (reconstruct_bytes_no_exn           r shards shard_present)
end

type reed_solomon = ReedSolomon.t

module ShardByShard = struct
  type t = {
    codec             : reed_solomon;
    mutable cur_input : int;
  }

  let make (codec : reed_solomon) : t =
    { codec;
      cur_input = 0 }

  let parity_ready (sbs : t) : bool =
    sbs.cur_input = sbs.codec.data_shard_count

  let reset_no_exn (sbs : t) : (unit, sbs_error) result =
    if sbs.cur_input > 0 && not (parity_ready sbs) then
      Error LeftoverShards
    else
      Ok (sbs.cur_input <- 0)

  let reset sbs =
    RS_SBS_Error.unwrap (reset_no_exn sbs)

  let reset_force (sbs : t) : unit =
    sbs.cur_input <- 0

  let cur_input_index (sbs : t) : int =
    sbs.cur_input

  let return_ok_and_incre_cur_input (sbs : t) : (unit, sbs_error) result =
    sbs.cur_input <- sbs.cur_input + 1;
    Ok ()

  let sbs_encode_checks
      (sbs    : t)
      (shards : ReedSolomon.Checker.shards_type)
    : (unit, sbs_error) result =
    if parity_ready sbs then
      Error RS_SBS_Error.TooManyCalls
    else (
      match ReedSolomon.Checker.check_piece_count_shards_type sbs.codec ReedSolomon.Checker.All shards with
      | Error e -> Error (RS_SBS_Error.RSError e)
      | Ok _ ->
        match ReedSolomon.Checker.check_shards_multi shards with
        | Error e -> Error (RS_SBS_Error.RSError e)
        | Ok _ -> Ok ()
    )

  let sbs_encode_sep_checks
      (sbs    : t)
      (data   : ReedSolomon.Checker.shards_type)
      (parity : ReedSolomon.Checker.shards_type)
    : (unit, sbs_error) result =
    if parity_ready sbs then
      Error RS_SBS_Error.TooManyCalls
    else (
      match ReedSolomon.Checker.check_piece_count_shards_type sbs.codec ReedSolomon.Checker.Data data with
      | Error e -> Error (RS_SBS_Error.RSError e)
      | Ok _ ->
        match ReedSolomon.Checker.check_piece_count_shards_type sbs.codec ReedSolomon.Checker.Parity parity with
        | Error e -> Error (RS_SBS_Error.RSError e)
        | Ok _ ->
          match ReedSolomon.Checker.check_shards_multi_multi data parity with
          | Error e -> Error (RS_SBS_Error.RSError e)
          | Ok _ -> Ok ()
    )

  module BigstringInOut = struct
    let encode
        (sbs    : t)
        (shards : bigstring array)
      : (unit, sbs_error) result =
      match sbs_encode_checks sbs (Bigstr shards) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_bigstr sbs.codec sbs.cur_input shards;
          return_ok_and_incre_cur_input sbs
        end

    let encode_sep
        (sbs    : t)
        (data   : bigstring array)
        (parity : bigstring array)
      : (unit, sbs_error) result =
      match sbs_encode_sep_checks sbs (Bigstr data) (Bigstr parity) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_sep_bigstr
            sbs.codec
            sbs.cur_input
            data.(sbs.cur_input)
            parity;
          return_ok_and_incre_cur_input sbs
        end
  end

  module StringInOut = struct
    let encode
        (sbs    : t)
        (shards : string array)
      : (unit, sbs_error) result =
      match sbs_encode_checks sbs (Str shards) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_str sbs.codec sbs.cur_input shards;
          return_ok_and_incre_cur_input sbs
        end

    let encode_sep
        (sbs    : t)
        (data   : string array)
        (parity : string array)
      : (unit, sbs_error) result =
      match sbs_encode_sep_checks sbs (Str data) (Str parity) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_sep_str
            sbs.codec
            sbs.cur_input
            data.(sbs.cur_input)
            parity;
          return_ok_and_incre_cur_input sbs
        end
  end

  module BytesInOut = struct
    let encode
        (sbs    : t)
        (shards : bytes array)
      : (unit, sbs_error) result =
      match sbs_encode_checks sbs (Bytes shards) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_bytes sbs.codec sbs.cur_input shards;
          return_ok_and_incre_cur_input sbs
        end

    let encode_sep
        (sbs    : t)
        (data   : bytes array)
        (parity : bytes array)
      : (unit, sbs_error) result =
      match sbs_encode_sep_checks sbs (Bytes data) (Bytes parity) with
      | Error _ as e -> e
      | Ok _ ->
        begin
          ReedSolomon.encode_single_sep_bytes
            sbs.codec
            sbs.cur_input
            data.(sbs.cur_input)
            parity;
          return_ok_and_incre_cur_input sbs
        end
  end

  let encode_bigstr_no_exn     = BigstringInOut.encode
  let encode_sep_bigstr_no_exn = BigstringInOut.encode_sep
  let encode_str_no_exn        = StringInOut.encode
  let encode_sep_str_no_exn    = StringInOut.encode_sep
  let encode_bytes_no_exn      = BytesInOut.encode
  let encode_sep_bytes_no_exn  = BytesInOut.encode_sep

  let encode_bigstr     sbs shards      = RS_SBS_Error.unwrap (BigstringInOut.encode     sbs shards)
  let encode_sep_bigstr sbs data parity = RS_SBS_Error.unwrap (BigstringInOut.encode_sep sbs data parity)
  let encode_str        sbs shards      = RS_SBS_Error.unwrap (StringInOut.encode        sbs shards)
  let encode_sep_str    sbs data parity = RS_SBS_Error.unwrap (StringInOut.encode_sep    sbs data parity)
  let encode_bytes      sbs shards      = RS_SBS_Error.unwrap (BytesInOut.encode         sbs shards)
  let encode_sep_bytes  sbs data parity = RS_SBS_Error.unwrap (BytesInOut.encode_sep     sbs data parity)
end

type shard_by_shard = ShardByShard.t
