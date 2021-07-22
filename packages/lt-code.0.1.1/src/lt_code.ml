module Param = Param
module Drop_set = Drop_set

let get_data_block_indices_into (param : Param.t) (drop : Drop.t)
    (into : int array) : unit =
  let systematic = Param.systematic param in
  let data_block_count = Param.data_block_count param in
  let rec aux rng degree set =
    if Hash_int_set.cardinal set < degree then (
      Hash_int_set.add set (Rand.gen_int_bounded rng);
      aux rng degree set)
  in
  let degree = Drop.degree drop in
  let drop_index = Drop.index drop in
  if systematic && drop_index < data_block_count then (
    assert (degree = 1);
    into.(0) <- drop_index)
  else
    let set = Hash_int_set.create degree in
    let rng =
      Rand.create_bounded_rng ~bound:data_block_count (Drop.index drop)
    in
    aux rng degree set;
    let c = ref 0 in
    Hash_int_set.iter
      (fun i ->
        into.(!c) <- i;
        c := !c + 1)
      set

let get_data_block_indices (param : Param.t) (drop : Drop.t) : int array =
  let arr = Array.make (Drop.degree drop) 0 in
  get_data_block_indices_into param drop arr;
  arr

module Encode = struct
  type error =
    [ `Inconsistent_data_block_size
    | `Invalid_drop_count
    | `Invalid_data_block_count
    | `Invalid_systematic_scaling_factor
    | `Invalid_drop_data_buffer
    ]

  let gen_degrees_into (param : Param.t) into : unit =
    let data_block_count = Param.data_block_count param in
    let max_drop_count = Param.max_drop_count param in
    assert (Array.length into = max_drop_count);
    let systematic = Param.systematic param in
    if systematic then (
      for i = 0 to data_block_count - 1 do
        into.(i) <- 1
      done;
      let n = max_drop_count - data_block_count in
      if n > 0 then (
        Dist.choose_into ~offset:data_block_count (Param.dist param) into;
        (* we amplify the coverage of the parity drops *)
        let multiplier =
          int_of_float
          @@ Float.round
               (Param.systematic_scaling_factor param
               *. float_of_int data_block_count
               /. float_of_int n)
        in
        for i = data_block_count to max_drop_count - 1 do
          into.(i) <- max 1 (min data_block_count (into.(i) * multiplier))
        done))
    else Dist.choose_into (Param.dist param) into;
    (* fix a random drop to be degree 1 to ensure decoding is at least possible *)
    if not systematic then into.(Rand.gen_int_global max_drop_count) <- 1

  let gen_degrees (param : Param.t) : int array =
    let max_drop_count = Param.max_drop_count param in
    let arr = Array.make max_drop_count 0 in
    gen_degrees_into param arr;
    arr

  type encoder = {
    param : Param.t;
    mutable degrees : int array;
    data_blocks : Cstruct.t array;
    drop_data_buffer : Cstruct.t array;
    mutable cur_drop_index : int;
  }

  let create_encoder ?(drop_data_buffer : Cstruct.t array option)
      (param : Param.t) data_blocks : (encoder, error) result =
    if Array.length data_blocks <> Param.data_block_count param then
      Error `Invalid_data_block_count
    else
      let data_block_len = Cstruct.length data_blocks.(0) in
      if not (Utils.cstruct_array_is_consistent data_blocks) then
        Error `Inconsistent_data_block_size
      else
        let drop_count = Param.max_drop_count param in
        let degrees = gen_degrees param in
        let drop_data_buffer =
          match drop_data_buffer with
          | None ->
              Ok
                (Array.init drop_count (fun _ -> Cstruct.create data_block_len))
          | Some buffer ->
              if
                Array.length buffer = Param.max_drop_count param
                && Cstruct.length buffer.(0) = Cstruct.length data_blocks.(0)
                && Utils.cstruct_array_is_consistent buffer
              then (
                Utils.zero_cstruct_array buffer;
                Ok buffer)
              else Error `Invalid_drop_data_buffer
        in
        match drop_data_buffer with
        | Error e -> Error e
        | Ok drop_data_buffer ->
            Ok
              {
                param;
                degrees;
                drop_data_buffer;
                data_blocks;
                cur_drop_index = 0;
              }

  let reset_encoder (encoder : encoder) : unit =
    encoder.cur_drop_index <- 0;
    gen_degrees_into encoder.param encoder.degrees

  let encode_one (encoder : encoder) : Drop.t option =
    let drop_count = Param.max_drop_count encoder.param in
    let index = encoder.cur_drop_index in
    if index < drop_count then (
      let degree = encoder.degrees.(index) in
      let drop_data = encoder.drop_data_buffer.(index) in
      let drop = Drop.make_exn ~index ~degree ~data:drop_data in
      let data_indices = get_data_block_indices encoder.param drop in
      if Array.length data_indices = 1 then
        let data_index = data_indices.(0) in
        Utils.memcpy ~src:encoder.data_blocks.(data_index) ~dst:drop_data
      else (
        Utils.zero_cstruct drop_data;
        Array.iter
          (fun i -> Utils.xor_into ~src:encoder.data_blocks.(i) ~into:drop_data)
          data_indices);
      encoder.cur_drop_index <- encoder.cur_drop_index + 1;
      Some drop)
    else None

  let encode_all (encoder : encoder) : Drop.t array =
    let drops_left =
      Param.max_drop_count encoder.param - encoder.cur_drop_index
    in
    Array.init drops_left (fun _ -> Option.get @@ encode_one encoder)

  let encode ?systematic_scaling_factor ?(systematic = true) ?drop_data_buffer
      ~max_drop_count (data_blocks : Cstruct.t array) :
      (Param.t * Drop.t array, error) result =
    match
      Param.make ?systematic_scaling_factor ~systematic
        ~data_block_count:(Array.length data_blocks) ~max_drop_count ()
    with
    | Error e -> (
        match e with
        | ( `Invalid_data_block_count | `Invalid_drop_count
          | `Invalid_systematic_scaling_factor ) as e ->
            Error (e :> error))
    | Ok param -> (
        match create_encoder ?drop_data_buffer param data_blocks with
        | Error e -> Error (e :> error)
        | Ok encoder ->
            let arr =
              Array.init max_drop_count (fun _ ->
                  Option.get @@ encode_one encoder)
            in
            Ok (param, arr))
end

module Decode = struct
  type error =
    [ `Invalid_drop_index
    | `Invalid_drop_degree
    | `Invalid_drop_count
    | `Invalid_data_block_buffer
    | `Invalid_data_block_size
    | `Invalid_drop_size
    | `Cannot_recover
    ]

  module Graph = struct
    type bucket = Hash_int_set.t

    type t = {
      param : Param.t;
      data_block_is_solved : bool array;
      mutable data_block_solved_count : int;
      mutable drop_fill_count : int;
      data_edges : bucket array;
      drop_edges : bucket array;
    }

    let create param =
      let data_block_count = Param.data_block_count param in
      let max_drop_count = Param.max_drop_count param in
      {
        param;
        data_block_is_solved = Array.make data_block_count false;
        data_block_solved_count = 0;
        drop_fill_count = 0;
        data_edges =
          Array.init data_block_count (fun _ ->
              Hash_int_set.create (max 1 (max_drop_count / 10)));
        drop_edges =
          Array.init max_drop_count (fun _ ->
              Hash_int_set.create (max 1 (data_block_count / 10)));
      }

    let reset (g : t) : unit =
      Utils.fill_array false g.data_block_is_solved;
      g.data_block_solved_count <- 0;
      g.drop_fill_count <- 0;
      Array.iter Hash_int_set.reset g.data_edges;
      Array.iter Hash_int_set.reset g.drop_edges

    let remove_edge ~data_index ~drop_index (g : t) : unit =
      Hash_int_set.remove g.drop_edges.(drop_index) data_index;
      Hash_int_set.remove g.data_edges.(data_index) drop_index

    let add_drop (drop : Drop.t) (g : t) : unit =
      let drop_index = Drop.index drop in
      let data_indices = get_data_block_indices g.param drop in
      Array.iter
        (fun data_index ->
          Hash_int_set.add g.drop_edges.(drop_index) data_index)
        data_indices;
      Array.iter
        (fun data_index ->
          Hash_int_set.add g.data_edges.(data_index) drop_index)
        data_indices;
      g.drop_fill_count <- g.drop_fill_count + 1

    let mark_data_as_solved ~data_index (g : t) : unit =
      g.data_block_is_solved.(data_index) <- true;
      g.data_block_solved_count <- g.data_block_solved_count + 1

    let degree_of_drop ~drop_index (g : t) : int =
      Hash_int_set.cardinal g.drop_edges.(drop_index)
  end

  type decoder = {
    param : Param.t;
    graph : Graph.t;
    data_block_size : int;
    data_blocks : Cstruct.t array;
    drops : Cstruct.t option array;
  }

  let create_decoder ?data_block_buffer ~data_block_size param :
      (decoder, error) result =
    let data_block_count = Param.data_block_count param in
    if data_block_size < 0 then Error `Invalid_data_block_size
    else
      let data_blocks =
        match data_block_buffer with
        | None ->
            Ok
              (Array.init data_block_count (fun _ ->
                   Cstruct.create data_block_size))
        | Some buffer ->
            if
              Array.length buffer = data_block_count
              && Cstruct.length buffer.(0) = data_block_size
              && Utils.cstruct_array_is_consistent buffer
            then (
              Utils.zero_cstruct_array buffer;
              Ok buffer)
            else Error `Invalid_data_block_buffer
      in
      match data_blocks with
      | Error e -> Error e
      | Ok data_blocks ->
          Ok
            {
              param;
              graph = Graph.create param;
              data_block_size;
              data_blocks;
              drops = Array.make (Param.max_drop_count param) None;
            }

  let reset_decoder (decoder : decoder) : unit =
    Graph.reset decoder.graph;
    Utils.zero_cstruct_array decoder.data_blocks;
    Utils.fill_array None decoder.drops

  let add_drop (drop : Drop.t) (decoder : decoder) : unit =
    decoder.drops.(Drop.index drop) <- Some (Drop.data drop);
    Graph.add_drop drop decoder.graph

  let remove_solved_drop_edges ~drop_index (decoder : decoder) : unit =
    (* this essentially catches up the missed data propagation *)
    let data_indices = decoder.graph.drop_edges.(drop_index) in
    Hash_int_set.iter
      (fun data_index ->
        if decoder.graph.data_block_is_solved.(data_index) then (
          Utils.xor_into
            ~src:decoder.data_blocks.(data_index)
            ~into:(Option.get decoder.drops.(drop_index));
          Graph.remove_edge ~data_index ~drop_index decoder.graph))
      data_indices

  let propagate_data_xor ~data_index (decoder : decoder) : unit =
    let data = decoder.data_blocks.(data_index) in
    Hash_int_set.iter
      (fun drop_index ->
        Option.iter
          (fun into ->
            Utils.xor_into ~src:data ~into;
            Graph.remove_edge ~data_index ~drop_index decoder.graph)
          decoder.drops.(drop_index))
      decoder.graph.data_edges.(data_index)

  type reduction_one_step_status =
    [ `Success
    | `Ongoing
    | `Need_more_drops
    ]

  let reduce_one_step (decoder : decoder) : reduction_one_step_status =
    let new_usable_degree_1_found =
      CCArray.foldi
        (fun new_usable_degree_1_found drop_index drop ->
          match drop with
          | None -> new_usable_degree_1_found
          | Some drop_data ->
              if Graph.degree_of_drop ~drop_index decoder.graph = 1 then (
                let data_index =
                  Hash_int_set.choose decoder.graph.drop_edges.(drop_index)
                in
                Graph.remove_edge ~data_index ~drop_index decoder.graph;
                if not decoder.graph.data_block_is_solved.(data_index) then (
                  Utils.memcpy ~src:drop_data
                    ~dst:decoder.data_blocks.(data_index);
                  Graph.mark_data_as_solved ~data_index decoder.graph;
                  propagate_data_xor ~data_index decoder;
                  true)
                else new_usable_degree_1_found)
              else new_usable_degree_1_found)
        false decoder.drops
    in
    if new_usable_degree_1_found then `Ongoing
    else if
      decoder.graph.data_block_solved_count
      = Param.data_block_count decoder.param
    then `Success
    else `Need_more_drops

  type reduction_status =
    [ `Success
    | `Need_more_drops
    ]

  let reduce (decoder : decoder) : reduction_status =
    let rec aux () =
      match reduce_one_step decoder with
      | (`Success | `Need_more_drops) as s -> (s :> reduction_status)
      | `Ongoing -> aux ()
    in
    aux ()

  type status =
    [ `Success of Cstruct.t array
    | `Ongoing
    ]

  let data_is_ready (decoder : decoder) : bool =
    decoder.graph.data_block_solved_count = Param.data_block_count decoder.param

  let max_tries_reached (decoder : decoder) : bool =
    decoder.graph.drop_fill_count = Param.max_drop_count decoder.param

  let decode_one (decoder : decoder) (drop : Drop.t) : (status, error) result =
    if Cstruct.length (Drop.data drop) <> decoder.data_block_size then
      Error `Invalid_drop_size
    else
      let drop_index = Drop.index drop in
      if drop_index >= Param.max_drop_count decoder.param then
        Error `Invalid_drop_index
      else if Drop.degree drop > Param.data_block_count decoder.param then
        Error `Invalid_drop_degree
      else if data_is_ready decoder then Ok (`Success decoder.data_blocks)
      else if max_tries_reached decoder then Error `Cannot_recover
      else
        match decoder.drops.(drop_index) with
        | Some _ -> Ok `Ongoing
        | None -> (
            add_drop drop decoder;
            remove_solved_drop_edges ~drop_index decoder;
            match reduce decoder with
            | `Success -> Ok (`Success decoder.data_blocks)
            | `Need_more_drops ->
                if max_tries_reached decoder then Error `Cannot_recover
                else Ok `Ongoing)

  let decode_all (decoder : decoder) (drops : Drop_set.t) :
      (Cstruct.t array, error) result =
    let rec aux decoder drops =
      let x = Drop_set.choose drops in
      let drops = Drop_set.remove x drops in
      match decode_one decoder x with
      | Error e -> Error e
      | Ok `Ongoing ->
          if max_tries_reached decoder then Error `Cannot_recover
          else aux decoder drops
      | Ok (`Success arr) -> Ok arr
    in
    aux decoder drops

  let decode ?data_block_buffer (param : Param.t) (drops : Drop_set.t) :
      (Cstruct.t array, error) result =
    if Drop_set.cardinal drops = 0 then Error `Cannot_recover
    else
      match
        create_decoder
          ~data_block_size:(Cstruct.length @@ Drop.data @@ Drop_set.choose drops)
          ?data_block_buffer param
      with
      | Error e -> Error e
      | Ok decoder -> decode_all decoder drops
end

let max_drop_count = Constants.max_drop_count

let max_data_block_count = Constants.max_data_block_count

type drop = Drop.t

let data_of_drop = Drop.data

type encode_error = Encode.error

type encoder = Encode.encoder

let create_encoder = Encode.create_encoder

let reset_encoder = Encode.reset_encoder

let param_of_encoder (encoder : Encode.encoder) = encoder.param

let encoder_is_systematic (encoder : Encode.encoder) =
  Param.systematic encoder.param

let data_block_count_of_encoder (encoder : Encode.encoder) =
  Param.data_block_count encoder.param

let max_drop_count_of_encoder (encoder : Encode.encoder) =
  Param.max_drop_count encoder.param

let data_block_size_of_encoder (encoder : Encode.encoder) =
  Cstruct.length encoder.data_blocks.(0)

let data_blocks_of_encoder (encoder : Encode.encoder) = encoder.data_blocks

let encode_one = Encode.encode_one

let encode_all = Encode.encode_all

let encode = Encode.encode

type decode_error = Decode.error

type decoder = Decode.decoder

type decode_status = Decode.status

let create_decoder = Decode.create_decoder

let reset_decoder = Decode.reset_decoder

let param_of_decoder (decoder : Decode.decoder) = decoder.param

let decoder_is_systematic (decoder : Decode.decoder) =
  Param.systematic decoder.param

let data_block_count_of_decoder (decoder : Decode.decoder) =
  Param.data_block_count decoder.param

let max_drop_count_of_decoder (decoder : Decode.decoder) =
  Param.max_drop_count decoder.param

let data_block_size_of_decoder (decoder : Decode.decoder) =
  decoder.data_block_size

let drop_fill_count_of_decoder (decoder : Decode.decoder) =
  decoder.graph.drop_fill_count

let data_blocks_of_decoder (decoder : Decode.decoder) : Cstruct.t array option =
  if Decode.data_is_ready decoder then Some decoder.data_blocks else None

let decode_one = Decode.decode_one

let decode_all = Decode.decode_all

let decode = Decode.decode
