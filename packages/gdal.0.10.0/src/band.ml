open Ctypes

type c = T.t
type ('v, 'e) t = c * ('v, 'e) Bigarray.kind
let t = T.t

module Data = struct
  type (_, _) t =
    | Byte : (int, Bigarray.int8_unsigned_elt) t
    | UInt16 : (int, Bigarray.int16_unsigned_elt) t
    | Int16 : (int, Bigarray.int16_signed_elt) t
    | UInt32 : (int32, Bigarray.int32_elt) t
    | Int32 : (int32, Bigarray.int32_elt) t
    | Float32 : (float, Bigarray.float32_elt) t
    | Float64 : (float, Bigarray.float64_elt) t

  let to_int : type v e. (v, e) t -> int = function
    | Byte -> 1
    | UInt16 -> 2
    | Int16 -> 3
    | UInt32 -> 4
    | Int32 -> 5
    | Float32 -> 6
    | Float64 -> 7

  let to_int_opt = function
    | None -> 0
    | Some x -> to_int x

  let of_int = function
    | 1 -> `byte
    | 2 -> `uint16
    | 3 -> `int16
    | 4 -> `uint32
    | 5 -> `int32
    | 6 -> `float32
    | 7 -> `float64
    | 0 -> `unknown
    | _ -> `unhandled

  let is_matching_int kind i =
    to_int kind = i

  let to_ba_kind : type v e. (v, e) t -> (v, e) Bigarray.kind = function
    | Byte -> Bigarray.int8_unsigned
    | UInt16 -> Bigarray.int16_unsigned
    | Int16 -> Bigarray.int16_signed
    | UInt32 -> Bigarray.int32
    | Int32 -> Bigarray.int32
    | Float32 -> Bigarray.float32
    | Float64 -> Bigarray.float64
end

exception IO_error
exception Invalid_dimensions

let err = T.err IO_error

let get_x_size =
  Lib.c "GDALGetRasterBandXSize"
    (t @-> returning int)

let get_x_size (t, _) = get_x_size t

let get_y_size =
  Lib.c "GDALGetRasterBandYSize"
    (t @-> returning int)

let get_y_size (t, _) = get_y_size t

let get_size t =
  get_x_size t, get_y_size t

let get_data_type =
  Lib.c "GDALGetRasterDataType"
    (t @-> returning int)

let check_data_type c kind =
  c <> null && Data.is_matching_int kind (get_data_type c)

let get_data_type c =
  get_data_type c
  |> Data.of_int

let to_ba_kind (_, kind) = kind

let get_band_number =
  Lib.c "GDALGetBandNumber"
    (t @-> returning int)

let get_band_number (t, _) =
  match get_band_number t with
  | 0 -> None
  | i -> Some i

let io =
  Lib.c "GDALRasterIO" (
    t @->
    int @->
    int @-> int @-> int @-> int @->
    ptr void @->
    int @-> int @->
    int @->
    int @-> int @->
    returning err
  )

let io
    ?(write : ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t option)
    ?(offset = 0, 0)
    ?size
    ?(pixel_spacing = 0)
    ?(line_spacing = 0)
    ?buffer_size
    ((c, _) as t)
    (kind : ('v, 'e) Data.t)
  =
  let size =
    match size with
    | None -> get_size t
    | Some s -> s
  in
  let buffer_cols, buffer_rows =
    let cols, rows =
      match buffer_size with
      | None -> size
      | Some s -> s
    in
    match write with
    | None -> cols, rows
    | Some a ->
      if
        rows = Bigarray.Array2.dim1 a &&
        cols = Bigarray.Array2.dim2 a
      then
        cols, rows
      else
        raise Invalid_dimensions
  in
  let ba =
    match write with
    | None ->
      let open Bigarray in
      Array2.create (Data.to_ba_kind kind) c_layout buffer_rows buffer_cols
    | Some buffer -> buffer
  in
  let ba_ptr = bigarray_start array2 ba in
  io
    c
    (match write with None -> 0 | Some _ -> 1)
    (fst offset)
    (snd offset)
    (fst size)
    (snd size)
    (to_voidp ba_ptr)
    buffer_cols
    buffer_rows
    (Data.to_int kind)
    pixel_spacing
    line_spacing;
  ba

let read ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind =
  io ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind

let write ?offset ?size ?pixel_spacing ?line_spacing t kind data =
  ignore (io ~write:data ?offset ?size ?pixel_spacing ?line_spacing t kind)

let fill =
  Lib.c "GDALFillRaster"
    (t @-> double @-> double @-> returning err)

let fill ?(imaginary = 0.0) (t, _) real =
  fill t real imaginary

let get_description =
  Lib.c "GDALGetDescription"
    (t @-> returning string)

let get_description (t, _) = get_description t

let get_no_data_value =
  Lib.c "GDALGetRasterNoDataValue"
    (t @-> ptr int @-> returning double)

let get_no_data_value (t, _) =
  let ok = allocate int 0 in
  let result = get_no_data_value t ok in
  if to_voidp ok = null || !@ok = 1 then
    Some result
  else
    None

let set_description =
  Lib.c "GDALSetDescription"
    (t @-> string @-> returning void)

let set_description (t, _) s = set_description t s

let set_no_data_value =
  Lib.c "GDALSetRasterNoDataValue"
    (t @-> double @-> returning err)

let set_no_data_value (t, _) x =
  set_no_data_value t x

let copy =
  Lib.c "GDALRasterBandCopyWholeRaster"
    (t @-> t @-> ptr string_opt @-> ptr void @-> ptr void @-> returning err)

let copy ?(options = []) ~src:(s, _) ~dst:(d, _) =
  let options = Lib.convert_creation_options options in
  copy s d (Lib.creation_options_to_ptr options) null null

module Block = struct
  exception Wrong_dimensions

  type offset_t = {
    block : int * int;
    offset : int * int;
  }

  let make_block_offset ~block ~offset =
    { block; offset }

  let get_band_size = get_size

  let get_size =
    Lib.c "GDALGetBlockSize"
      (t @-> ptr int @-> ptr int @-> returning void)

  let get_size (t, _) =
    let cols = allocate int 0 in
    let rows = allocate int 0 in
    get_size t cols rows;
    !@cols, !@rows

  let pixel_of_block_offset t =
    let block_cols, block_rows = get_size t in
    fun { block = (block_col, block_row); offset = (off_col, off_row) } ->
      let pixel_col = block_col * block_cols + off_col in
      let pixel_row = block_row * block_rows + off_row in
      pixel_col, pixel_row

  let block_of_pixel_offset t =
    let block_cols, block_rows = get_size t in
    fun pixel_col pixel_row ->
      let block_col = pixel_col / block_cols in
      let block_row = pixel_row / block_rows in
      let offset_col = pixel_col mod block_cols in
      let offset_row = pixel_row mod block_rows in
      {
        block = block_col, block_row;
        offset = offset_col, offset_row;
      }

  let get_block_count t =
    let band_cols, band_rows = get_band_size t in
    let block_cols, block_rows = get_size t in
    (band_cols + block_cols - 1) / block_cols,
    (band_rows + block_rows - 1) / block_rows

  let read =
    Lib.c "GDALReadBlock"
      (t @-> int @-> int @-> ptr void @-> returning err)

  let read ?data ((c, k) as t) ~column ~row =
    let columns, rows = get_size t in
    let ba =
      match data with
      | None -> Bigarray.(Array2.create k c_layout rows columns)
      | Some ba ->
        if
          columns = Bigarray.Array2.dim2 ba &&
          rows = Bigarray.Array2.dim1 ba
        then
          ba
        else
          raise Wrong_dimensions
    in
    let ba_ptr = bigarray_start array2 ba in
    read c column row (to_voidp ba_ptr);
    ba

  let write =
    Lib.c "GDALWriteBlock"
      (t @-> int @-> int @-> ptr void @-> returning err)

  let write (t, _) ~column ~row data =
    let data_ptr = bigarray_start array2 data in
    write t column row (to_voidp data_ptr)

  let read' = read
  let write' = write

  let iter ((_c, k) as t) ~read ~write f =
    let block_cols, block_rows = get_block_count t in
    let pixel_cols, pixel_rows = get_size t in
    let band_cols, band_rows = get_band_size t in
    let data = Bigarray.(Array2.create k c_layout pixel_rows pixel_cols) in
    for block_row = 0 to block_rows - 1 do
      for block_col = 0 to block_cols - 1 do
        let data =
          if read then read' ~data t ~column:block_col ~row:block_row
          else data
        in
        let valid, valid_cols, valid_rows =
          if
            (block_row + 1) * pixel_rows > band_rows ||
            (block_col + 1) * pixel_cols > band_cols
          then (
            (* Only pass valid data to f *)
            let valid_cols =
              min (band_cols - block_col * pixel_cols) pixel_cols
            in
            let valid_rows =
              min (band_rows - block_row * pixel_rows) pixel_rows
            in
            let valid =
              Bigarray.Array2.create k Bigarray.c_layout
                valid_rows valid_cols
            in
            begin
              if read then (
                for r = 0 to valid_rows - 1 do
                  for c = 0 to valid_cols - 1 do
                    valid.{r, c} <- data.{r, c}
                  done;
                done;
              )
              else (
              )
            end;
            valid, valid_cols, valid_rows
          )
          else
            data, pixel_cols, pixel_rows
        in
        f block_col block_row valid;
        if write then (
          (* If we created a fresh bigarray copy the values back to data for
             writing. Physical equality is intentional here. *)
          if valid != data then (
            for r = 0 to valid_rows - 1 do
              for c = 0 to valid_cols - 1 do
                data.{r, c} <- valid.{r, c}
              done;
            done;
          );
          write' t ~column:block_col ~row:block_row data;
        );
      done;
    done;
    ()

  let iter_read t f = iter ~read:true ~write:false t f
  let iter_write t f = iter ~read:false ~write:true t f
end

let iter t f =
  let block_cols, block_rows = Block.get_size t in
  Block.iter t ~read:true ~write:true (
    fun block_col block_row data ->
      let open Bigarray in
      let pixel_rows = Array2.dim1 data in
      let pixel_cols = Array2.dim2 data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let v = data.{pixel_row, pixel_col} in
          let result = f col row v in
          data.{pixel_row, pixel_col} <- result;
        done;
      done;
      ()
  )

(* [check src dst] returns [true] if the dimensions and block dimensions of
   each band in [src] matches those in [dst]. *)
let check src dst =
  let block_size = Block.get_size dst in
  let band_size = get_size dst in
  Array.fold_right (
    fun band ok ->
      ok &&
      Block.get_size band = block_size &&
      get_size band = band_size
  ) src true

let itera src dst f =
  if not (check src dst) then invalid_arg "Band.itera";
  let block_cols, block_rows = Block.get_size dst in
  Block.iter dst ~read:true ~write:true (
    fun block_col block_row dst_data ->
      let src_data =
        Array.map (
          fun t ->
            Block.read t ~column:block_col ~row:block_row
        ) src
      in
      let open Bigarray in
      let pixel_rows = Array2.dim1 dst_data in
      let pixel_cols = Array2.dim2 dst_data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let dst_v = dst_data.{pixel_row, pixel_col} in
          let src_v = Array.map (fun a -> a.{pixel_row, pixel_col}) src_data in
          let result = f col row src_v dst_v in
          dst_data.{pixel_row, pixel_col} <- result;
        done;
      done;
      ()
  )

let itera_read src dst f =
  if not (check src dst) then invalid_arg "Band.itera_read";
  let block_cols, block_rows = Block.get_size dst in
  Block.iter dst ~read:true ~write:false (
    fun block_col block_row dst_data ->
      let src_data =
        Array.map (
          fun t ->
            Block.read t ~column:block_col ~row:block_row
        ) src
      in
      let open Bigarray in
      let pixel_rows = Array2.dim1 dst_data in
      let pixel_cols = Array2.dim2 dst_data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let dst_v = dst_data.{pixel_row, pixel_col} in
          let src_v = Array.map (fun a -> a.{pixel_row, pixel_col}) src_data in
          f col row src_v dst_v;
        done;
      done;
      ()
  )

let itera_write src dst f =
  if not (check src dst) then invalid_arg "Band.itera_write";
  let block_cols, block_rows = Block.get_size dst in
  Block.iter dst ~read:false ~write:true (
    fun block_col block_row dst_data ->
      let src_data =
        Array.map (
          fun t ->
            Block.read t ~column:block_col ~row:block_row
        ) src
      in
      let open Bigarray in
      let pixel_rows = Array2.dim1 dst_data in
      let pixel_cols = Array2.dim2 dst_data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let src_v = Array.map (fun a -> a.{pixel_row, pixel_col}) src_data in
          let result = f col row src_v in
          dst_data.{pixel_row, pixel_col} <- result;
        done;
      done;
      ()
  )

let iter_read t f =
  let block_cols, block_rows = Block.get_size t in
  Block.iter t ~read:true ~write:false (
    fun block_col block_row data ->
      let open Bigarray in
      let pixel_rows = Array2.dim1 data in
      let pixel_cols = Array2.dim2 data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let v = data.{pixel_row, pixel_col} in
          f col row v;
        done;
      done;
      ()
  )

let iter_write t f =
  let block_cols, block_rows = Block.get_size t in
  Block.iter t ~read:false ~write:true (
    fun block_col block_row data ->
      let open Bigarray in
      let pixel_rows = Array2.dim1 data in
      let pixel_cols = Array2.dim2 data in
      for pixel_row = 0 to pixel_rows - 1 do
        for pixel_col = 0 to pixel_cols - 1 do
          let col = block_col * block_cols + pixel_col in
          let row = block_row * block_rows + pixel_row in
          let result = f col row in
          data.{pixel_row, pixel_col} <- result;
        done;
      done;
      ()
  )

let fold t f init =
  let accu = ref init in
  iter_read t (
    fun col row v ->
      accu := f col row v !accu
  );
  !accu
