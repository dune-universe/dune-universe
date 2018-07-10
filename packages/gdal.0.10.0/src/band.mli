(** {1 Raster Bands} *)

type c
type ('v, 'e) t = c * ('v, 'e) Bigarray.kind
val t : c Ctypes.typ

val to_ba_kind : ('v, 'e) t -> ('v, 'e) Bigarray.kind
(** [to_ba_kind t] returns the {!Bigarray.kind} matching [t]. *)

module Data : sig
  type ('v, 'e) t =
    | Byte : (int, Bigarray.int8_unsigned_elt) t
    | UInt16 : (int, Bigarray.int16_unsigned_elt) t
    | Int16 : (int, Bigarray.int16_signed_elt) t
    | UInt32 : (int32, Bigarray.int32_elt) t
    | Int32 : (int32, Bigarray.int32_elt) t
    | Float32 : (float, Bigarray.float32_elt) t
    | Float64 : (float, Bigarray.float64_elt) t

  val to_ba_kind : ('v, 'e) t -> ('v, 'e) Bigarray.kind
  (** [to_ba_kind t] returns the {!Bigarray.kind} matching [t]. *)

  (**/**)
  val to_int : (_, _) t -> int
  val to_int_opt : (_, _) t option -> int
  (**/**)
end

exception IO_error
exception Invalid_dimensions

val get_size : (_, _) t -> int * int
(** [get_size t] returns the [(x, y)] dimensions in pixels. *)

val get_data_type : c -> [
    `byte
  | `uint16
  | `int16
  | `uint32
  | `int32
  | `float32
  | `float64
  | `unknown
  | `unhandled
  ]
(** [get_data_type t] returns the data type of the given raster band.

    @return `unknown if GDAL does not know the data type
    @return `unhandled if the data type is recognized by GDAL but unhandled by
    the OCaml bindings. *)

val get_band_number : (_, _) t -> int option
(** [get_band_number t] returns the index of [t] in its dataset or [None] if
    the index is unknown. *)

val read :
  ?offset:int * int ->
  ?size:int * int ->
  ?pixel_spacing:int ->
  ?line_spacing:int ->
  ?buffer_size:int * int ->
  (_, _) t -> ('v, 'e) Data.t -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
(** [read t kind] reads the values from [t] as [kind] values. *)

val write :
  ?offset:int * int ->
  ?size:int * int ->
  ?pixel_spacing:int ->
  ?line_spacing:int ->
  (_, _) t -> ('v, 'e) Data.t ->
  ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t ->
  unit
(** [write t kind data] writes the values from [t] as [kind] values. *)

val fill : ?imaginary:float -> (_, _) t -> float -> unit
(** [file ?imaginary t real] sets all values in [t] to [real].  If [t] holds
    complex numbers then [imaginary] can be specified as well.

    @param imaginary defaults to [0.0]. *)

val get_description : (_, _) t -> string
(** [get_description t] returns the description of the current band.  If no
    description exists then the returned string will be empty. *)

val get_no_data_value : (_, _) t -> float option
(** [get_no_data_value t] returns the value representing no/missing data in
    band [t], or [None] if no such value is set. *)

val set_description : (_, _) t -> string -> unit
(** [set_description t desc] sets the description of [t] to [desc]. *)

val set_no_data_value : (_, _) t -> float -> unit
(** [set_no_data_value t x] defines [x] as representing no/missing data in
    band [t]. *)

val iter : ('v, _) t -> (int -> int -> 'v -> 'v) -> unit
(** [iter t f] applies [f] to every value in [t] then assigns the result back
    to the same point in [t].

    @param f gets three arguments: [column], [row] and the [v]alue at this
    location in the band. *)

val iter_read : ('v, _) t -> (int -> int -> 'v -> unit) -> unit
(** [iter_read t f] calls [f] with every value in [t].

    @param f gets three arguments: [column], [row] for the pixel offset within
    the band and the [v]alue at this offset. *)

val iter_write : ('v, _) t -> (int -> int -> 'v) -> unit
(** [iter_write t f] calls [f] with every pixel offset in [t].  The result of
    [f] is written back to [t] at the current offset.

    @param f gets three arguments: [column], [row] for the pixel offset within the
    band. *)

val itera :
  ('v1, _) t array -> ('v2, _) t ->
  (int -> int -> 'v1 array -> 'v2 -> 'v2) ->
  unit
(** [itera src dst f] acts like {!iter} except that it applies [f] to matching
    pixels in each raster in [src] and [dst], writing the result of [f] back to
    [dst].  All bands in [src] and [dst] must have the same overall dimensions
    and the same block size.

    @param f gets three arguments: [column], [row] and the [src] value and
    [dst] values at this location in the bands. *)

val itera_read :
  ('v1, _) t array -> ('v2, _) t ->
  (int -> int -> 'v1 array -> 'v2 -> unit) ->
  unit
(** [itera_read src dst f] is like {!itera} except that no values are written
    back to [dst]. *)

val itera_write :
  ('v1, _) t array -> ('v2, _) t ->
  (int -> int -> 'v1 array -> 'v2) ->
  unit
(** [itera_write t f] is like {!itera} except that no values are read from
    [dst]. *)

val fold : ('v, _) t -> (int -> int -> 'v -> 'accu -> 'accu) -> 'accu -> 'accu
(** [fold t f init] folds over the pixels in [t] with [f]. *)

val copy : ?options:string list -> src:(_, _) t -> dst:(_, _) t -> unit
(** [copy ?options ~src ~dst] copies the contents of [src] to [dst].  GDAL will
    perform any data conversion necessary. *)

module Block : sig
  (** {2 Block IO} *)

  (** These functions should generally be faster than the more generic {!read}
      and {!write} functions. *)

  exception Wrong_dimensions

  type offset_t = {
    block : int * int;
    offset : int * int;
  }

  val make_block_offset : block:int * int -> offset:int * int -> offset_t

  val pixel_of_block_offset : (_, _) t -> (offset_t -> int * int)
  (** [pixel_of_block_offset t] returns a function which may be used to convert
      offsets within a block to pixel offsets within the band [t].

      @return f: A function which, given a block index [block] and an offset
      within that block [offset] returns the index of the matching pixel. *)

  val block_of_pixel_offset : (_, _) t -> (int -> int -> offset_t)
  (** [block_of_pixel_offset t] returns a function which returns the
      {!block_offset_t} matching the pixel offset provided. *)

  val get_block_count : (_, _) t -> int * int
  (** [get_block_count t] returns [(nx, ny)] giving the number of blocks in the
      band's x direction ([nx]) and the number of blocks in the band's y
      direction ([ny]). *)

  val get_size : (_, _) t -> int * int
  (** [get_size t] returns the native [(x, y)] dimensions of the individual
      blocks making up [t]. *)

  val read :
    ?data:('v, 'e, Bigarray.c_layout) Bigarray.Array2.t ->
    ('v, 'e) t -> column:int -> row:int ->
    ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
  (** [read ?data t ~column ~row] returns the block at given offset in [t].

      @param data will be written to and returned if it is provided, otherwise
             a fresh {!Bigarray.Array2.t} will be allocated.  [data] must be
             large enough to hold at least on block of values.
      @raise Wrong_dimensions if [data] is provided and does not have enough
             elements to hold a block. *)

  val write : ('v, 'e) t -> column:int -> row:int ->
    ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t -> unit
  (** [write t ~column ~row data] writes [data] to the block at the given offset
      in [t]. *)

  val iter :
    ('v, 'e) t ->
    read:bool -> write:bool ->
    (int -> int -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t -> unit) ->
    unit
  (** [iter t ~read ~write f] applies [f] to each block in [t].  The [read]
      and [write] arguments determine if values are from [t], written to [t] or
      both.

      @param f gets three arguments: the [i] index, [j] index and a
      bigarray with the contents of the current block.  If [read] is [true]
      then the bigarray will contain the current contents of the block at
      [(i, j)].  If [write] is [true] then the contents of the bigarray will be
      written to the block at [(i, j)] after [f] returns. *)

  val iter_read :
    ('v, 'e) t ->
    (int -> int -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t -> unit) ->
    unit
  (** [iter_read t f] applies [f] to each block in [t].

      @param f gets three arguments: the [column] index, [row] index and a
      bigarray with the contents of the current block. *)

  val iter_write :
    ('v, 'e) t ->
    (int -> int -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t -> unit) ->
    unit
  (** [iter_write t f] applies [f] to each block in [t].

      @param f gets three arguments: the [column] index, [row] index and a
      bigarray which should be filled with the values meant for the current
      block. *)
end

(**/**)
val get_x_size : (_, _) t -> int
val get_y_size : (_, _) t -> int
val check_data_type : c -> (_, _) Data.t -> bool
(**/**)
