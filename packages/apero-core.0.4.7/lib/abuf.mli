type byte = Abytes.byte
type bigstring = Abytes.bigstring
type t      

include Ordered.Comparable with type t := t

val create : ?grow:int -> int -> t
(** [create c] allocates a new Abuf of capacity [c]. *)

val create_bytes : ?grow:int -> int -> t
(** [create_bytes c] allocates a new Abuf of bytes and of capacity [c]. *)

val create_bigstring : ?grow:int -> int -> t
(** [create_bigstring c] allocates a new Abuf of bigstring and of capacity [c]. *)

val from_abytes : Abytes.t -> t 
(** [from_abytes bs] creates an Abuf by wrapping [bs].
    The resulting Abuf writer position will be set to the capacity of [bs]. *)

val from_bytes : ?grow:int -> bytes -> t 
(** [from_bytes bs] creates an Abuf by wrapping [bs].
    The resulting Abuf writer position will be set to the length of [bs]. *)

val from_bigstring : ?grow:int -> bigstring -> t 
(** [from_bigstring bs] creates an Abuf by wrapping [bs].
    The resulting Abuf writer position will be set to the length of [bs]. *)

val duplicate : t -> t
(** [duplicate buf] Returns an Abuf which shares the whole region of [buf]. 
    This operation involves NO COPY. Modifying the content of the returned 
    buffer or of [buf] affects each other's content while they maintain separate 
    positions and marks. This method does not modify the positions of [buf]. *)

val wrap : ?grow:int -> t list -> t
(** [wrap bs] creates an Abuf by wrapping the Abufs in the list [bs].
    The capacity for the Abuf will be set to the sum of the readable_bytes 
    of the Abufs in the list [bs].
    This operation involves NO COPY. Modifying the content of the returned 
    buffer or of [buf] affects each other's content while they maintain 
    separate positions and marks. *)

val slice : int -> int -> t -> t
(** [slice from len buf] creates an Abuf that wraps the subregion 
    of buffer [buf] of length [len] starting at index [from]. 
    This operation involves NO COPY. Modifying the content of the returned 
    buffer or of [buf] affects each other's content while they maintain 
    separate positions and marks. The resulting buffer is not expandable. 
    It's reader position and writer position are respectively set to 0 and len. *)

val capacity : t -> int
(** [capacity buf] returns the number of bytes (octets) the buffer 
    [buf] can contain. *)

val clear : t -> unit
(** [clear buf] sets the reader position and writer position of [buf] to 0. *)

val compact : int -> t -> unit 
(**  [compact buf] moves the current content of the buffer so that the first byte starts at r_pos = 0. *)

val r_pos : t -> int
(** [r_pos buf] returns the reader position of [buf]. *)

val set_r_pos : int -> t -> unit
(** [set_r_pos p buf] sets the reader position of [buf] to [r]. *)

val mark_r_pos : t -> unit
(** [mark_r_pos buf] marks the current reader position in the buffer [buf]. *)

val reset_r_pos : t -> unit
(** [reset_r_pos buf] resets the reader position of [buf] to the marked position. 
    The marked reader position is initially set to 0. *)

val w_pos : t -> int
(** [w_pos buf] returns the writer position of [buf]. *)

val set_w_pos : int -> t -> unit
(** [set_w_pos p buf] sets the writer position of [buf] to [p]. *)

val mark_w_pos : t -> unit
(** [mark_w_pos buf] marks the current writer position in the buffer [buf]. *)

val reset_w_pos : t -> unit
(** [reset_w_pos buf] resets the writer position of [buf] to the marked position. 
    The marked writer position is initially set to 0. *)

val readable : t -> bool
(** [readable buf] returns true if and only if 
    ((w_pos [buf]) - (r_pos [buf])) is greater than 0. *)

val readable_bytes : t -> int
(** [readable_bytes buf] returns the number of readable bytes of [buf] 
    which is equal to ((w_pos [buf]) - (r_pos [buf])). *)

val writable : t -> bool
(** [writable buf] returns true if and only if 
    ((capacity [buf]) - (w_pos [buf])) is greater than 0. *)

val writable_bytes : t -> int
(** [writable_bytes buf] returns the number of writable bytes of [buf] 
    which is equal to ((capacity [buf]) - (w_pos [buf])). *)


val skip : int -> t -> unit
(** [skip n buf] increases the reader position by [n] in [buf]. *)


val read_byte : t -> byte
(** [read_byte buf] gets a byte from [buf] at reader position and 
    increases the reader position by 1 in [buf]. *)

val read_bytes : int -> t -> bytes
(** [read_bytes n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)

val read_abytes : int -> t -> Abytes.t
(** [read_abytes n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)

val read_bigstring : int -> t -> bigstring
(** [read_bigstring n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)

val read_buf : int -> t -> t 
(** [read_bytes n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)


val get_byte : at:int -> t -> byte
(** [get_byte ~at buf] gets a byte from [buf] at index [at]. *)

val get_bytes : at:int -> int -> t -> bytes
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_abytes : at:int -> int -> t -> Abytes.t
(** [get_abytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_bigstring : at:int -> int -> t -> bigstring
(** [get_bigstring ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_buf : at:int -> int -> t -> t
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)


val write_byte : byte -> t -> unit
(** [write_byte b buf] sets the byte [b] in [buf] at writer position and 
    increases the writer position by 1 in [buf]. *)

val write_bytes : bytes -> t -> unit
(** [write_bytes bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (length [bs]) in [buf]. *)

val write_abytes : Abytes.t -> t -> unit
(** [write_abytes bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (length [bs]) in [buf]. *)

val write_bigstring : bigstring -> t -> unit
(** [write_bigstring bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (length [bs]) in [buf]. *)

val write_buf : t -> t -> unit
(** [write_buf bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (readable_bytes [bs]) in [buf]. *)


val set_byte : byte -> at:int -> t -> unit
(** [set_byte b ~at buf] sets the byte [b] in [buf] at index [at]. *)

val set_bytes : bytes -> at:int -> t -> unit
(** [set_bytes bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_abytes : Abytes.t -> at:int -> t -> unit
(** [set_abytes bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_bigstring : bigstring -> at:int -> t -> unit
(** [set_bigstring bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_buf : t -> at:int -> t -> unit 
(** [set_buf bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)


val blit : src:t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bytes : src:bytes -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bytes : src:t -> src_idx:int -> dst:bytes -> dst_idx:int -> len:int -> unit
(** [blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bigstring : src:bigstring -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bigstring : src:t -> src_idx:int -> dst:bigstring -> dst_idx:int -> len:int -> unit
(** [blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_abytes : src:Abytes.t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit_from_abytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_abytes : src:t -> src_idx:int -> dst:Abytes.t -> dst_idx:int -> len:int -> unit
(** [blit_to_abytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)


val to_io_vecs : idx:int -> len:int -> append_bytes:('a -> bytes -> int -> int -> unit) -> append_bigarray:('a -> bigstring -> int -> int -> unit) -> 'a -> t -> unit


val hexdump : ?separator:string -> t -> string
(** [hexdump buf] returns an hexadecimal representation of the bytes 
    in buffer [buf] from index 0 to writer position. *)

val to_string : t -> string