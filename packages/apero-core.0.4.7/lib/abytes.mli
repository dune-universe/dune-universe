type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t      

include Ordered.Comparable with type t := t

val create : ?grow:int -> int -> t
(** [create c] allocates a new Abytes of capacity [c]. *)

val create_bytes : ?grow:int -> int -> t
(** [create_bytes c] allocates a new Abytes of bytes and of capacity [c]. *)

val create_bigstring : ?grow:int -> int -> t
(** [create_bigstring c] allocates a new Abytes of bigstring and of capacity [c]. *)

val duplicate : t -> t
(** [duplicate bs] Returns an Abytes which shares the whole region of [bs]. 
    This operation involves NO COPY. Modifying the content of the returned 
    Abytes or of [bs] affects each other's content. *)

val from_bytes : ?grow:int -> bytes -> t 
(** [from_bytes bs] creates an Abytes by wrapping [bs].
    The capacity for the Abytes will be set to the length of [bs]. *)

val from_bigstring : ?grow:int -> bigstring -> t 
(** [from_bigstring bs] creates an Abytes by wrapping [bs].
    The capacity for the Abytes will be set to the length of [bs]. *)

val wrap : ?grow:int -> t list -> t
(** [wrap bs] creates an Abytes by wrapping the Abytes in the list [bs].
    The capacity for the Abytes will be set to the sum of the capacities 
    of the Abytes in the list [bs].
    This operation involves NO COPY. Modifications on the resulting 
    Abytes will modify the original Abytes in the list [bs] and reverse. *)

val slice : int -> int -> t -> t
(** [slice from len bs] creates an Abytes that wraps the subregion 
    of Abytes [bs] of length [len] starting at index [from]. 
    This operation involves NO COPY. Modifications on the resulting 
    Abytes will modify the original [bs] and reverse. 
    The resulting Abytes is not expandable. It's reader position and 
    writer position are set to 0. *)

val capacity : t -> int
(** [capacity bs] returns the number of bytes (octets) the Abytes 
    [bs] can contain. *)

val get_byte : at:int -> t -> byte
(** [get_byte ~at bs] gets a byte from [bs] at index [at]. *)

val get_bytes : at:int -> int -> t -> bytes
(** [get_bytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_bigstring : at:int -> int -> t -> bigstring
(** [get_bigstring ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_abytes : at:int -> int -> t -> t
(** [get_abytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)


val set_byte : byte -> at:int -> t -> unit
(** [set_byte b ~at bs] sets the byte [b] in [bs] at index [at]. *)

val set_bytes : bytes -> at:int -> t -> unit
(** [set_bytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_bigstring : bigstring -> at:int -> t -> unit
(** [set_bigstring s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_abytes : t -> at:int -> t -> unit
(** [set_abytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)


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

val to_io_vecs : idx:int -> len:int -> append_bytes:('a -> bytes -> int -> int -> unit) -> append_bigarray:('a -> bigstring -> int -> int -> unit) -> 'a -> t -> unit


val hexdump : ?separator:string -> t -> string
(** [hexdump bs] returns an hexadecimal representation of the bytes 
    in Abytes [bs] from index 0 to writer position. *)

val to_string : t -> string