val add : char -> char -> char

val sub : char -> char -> char

val mul : char -> char -> char

val div : char -> char -> char

val exp : char -> int -> char

val mul_slice : char -> Core_kernel.Bigstring.t -> Core_kernel.Bigstring.t -> unit

val mul_slice_pure_ocaml : ?skip_to:int -> char -> Core_kernel.Bigstring.t -> Core_kernel.Bigstring.t -> unit

val mul_slice_xor : char -> Core_kernel.Bigstring.t -> Core_kernel.Bigstring.t -> unit

val mul_slice_xor_pure_ocaml : ?skip_to:int -> char -> Core_kernel.Bigstring.t -> Core_kernel.Bigstring.t -> unit

val slice_xor : Core_kernel.Bigstring.t -> Core_kernel.Bigstring.t -> unit
