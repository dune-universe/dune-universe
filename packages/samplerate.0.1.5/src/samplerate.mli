(**
  * Bindings for libsamplerate.
  *
  * @author Samuel Mimram
  *)

(** Kind of converter. *)
type converter =
  | Conv_sinc_best_quality
      (** This is a bandlimited interpolator derived from the mathematical sinc function and this is the highest quality sinc based converter, providing a worst case Signal-to-Noise Ratio (SNR) of 97 decibels (dB) at a bandwidth of 97%. All three Conv_sinc_* converters are based on the techniques of Julius O. Smith  although this code was developed independantly. *)
  | Conv_sinc_medium_quality
      (** This is another bandlimited interpolator much like the previous one. It has an SNR of 97dB and a bandwidth of 90%. The speed of the conversion is much faster than the previous one. *)
  | Conv_fastest
      (** This is the fastest bandlimited interpolator and has an SNR of 97dB and a bandwidth of 80%. *)
  | Conv_zero_order_hold
      (** A Zero Order Hold converter (interpolated value is equal to the last value). The quality is poor but the conversion speed is blindlingly fast. *)
  | Conv_linear
      (** A linear converter. Again the quality is poor, but the conversion speed is blindingly fast. *)

(** Name of a converter. *)
val get_conv_name : converter -> string

(** Description of a converter. *)
val get_conv_descr : converter -> string

(** [convert converter channels ratio inbuf offset length]. *)
val convert :
  converter -> int -> float -> float array -> int -> int -> float array

type t

val create : converter -> int -> t

val process :
  t ->
  float ->
  float array ->
  int ->
  int ->
  float array ->
  int ->
  int ->
  int * int

val process_ba :
  t ->
  float ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int * int

val process_alloc : t -> float -> float array -> int -> int -> float array
val reset : t -> unit
