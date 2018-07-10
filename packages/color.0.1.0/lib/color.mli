(** This module deals with colors. *)

(** Representation of a color. *)
type t

module Hsla : sig
  type t = {h: float; s: float; l: float; a: float}
end

module Rgba' : sig
  type t = {r: float; g: float; b: float; a: float}
end

module Rgba : sig
  type t = {r: int; g: int; b: int; a: float}
end

val of_rgba : int -> int -> int -> float -> t
(** Creates a [color] from integer RGB values between 0 and 255
    and a floating point alpha value between 0.0 and 1.0.
    Algorithm adapted from: {{: https://en.wikipedia.org/wiki/HSL_and_HSV} https://en.wikipedia.org/wiki/HSL_and_HSV}*)

val of_rgb : int -> int -> int -> t
(** Creates a [color] from RGB values between 0 and 255. *)

val of_rgba' : float -> float -> float -> float -> t
(** Creates a [color] from RGB and alpha values between 0.0 and 1.0 *)

val of_rgb' : float -> float -> float -> t
(** Creates a [color] from RGB values between 0.0 and 1.0 *)

val of_hsla : float -> float -> float -> float -> t
(** Creates a [color] from Hue, Saturation, Lightness and Alpha values.
    Hue is in degrees, a float value between 0.0 and 360.0. Saturation, Lightness
    and Alpha are float values between 0.0 and 1.0 *)

val of_hsl : float -> float -> float -> t
(** Creates a [color] from Hue, Saturation and Lightness. Hue is in
    degrees, a float value between 0.0 and 360.0. Saturation and Lightness are float
    values between 0.0 and 1.0 *)

val to_hsla : t -> Hsla.t
(** Converts a [color] to its Hue, Saturation, Lightness and Alpha values. *)

val to_rgba' : t -> Rgba'.t
(** Converts a [color] to its rgba value. All values are floats
    between 0.0 and 1.0 *)

val to_rgba : t -> Rgba.t
(** Converts a [color] to its rgba value. RGB values are
    integers in the range of 0 to 255. The alpha channel is a float between
    0.0 and 1.0 *)

val to_hexstring : t -> string
(** Converts a color to its hexadecimal representation. The alpha channel
    is not represented. *)
