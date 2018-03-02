(** Color blending. Inspired from the implementation of Ghostscript *)

type mode =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *)

val blend : mode -> int -> int -> int
    (** [blend blendmode src dst] computes blending of one color component. *)

val f : mode -> int -> Color.rgb -> Color.rgb -> Color.rgb
    (** [f blendmode srcalpha src dst] 8bit depth .

         bug: no destination alpha support.
    *)
