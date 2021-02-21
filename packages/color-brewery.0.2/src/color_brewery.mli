(** Colors palettes and functions to brew colors.

  @version 0.2 *)

type rgba = Gg.color
(** A RGBA color. *)

type cmyk = Gg.v4
(** A CMYK color. *)


val to_int : rgba -> int
(** [to_int c] converts the color to [0xRRGGBB] where [RR], [GG] and
    [BB] are the red, green and blue values expressed on 2 hexadecimal
    digits.  The alpha value is ignored.  This is convenient to
    interact, say, with the Graphics module.  *)

val of_int_exn : ?a: float -> int -> rgba
(** [of_int_exn i] returns the color provided as [0xRRGGBB].
   @raise Invalid_argument if [i] does not represent a color.
   @param a the transparency component of the color.  Default: [0.] *)

val of_int : ?a: float -> int -> rgba option
(** [of_int] is the similar to [of_int_exn] except that it returns
   [None] instead of raising an exception. *)

val to_string : rgba -> string
(** [to_string c] converts the color to a string of the form #RRGGBB. *)


val to_gray : rgba -> rgba
(** [to_gray c] returns the grayscale color corresponding to [c].  It
   is a weighted sum of RGB
   {{:https://www.itu.int/dms_pubrec/itu-r/rec/bt/R-REC-BT.601-7-201103-I!!PDF-E.pdf}Rec. ITU-R T.601-7}
   [0.299 r + 0.587 g + 0.114 b]. *)


(** {2 “Continuous” color ranges} *)

val hue : float -> rgba
(** [hue h] return the color corresponding to the hue [h ∈ \[0., 360.)]. *)

val hue_pct : float -> rgba
(** [hue h] return the color corresponding to the hue [h ∈ \[0., 1.)]. *)

module Gradient : sig
  type t
  (** Represent an interpolation between two colors. *)

  val v : rgba -> rgba -> t
  (** [v c0 c1] construct a gradient from the color [c0] to [c1]. *)

  val rgba : t -> float -> rgba
  (** [rgba g s] returns the color corresponding to [s] ∈ \[0,1\], where
    [s = 0.] returns the first color provided in the gradient and
    [s = 1.] the second. *)

  val cmyk : t -> float -> cmyk
  (** Same as {!rgba} except that it returns a CMYK color. *)
end

val range : ?grad: Gradient.t ->
            n:int -> float -> float -> (float * rgba) list
(** [range ~n a b] generates a uniform sampling of [n] points between
    [a] and [b] (with the bounds [a] and [b] included in the list of
    points) together with colors (based on {!hue} at the moment).

    @param grad generate colors using the given gradient.  Default:
    use the hue. *)

val with_colors : ?grad: Gradient.t -> 'a list -> ('a * rgba) list
(** [with_colors l] add a color range to the list [l].

    @param grad generate colors using the given gradient.  Default:
    use the hue. *)


(** {2 Color palettes (aka colormaps)} *)

(** Colormaps with certain characteristics. *)
module Palette : sig
  type t
  (** A color map. *)

  val length : t -> int
  (** [length m] returns the number of colors in the palette [m]. *)

  val rgb : t -> rgba list
  (** [rgb m] returns the RGB color range of the palette [m]. *)

  val cmyk : t -> cmyk list
  (** [cmyk m] returns the CMYK color range of the palette [m]. *)

  val get_rgb : t -> int -> rgba
  (** [get_rgb m i] return the [i]th RGB color of the palette [m]. *)

  val get_cmyk : t -> int -> cmyk
  (** [get_cmyk m i] return the [i]th CMYK color of the palette [m]. *)

  val gradient : ?interpolate: bool -> t -> Gradient.t
  (** [gradient m] returns a gradient constructed from the palette.
     It only makes sense for sequential and some diverging palettes.

     @param interpolate If [false] (the default), just map *)

  val ty : t -> [`Seq | `Div | `Qual]
  (** [ty m] says whether the palette is [`Seq]uential, [`Div]ergent
     or [`Qual]itative. *)

  val blind : t -> [`Yes | `No | `Maybe]
  (** [blind m] says whether the palette [m] is colorblind safe. *)

  val print : t -> [`Yes | `No | `Maybe]
  (** [print m] says whether the palette [m] is print friendly. *)

  val copy : t -> [`Yes | `No | `Maybe]
  (** [print m] says whether the palette [m] is photocopy safe. *)

  val lcd : t -> [`Yes | `No | `Maybe]
  (** [print m] says whether the palette [m] is friendly for LCD screens. *)

  val find : ?ty:[`Seq | `Div | `Qual] ->
             ?blind:[`Yes | `No | `Maybe] ->
             ?print:[`Yes | `No | `Maybe] ->
             ?copy:[`Yes | `No | `Maybe] ->
             ?lcd:[`Yes | `No | `Maybe] ->
             int -> t list
  (** [find length] return the list of palettes that support the
      desired properties and having at least length [length].  Note
      that most of the palette have ≤ 12 colors and the longer the
      palette, the less it will satisfy other properties.  For
      properties selected with [`Yes] [`No] or [`Maybe], setting [`No]
      (the default) means one does not care, setting [`Maybe] selects
      palettes that maybe (or for sure) satisfy the property and
      [`Yes] selects palettes that satisfy the property for sure.

      - [ty], if provided, restricts maps to the color scheme:
        {ul
         {- [`Seq]: Sequential scheme}
         {- [`Div]: Diverging scheme}
         {- [`Qual]: Qualitative scheme}}
      - [blind]: the palette should be safe for color blind people.
      - [print]: the palette is print friendly.
      - [copy]: the palette is photocopy friendly.
      - [lcd]: the palette is LCD friendly.
   *)


  (** {2 Matplotlib colormaps}

     These are sequential colormaps (each with 256 colors).  You can
     find more information on {{:https://bids.github.io/colormap/}this
     page}. *)
  val viridis : t
  val magma : t
  val inferno : t
  val plasma : t

  (** {2 ColorBrewer schemes} *)

  (** Color schemes as described in the paper: Brewer, Cynthia A.,
      Geoffrey W. Hatchard and Mark A. Harrower, 2003, ColorBrewer in
      Print: A Catalog of Color Schemes for Maps, Cartography and
      Geographic Information Science 30(1): 5-32.

      See also www.ColorBrewer.org *)

  (** {3 Sequential schemes} *)

  val ylgn : t list     (** Light yellow to dark green *)

  val ylgnbu : t list   (** Light yellow to green to dark blue *)

  val gnbu : t list     (** Light green to dark blue *)

  val bugn : t list     (** Light blue to dark green *)

  val pubugn : t list   (** Light purple to blue to dark green *)

  val pubu : t list     (** Light purple to dark blue *)

  val bupu: t list      (** Light blue to dark purple *)

  val rdpu : t list     (** Light red to dark purple *)

  val purd : t list     (** Light purple to dark red *)

  val orrd : t list     (** Light orange to dark red *)

  val ylorrd : t list   (** Light yellow to orange to dark red *)

  val ylorbr : t list   (** Light yellow to orange to dark brown *)

  (** {3 Sequential schemes, single hue} *)

  val purples : t list  (** Light to dark purple *)

  val blues : t list    (** Light to dark blue *)

  val greens : t list   (** Light to dark green *)

  val oranges : t list  (** Light to dark oranges *)

  val reds : t list     (** Light to dark red *)

  val greys : t list    (** Light to dark gray *)

  (** {3 Diverging schemes} *)

  val puor : t list     (** Dark orange to light to dark purple *)

  val brbg : t list     (** Dark brown to light to dark blue-green *)

  val prgn : t list     (** Dark reddish-purple to light to dark green *)

  val piyg : t list     (** Dark magenta to light to dark yellow-green *)

  val rdbu : t list     (** Dark red to light to dark blue *)

  val rdgy : t list     (** Dark red to light to dark grey *)

  val rdylbu : t list   (** Dark red to light yelow to dark blue *)

  val spectral : t list (** Dark red, orange, light yellow, green, dark blue *)

  val rdylgn : t list   (** Dark red, orange, light yellow, yellow-green,
                            dark green *)

  (** {3 Qualitative schemes} *)

  val set1 : t list    (** Includes bold, readily named, basic colors (such
                           as red, green, blue) *)

  val pastel1 : t list (** Lighter version of [Set1] *)

  val set2 : t list    (** Includes mostly a mixture colors (such as
                           blue-green, red-orange) *)

  val pastel2 : t list (** Lighter version of [Set2] *)

  val dark2 : t list   (** Darker version of [Set2] *)

  val set3 : t list    (** Medium saturation set with more lightness
                           variation and more classes than [Set1] and [Set2]. *)

  val paired : t list  (** Light/dark paris for namable hues *)

  val accent : t list  (** Include lightness and saturation extremes to
                           accent small or important areas *)
end

