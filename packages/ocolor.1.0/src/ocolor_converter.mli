open Ocolor_types

(** {1 Conversion functions} *)

(** {2 Upgrade} *)

(** These function allow converion of color to a larger encoding. Since a
    terminal with 24-bits colors capability can also print 4- and 8-bits colors,
    they are never used in the library. However they are avaiable for the
    developpers.
*)

(** Make a 8-bits color from a 4-bits color. This is not depend on configuration
    (current palette), since 4-bits colors are a particular case of 8-bits
    colors.
*)
val color8_of_color4: color4 -> color8

(** Make a 24-bits color from a 8-bits colors. This is sometimes depend on the
    configuration. Greyscale and 6*6*6 color cubes are pure results (not
    configuration-dependent), but encoding a stadndard 4-bits color into 24-bits
    color use the current palette as set in {!Ocolor_config.palette}.
*)
val color24_of_color8: color8 -> color24

(** Make a 24-bits color from a 4-bits color. This depends on the current
    palette as set in {!Ocolor_config.palette}. This is merely a lookup into the
    palette. *)
val color24_of_color4: color4 -> color24

(** {2 Downgrade} *)

(** These functions are used to print colors in a terminal that does not support
    them by finding the closest. Since most terminal support 24-bits colors,
    these functions will be probably never used. However, in the case of a
    4-bits colors only terminal, the best is to use only 4-bits colors. Indeed,
    the rendering can be surprising and these functions might run slowly.
*)

(** Make a 4-bits color from a 8-bits color. In the case of a greyscale or
    6*6*6 cube color, it uses the palette to find the closest 4-bits color. When
    it is a 4-bits color, it is unchanged. *)
val color4_of_color8: color8 -> color4

(** Make a 8-bits color from a 24-bits color. Use the palette as set in
    {!Ocolor_config.palette}. *)
val color8_of_color24: color24 -> color8

(** Make a 4-bits color from a 24-bits color. Use the palette as set in
    {!Ocolor_config.palette}. *)
val color4_of_color24: color24 -> color4
