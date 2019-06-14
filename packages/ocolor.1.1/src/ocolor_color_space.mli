open Ocolor_types

(** {1 Color space functions} *)

(** {2 Downgrade} *)

(** These functions find the closest color from a given 24-bits color in some
    subspace. They use the euclidean distance in CIELAB color space with the CIE
    standard illuminant D65. It should be good enough! There is currently no
    way to change the standard illuminant or the metric.
*)

(** Choose the closest color in the current palette, according to the current
    {!Ocolor_config.palette}. Used only when trying to print a 24-bits color in
    a terminal with only 4-bits capability. Should probably never happen since
    most terminals have truecolors support.
*)
val closest_color4: rgb -> color4

(** Choose the closest color in the current palette, according to the current
    {!Ocolor_config.palette}. Used only when trying to print a 24-bits color in
    a terminal with only 8-bits capability. Should probably never happen since
    most terminals have truecolors support. *)
val closest_color8: rgb -> color8

(** {2 Upgrade} *)

(** Return the rgb color associated to the 4-bits color in argument, according
    to the current {!Ocolor_config.palette}. Only used to upgrade a 4-bits
    color to a 24-bits color. Never used in normal circumstances, but avaiable
    for the developper. Indeed, the result might look different than directly
    printing the 4-bits color if the current palette does not match the
    terminal settings.
*)
val rgb_of_color4: color4 -> rgb