(** A high-level interface for Font metrics *)

type t = Tfm.t

val char_width : t -> int -> float
(** [char_width t i] returns the width of the [i]th character of the font metric
   [t], [0] is the first character *)

val char_height : t -> int -> float
(** same as [char_width], but for character height *)

val char_depth : t -> int -> float
(** same as [char_width], but for character depth *)

(** same as [char_width], but for character depth *)
val char_italic : t -> int -> float
(** same as [char_width], but for italic correction of the character *)

val char_dims : t -> int -> float * float * float
(** [char_dims metric i] returns the width, height and depth of the [i]th
  char, slightly more efficient than invoking the other functions three times *)

val slant : t -> float
(** is the amount of italic slant, which is used to help position accents. For
  example, slant=.25 means that when you go up one unit, you also go .25 units
  to the right. *)

val space : t -> float
(** is the normal spacing between words in text. Note that character " " in the
   font need not have anything to do with blank spaces. *)

val space_stretch : t -> float
(** is the amount of glue stretching between words. *)

val space_shrink : t -> float
(** is the amount of glue shrinking between words. *)

val x_height : t -> float
(** is the height of letters for which accents don't have to be raised or
   lowered. *)

val quad : t -> float
(** is the size of one em in the font. *)

(** is the size of one em in the font. *)
val extra_space : t -> float
(** is the amount added to [space] at the ends of sentences. *)
