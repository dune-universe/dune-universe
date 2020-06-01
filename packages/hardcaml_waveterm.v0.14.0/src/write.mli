open! Import

(** stylting functions *)
type styler =
  { start : (string -> unit) -> unit (** called at start *)
  ; set : (string -> unit) -> Draw.Style.t -> unit (** called for each element *)
  ; eol : (string -> unit) -> unit (** called at end of each line *)
  ; finish : (string -> unit) -> unit (** called at end *)
  }

(** not styling information inserted *)
val no_styler : styler

(** Inline CSS per span element *)
val html_styler : styler

(** CSS specified with classes *)
val css_class_styler : styler

(** CSS classes for [css_class_styler]. Dump in CSS file or style tag. *)
val css_classes : string

(** ANSI termianl escape codes *)
val term_styler : styler

(** write data as html escape code *)
val html_escape : ?styler:styler -> (string -> unit) -> Draw.In_memory.ctx -> unit

(** write data as utf-8 *)
val utf8 : ?styler:styler -> (string -> unit) -> Draw.In_memory.ctx -> unit
