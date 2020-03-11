module type PRINTER =
  sig
    type t
    val empty: t
    val (<+>): t -> t -> t
    val char: char -> t
    val substring: string -> int -> int -> t
    val fill: int -> char -> t
  end



module type SIG =
  sig
    (** Type of a pretty printer. *)
    type t


    (** The printer which prints nothing. *)
    val empty: t



    (** [substring s pos len]: pretty printer printing a substring of [s]
       starting at [pos] and having length [len]. *)
    val substring: string -> int -> int -> t


    (** [string s]: pretty printer printing the string [s]. *)
    val string: string -> t


    (** [char s]: pretty printer printing the character [c]. *)
    val char: char -> t


    (** [fill n c]: pretty printer printing the character [c] [n] times. *)
    val fill: int -> char -> t


    (** [text s]: pretty printer printing an optional line break with the
       alternative text [s]. *)
    val line: string -> t


    (** pretty printer printing an optional line break with no alternative
       text.  *)
    val cut: t


    (** pretty printer printing an optional line break with a blank as
       alternative text.  *)
    val space: t


    (** [nest i pp]: pretty printer printing the same content as [pp] but
       doing an additional indent of [i] at each line break.  *)
    val nest: int -> t -> t


    (** nest the list of pretty printers. *)
    val nest_list: int -> t list -> t


    (** [nest i pp]: pretty printer printing the same content as [pp] but
       doing an additional indent of [i] relative to the current position at
       each line break.  *)
    val nest_relative: int -> t -> t


    (** [group pp]: treat all line breaks appearing in [pp] consistently
       i.e. either output all as line breaks or all with their alternative
       text. *)
    val group: t -> t


    (** [group_list lst]: group the list of pretty printers [lst]. *)
    val group_list: t list -> t


    (** [wrap_words s]: Print the string [s] with all the words in it
        potentially wrapped. *)
    val wrap_words: string -> t


    (** [fill_paragraph s]: Print the string [s] as a paragraph i.e. putting
       as many words on a line as possible. *)
    val fill_paragraph: string -> t


    (** [p1 <+> p2] Print [p1] and then [p2]. *)
    val (<+>): t -> t -> t


    (** [chain list] Print the list of printers. *)
    val chain: t list -> t


    val chain_separated: t list -> t -> t

    val list_separated: t -> t list -> t
  end





(** Customizable pretty printer: Works with any printer which satisfies the
   PRINTER interface. *)
module Pretty (P:PRINTER):
sig
  include SIG

(** [run indent width ribbon pp]: A printer generated from the pretty printer
   [pp] formatted with an initial [indent], a line [width] and a [ribbon]
   width. *)
  val run: int -> int -> int -> t -> P.t
end
