(** {1 Format-friendly functions} *)

(** {2 Overview} *)

(** These are functions made to be used easily with [Format] functions. They use
    semantic tags to apply and remove a style. Moreover, they are made to be
    compositional: when using a new style (typically, in a %a context) that
    override a current style, the previous style is recovered when closing the
    new one.

    For instance, in [printf "a@{<red>b@{<blue>c@}d@}e"], the 'a' will be
    printed with the default color (usually white), the 'b' will be in red, the
    'c' in blue (since "blue" overrides "red"), the 'd' in red (since "blue" is
    closed, but nut "red" yet), and 'e' in the default color again.

    When using styles that could exists together (for instance, a foreground
    color, a background color and a bold font), each is handled separatly. For
    instance, in [printf "a@{<red>b@{<bold>c@}d@}e"], 'a' and 'e' will be
    printed with default style, 'b' and 'd' will be only red, but 'c' will be
    bold and red.

    Allowed tags are:
    - standard 4-bits colors: [black], [red], [green], [blue], [yellow],
      [magenta], [cyan], [white] and the same colors prefixed with [hi_] for
      high intensity versions
    - rgb colors in the format [rgb(ddd,ddd,ddd)] where [ddd] are numbers
      between 0 and 255 (included) or [rgb(0xhh,0xhh,0xhh)] where [hh] are
      numbers between 0 and ff (included). [0x] are mandatory, to disambiguate
      [10] and [0x10] for instance. There must be no spaces.
    - 6*6*6 color, in the format [c6(d,d,d)] where [d] are numbers between 0 and
      5 (included). There must be no spaces. These colors are interpreted as
      8-bits colors.
    - 24-step grayscale in the format [gs(dd)] where[dd] is a number between 0
      and 23 (included). There must be no spaces.  These colors are interpreted
      as 8-bits colors.
    - a X11 color name (just as in [X11/rgb.txt]). In this case, it may constain
      spaces (for instance [Dark Magenta])
    - a X11 color name prefixed by [x11_]. Useful to disambiguate with standard
      colors. For instance [green] is a standard 4-bits color, but [x11_green]
      is the x11 color.
    - a color prefixed by [bg_] to apply this color on the background. For
      instance [bg_green], [bg_hi_green], [bg_x11_green], [bg_gs(12)],
      [bg_rgb(1,2,3)].
    - [font(i)] with [i] between 1 and 9 included. Rarely supported
    - [ul], [under], [underlined]: underlined
    - [uul], [dul], [uunder], [dunderlined], [doubleunderlined]: doubly underlined.
      Rarely supported. Sometimes interpreted as "bold off". Use at your own risks.
    - [b], [bold]: bold
    - [faint]
    - [blink]
    - [conceal]
    - [frak], [fraktur]: rarely supported
    - [reverse]: reverse video, exchange foreground and background colors
    - [crossed]
    - [framed]: rarely supported
    - [it], [italic]
    - [encircled]: rarely supported
    - [ol], [over], [overlined]

    There must be no extra spaces, and everything is case sensitive.

    One can as well make a tag by separating two tags by a ";" (without spaces).
    All styles will be applied and removed at once. For instance
    [Ocolor_format.printf "@{<red;bold>foo@}"] is the same as
    [Ocolor_format.printf "@{<red>@{<bold>foo@}@}"].

    Of course, tags can be opened and closed using [Format.pp_open_tag] and
    [Format.pp_close_tag]. We can rewrite the previous example as
    [Ocolor_format.printf "%afoo%a" Format.pp_open_tag "red;bold" Format.pp_close_tag ()].
    This seems less smart, but this is useful to make styles parametrizable.

    However, strictly speaking, applying several styles at once is
    implementation defined. But most reasonnable combinations will work anyway.

    To make it work, the formatter should have semantic tags enabled and
    correctly set, this is the job of {!make_formatter} and
    {!prettify_formatter}. There are also pretty standard formatters
    (std_formatter and err_formatter) and their printers (printf and eprintf)
    and formatter-less functions (kasprintf and asprintf) that rely on a local
    pretty formatter (to a buffer) to generate strings.

    Prettifying a formatter with {!prettify_formatter} is simply
    calling {!make_formatter} and ignoring the result. The custom {!formatter}
    is only needed to do introspection on the current style, this is not the
    most current usage. Prettifying a formatter enables tag marking
    (using {!Format.pp_set_mark_tags}) and set (overwrite) {!mark_open_tag} and
    {!mark_close_tag}. It does not change anything about tag printing.

    If one use functions like {!pp_bool}, {!pp_option}, {!pp_list} etc. with a
    non prettified formatter, the text will be printed with tags, but they won't
    be interpreted. Thus, with a standard formatter (in which tag marking is
    disabled by default), it will print raw text. If the formatter allow tag
    marking, [mark_open_tag] and [mark_close_tag] will be used. Thus, it depends
    on these functions.

    To disable pretty printing on a {!Format.formatter}, one can simply disable
    tag marking with [Format.pp_set_mark_tags fmt false]. It can be useful to
    make colors conditional.

    Please note that only ANSI escape code are issued. It should suit most
    modern terminals. However they should not be interpreted on Windows before
    Windows 10 1511 (November update).
*)

(** {2 Examples} *)

(**
   {[
     let () = Ocolor_format.printf "a@{<red>b@}c"
     (* "abc" is printed on the standard output
        "a" and "c" without styles and "b" in red.
     *)

     let pp_pair (fmt: Format.formatter) ((a, b): int * int) : unit =
       Format.fprintf fmt "(@{<bold>%d@}, @{<ul>%d@})" a b

     let pp_pair_pair (fmt: Format.formatter) ((a, b): (int * int) * (int * int)) : unit =
       Format.fprintf fmt "(@{<red>%a@}, @{<blue>%a@})" pp_pair a pp_pair b

     let () = Format.fprintf Ocolor_format.raw_std_formatter "%a" pp_pair_pair ((1, 2), (3, 4))
     (* "((1, 2), (3, 4))" is printed on the standard output.
        "(1, 2)" is red, "(3, 4)" is blue.
        "1" and "3" are bold and "2" and "4" are underlined.

        Equivalent to
     *)

     let pp_pair (fmt: Format.formatter) ((a, b): int * int) : unit =
       Format.fprintf fmt "(%a%d%a, %a%d%a)"
         Format.pp_open_tag "bold"
         a
         Format.pp_close_tag ()
         Format.pp_open_tag "ul"
         b
         Format.pp_close_tag ()

     let pp_pair_pair (fmt: Format.formatter) ((a, b): (int * int) * (int * int)) : unit =
       Format.fprintf fmt "(%a%a%a, %a%a%a)"
         Format.pp_open_tag "red"
         pp_pair a
         Format.pp_close_tag ()
         Format.pp_open_tag "blue"
         pp_pair b
         Format.pp_close_tag ()

     let () = Format.fprintf Ocolor_format.raw_std_formatter "%a" pp_pair_pair ((1, 2), (3, 4))

     let () = Ocolor_format.printf "%a"
         (Ocolor_format.pp_list
            (fun fmt n ->
               Format.pp_open_tag fmt "red";
               Format.pp_print_int fmt n;
               Format.pp_close_tag fmt ()
            )
         )
         [1;2;3]
     (* Prints "[1; 2; 3]" each number in red, semicolons and brackets are fainted. *)

     (* Same as *)
     let () = Ocolor_format.printf "%a"
         (Ocolor_format.pp_list_generic
            ~elem_style:[Ocolor_types.(Fg (C4 red))]
            Format.pp_print_int
         )
         [1;2;3]
   ]}
*)

(** {2 Custom formatters} *)

(** The custom formatter to print with nice colors. To use only with toplevel
    functions. All composition ([%a], for instance) rely on standard
    {!Format.formatter} and thus, one can use standard {!Format} functions.

    It internally keeps some information about the current state to be able to
    resume the previous style when closing a tag. It is used only for
    introspection. It contains a standard {!Format.formatter} that is enough for
    pretty printing. One can get it with {!unwrap_formatter}.
*)
type formatter

(** Make formatter from standard {!Format.formatter}. In particular, it enables
    pretty printing in the given formatter. Thus, one could use it with Format
    functions and it will work! *)
val make_formatter: Format.formatter -> formatter

(** Get the {!Format.formatter} hinding in a {!formatter}. Beware
    [unwrap_formatter (make_formatter fmt)] is [fmt]. *)
val unwrap_formatter : formatter -> Format.formatter

(** Prettify a standard {!Format.formatter}. It enables pretty printing in the
    given formatter. But does not return the custom formatter. Enough in most
    cases. In fact, except if one wants to introspect the styles, this is what
    one should use. It is exactly the same as {!make_formatter} and ignoring the
    result. *)
val prettify_formatter: Format.formatter -> unit

(** The equivalent of {!Format.std_formatter} but for Ocolor. Using this
    formatter will interpret semantic tags and print with styles, and output to
    the standard output. No effect on {!Format.std_formatter}, thus, no
    synchronisation is guaranteed.  *)
val std_formatter : formatter

(** The equivalent of {!Format.err_formatter} but for Ocolor. Using this
    formatter will interpret semantic tags and print with styles, and output to
    the standard error output. No effect on {!Format.err_formatter}, thus, no
    synchronisation is guaranteed.  *)
val err_formatter : formatter

(** The equivalent of {!Format.std_formatter} but for Ocolor, without the
    wrapper. {!Format.std_formatter} is not prettified, it is another
    formatter, thus, no synchronisation is guaranteed. *)
val raw_std_formatter : Format.formatter

(** The equivalent of {!Format.err_formatter} but for Ocolor, without the
    wrapper. {!Format.err_formatter} is not prettified, it is another
    formatter, thus, no synchronisation is guaranteed. *)
val raw_err_formatter : Format.formatter

(** Make a new formatter from a buffer. Equivalent to
    {!Format.formatter_of_buffer} followed by {!make_formatter}. *)
val formatter_of_buffer: Buffer.t -> formatter

(** Make a {!Format.formatter} from a buffer. Equivalent to
    {!Format.formatter_of_buffer} followed by {!prettify_formatter}. *)
val raw_formatter_of_buffer: Buffer.t -> Format.formatter

(** {2 Format-like functions} *)

(** The equivalent of {!Format.printf} but this one understand tags. Equivalent
    to [Format.fprintf raw_std_formatter]. *)
val printf: ('a, Format.formatter, unit) format -> 'a

(** The equivalent of {!Format.eprintf} but this one understand tags. Equivalent
    to [Format.fprintf raw_err_formatter]. *)
val eprintf: ('a, Format.formatter, unit) format -> 'a

(** The equivalent of {!Format.kasprintf} but this one understand tags. It uses
    a temporary buffer. *)
val kasprintf: (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b

(** The equivalent of {!Format.asprintf} but this one understand tags. It uses
    a temporary buffer. *)
val asprintf: ('a, Format.formatter, unit, string) format4 -> 'a

(** Flush the formatter. It is just a flush on the underlying
    {!Format.formatter} *)
val pp_print_flush: formatter -> unit -> unit

(** {2 Introspection} *)

(** Introspect a {!formatter} to find the current foreground style. *)
val get_current_fg_color: formatter -> Ocolor_types.color option

(** Introspect a {!formatter} to find the current background style. *)
val get_current_bg_color: formatter -> Ocolor_types.color option

(** {2 Pretty printers} *)

#if OCAML_VERSION >= (4, 08, 0)
type Format.stag += Ocolor_styles_tag of Ocolor_types.style list
type Format.stag += Ocolor_style_tag of Ocolor_types.style
#endif

(** Open a semantic tag with the string corresponding to the fiven style list. *)
val pp_open_styles: Format.formatter -> Ocolor_types.style list -> unit

(** Open a semantic tag with the string corresponding to the fiven style. *)
val pp_open_style: Format.formatter -> Ocolor_types.style -> unit

(** Alias for {!Format.pp_close_tag}. Symmetric for {!pp_open_styles}. *)
val pp_close_styles: Format.formatter -> unit -> unit

(** Alias for {!Format.pp_close_tag}. Symmetric for {!pp_open_style}. *)
val pp_close_style: Format.formatter -> unit -> unit

(** {3 Pretty printers for common types} *)

(** {4 Modules} *)

(** The module that contains handy pretty printers with styling enbaled.
    This module is included at the toplevel to make its contents quickly
    available. *)
module StylingPrettyPrinters: Ocolor_pp.PRETTY_PRINTERS
  with type formatter := Format.formatter

(** The module that contains handy pretty printers with styling disabled.
    Even with a prettified formatter, there will be no styles. *)
module NonStylingPrettyPrinters: Ocolor_pp.PRETTY_PRINTERS
  with type formatter := Format.formatter


(** {4 Generic functions} *)
(** These functions come from [StylingPrettyPrinters]
*)

(** Pretty print a bool. Useful with %a. It displays boolean as %b but with
    styles.
    [false_style] defaults to
    [[Ocolor_types.Bold;Ocolor_types.Fg (C4 Ocolor_values.red)]] (ie. red and
    bold) and [true_style] defaults to
    [[Ocolor_types.Bold;Ocolor_types.Fg (C4 Ocolor_values.green)]] (ie. green and
    bold). *)
val pp_bool_generic:
  ?false_style:Ocolor_types.style list ->
  ?true_style:Ocolor_types.style list ->
  Format.formatter -> bool -> unit

(** Pretty print a list. Useful with %a.
    Default settings:
    - [left] = ["\["]
    - [sep] = ["; "]
    - [right] = ["\]"]
    - [delim_style] = [Ocolor_types.[Faint]]
    - [sep_style] = [Ocolor_types.[Faint]]
    - [elem_style] = [[]]

    Moreover, it need a (pretty) printer that prints an element

    With default settings, if the list is empty it prints "[]". If the list
    contains only one element [a], it prints "[a]". Otherwise, elements are
    separated by "; " without final separator, for instance "[a; b; c]"

    [delim_style] is applied to [left] and [right], [sep_style] is applied to
    [sep] and [elem_style] is applied to each element. In particular, the pretty
    printer will work in the context set by [elem_style], so it does not need to
    apply the same style once again (however, it is harmless).
*)
val pp_list_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list ->
  ?sep_style:Ocolor_types.style list ->
  ?elem_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print an option. Useful with %a.
    Default settings:
    - [none] = ["None"]
    - [none_style] = [Ocolor_types.[Faint]]
    - [some_style] = [[]]

    Moreover, it need a (pretty) printer that prints the content of the option.

    It prints [none] when the option is [None] with the style [none_style], and
    [p a] where [p] is the printer and the option is [Some a]. In particular, it
    does not display "Some" or something like that.
*)
val pp_option_generic:
  ?none:string ->
  ?none_style:Ocolor_types.style list ->
  ?some_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

(** Pretty print a pair. Useful with %a.
    Default settings:
    - [left] = ["("]
    - [sep] = [", "]
    - [right] = [")"]
    - [delim_style] = [Ocolor_types.[Faint]]
    - [sep_style] = [Ocolor_types.[Faint]]
    - [elem_style] = [[]]

    Moreover, it need two (pretty) printers that prints each part of the pair.

    [delim_style] is applied to [left] and [right], [sep_style] is applied to
    [sep] and [elem_style] is applied to each element. In particular, the pretty
    printer will work in the context set by [elem_style], so it does not need to
    apply the same style once again (however, it is harmless).
*)
val pp_pair_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list -> ?sep_style:Ocolor_types.style list -> ?elem_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter) ->
  ('a * 'b) -> unit

val pp_3_tuple_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list -> ?sep_style:Ocolor_types.style list -> ?elem_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c) -> unit

val pp_4_tuple_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list -> ?sep_style:Ocolor_types.style list -> ?elem_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c * 'd) -> unit

val pp_5_tuple_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list -> ?sep_style:Ocolor_types.style list -> ?elem_style:Ocolor_types.style list ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter -> 'e -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c * 'd * 'e) -> unit

(** Print an itarable set-like data structure
    Default settings:
    - [left] = ["{"]
    - [sep] = ["; "]
    - [right] = ["}"]
    - [delim_style] = [Ocolor_types.[Faint]]
    - [sep_style] = [Ocolor_types.[Faint]]
    - [elem_style] = [[]]
*)
val pp_iterable_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?delim_style:Ocolor_types.style list ->
  ?sep_style:Ocolor_types.style list ->
  ?elem_style:Ocolor_types.style list ->
  (('value -> unit) -> 't -> unit) ->
  (Format.formatter -> 'value -> unit) ->
  Format.formatter -> 't -> unit

(** Print an itarable map-like data structure
    Default settings:
    - [left] = ["{"]
    - [sep] = ["; "]
    - [right] = ["}"]
    - [delim_style] = [Ocolor_types.[Faint]]
    - [sep_style] = [Ocolor_types.[Faint]]
*)
val pp_iterable_mapping_more_generic:
    ?left:string -> ?sep:string -> ?right:string ->
    ?delim_style:Ocolor_types.style list ->
    ?sep_style: Ocolor_types.style list ->
    (('key -> 'value -> unit) -> 't -> unit) ->
    (Format.formatter -> 'key * 'value -> unit) ->
    Format.formatter -> 't -> unit

(** Print an itarable map-like data structure as a sequence of
    <key><mapsto><value> separeted by <sep>.
    Default settings:
    - [left] = ["{"]
    - [sep] = ["; "]
    - [right] = ["}"]
    - [mapsto] = [":"]
    - [delim_style] = [Ocolor_types.[Faint]]
    - [sep_style] = [Ocolor_types.[Faint]]
    - [mapsto_style] = [Ocolor_types.[Faint]]
    - [key_style] = [[]]
    - [value_style] = [[]]
*)
val pp_iterable_mapping_generic:
  ?left:string -> ?sep:string -> ?right:string ->
  ?mapsto:string ->
  ?delim_style:Ocolor_types.style list ->
  ?sep_style:Ocolor_types.style list ->
  ?mapsto_style:Ocolor_types.style list ->
  ?key_style:Ocolor_types.style list ->
  ?value_style:Ocolor_types.style list ->
  (('key -> 'value -> unit) -> 't -> unit) ->
  (Format.formatter -> 'key -> unit) ->
  (Format.formatter -> 'value -> unit) ->
  Format.formatter -> 't -> unit


(** {4 Default pretty printers} *)

(** Like {!pp_bool_generic} with default settings *)
val pp_bool: Format.formatter -> bool -> unit

(** Like {!pp_list_generic} with default settings *)
val pp_list: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Like {!pp_option_generic} with default settings *)
val pp_option: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

(** Like {!pp_pair_generic} with default settings *)
val pp_pair:
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter) ->
  ('a * 'b) -> unit

(** Like {!pp_3_tuple_generic} with default settings *)
val pp_3_tuple:
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c) -> unit

(** Like {!pp_4_tuple_generic} with default settings *)
val pp_4_tuple:
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c * 'd) -> unit

(** Like {!pp_5_tuple_generic} with default settings *)
val pp_5_tuple:
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter -> 'e -> unit) ->
  (Format.formatter) ->
  ('a * 'b * 'c * 'd * 'e) -> unit

(** Default version of pp_iterable_generic *)
val pp_iterable:
  (('value -> unit) -> 't -> unit) ->
  (Format.formatter -> 'value -> unit) ->
  Format.formatter -> 't -> unit

(** Default version of pp_iterable_mapping_generic *)
val pp_iterable_mapping:
  (('key -> 'value -> unit) -> 't -> unit) ->
  (Format.formatter -> 'key -> unit) ->
  (Format.formatter -> 'value -> unit) ->
  Format.formatter -> 't -> unit

