# OColor

OColor is an OCaml library which help to format nicely using ANSI escape codes.

## Subset of ANSI

With OColor, you can print... colors. That's the least you can expect. But there
is more. It also support a lot of style such as bold, underline, italic, faint,
overline.... And 4, 8 and 24- bits colors. In fact, OColor is probably capable
of more than your terminal allows. However, it does not support every SGR code
(the family of ANSI sequences that does styling). The supported subset was
minutely chosen with a combination of "Do I understand what it does?", "Do I
know someone that understand it?", "Is it supported by at least one terminal my
coworker or I use or know?", "Will I use it at least once?", "How much time do I
have?"....

The general philosophy is that there must be at least what you need, but use
with care, OColor does not detect if your terminal support what you're asking
him. However, most terminals will simply ignore unknown sequences, so it's
probably not so bad anyway. Portability is user's problem.

## About colors

In ANSI, there are 3 ways to describe colors.
- 4 bits: the old one, 16 available colors that are "named" by their number, the
real rendered color depend on the terminal configuration.
- 8 bits: provides an access to 4 bits colors, an rgb (6^3) cube and a
24 steps grayscale. The former depend on the terminal, the two latter are
absolute.
- 24 bits: true rgb colors (256^3). Obviously absolute.

OColor has a quite nice feature: you can tell it which is the better kind of
colors you support and will convert colors appropriately. In fact, it only
perform downgrade.

Nowadays, most terminal support 24 bits colors, but not all. Some support only 4
and 8 bits, and, worse, sometimes, only 4 bits. So when printing a color, OColor
look if it is available given the configuration (that have to be set manually)
and, if not, find the closest allowed color. The metric is the euclidean
distance in the CIELAB colorspace (under the standard illuminant D65). Should be
good enough.

A more pragmatic problem: when downgrading a 24 into a 4 bits colors, you have
to know how these 4 bits colors are rendered. This is configurable as well. This
is especially important because it may vary a lot between terminals with white
and black background.

## Usage

There is two way to use OColor: with `Printf` or `Format`. `Format` is good.
`Format` is better. `Format` will win the final battle! Oh, and before we go in
the details, most of the mechanism is exposed for the developper, even things
that are not used in the library actually, so it should be possible to get what
you want of OColor.

### `Format`

To format with style with `Format`, we use semantic tags. Semantic tags are a
bit like boxes: you can open them, with a tag (a string annotating it) and close
them. They are handled by user-provided functions, and disabled by default.

We can open a semantic tag, with tag "foo", with a format containing `@{<foo>`,
and we close the last opened tag with `@}`. There is another way to do it:
```ocaml
val Format.pp_open_tag : Format.formatter -> string -> unit
val Format.pp_close_tag : Format.formatter -> unit -> unit
```

Thus, we have access to an almost-well-parenthesized structure (because closing
a tag when none are open is legal and does nothing), that we can use to print
with styles in a compositional way. Opening a tag will apply a new style, and
closing it will cancel the effect, restoring the previous style, if any.

A simple example:
```ocaml
Ocolor_format.printf "Normal @{<red>This is red @{<green> Here is green@} Red again@} Normal"
```
Better:
```ocaml
Ocolor_format.printf "Normal @{<red>This is red @{<ul;bold> Here is red, bold and underlined@} Only red again@} Normal"
```

To make styling work, you'll have to use `Ocolor_format.printf` or use standard
`Format` functions with a formatter that has been prettified with
`Ocolor_format` functions. So, everything, especially `%a` will work as
expected: called with a prettified formatter, tags will be interpreted,
otherwise, text will be printed without formatting. Here is an example of usage
with `Format` functions.

```ocaml
(* Non prettified formatter does not style. *)
Format.printf "Normal @{<red>This is normal@} Normal";
(* Format function with a predefined prettified formatter *)
Format.fprintf Ocolor_format.raw_std_formatter "Normal @{<red>This is red@} Normal"; (* Exactly as Ocolor_format.printf *)
(* Format function with a arbitrary formatter *)
Ocolor_format.prettify_formatter (fmt: Format.formatter); (* We assume fmt is defined somewhere *)
Format.fprintf fmt "Normal @{<red>This is red@} Normal";
(* Prettified formatter from a buffer *)
let fmt2 : Format.formatter = Ocolor_format.raw_formatter_of_buffer (Buffer.create 64) in
(* Could have be done with:
let fmt2 : Format.formatter = Format.formatter_of_buffer (Buffer.create 64) in
Ocolor_format.prettify_formatter fmt2;
*)
Format.fprintf fmt2 "Normal @{<red>This is red@} Normal"
```

Prettifying an already prettified formatter will reset the internal state but
not reset the output. So it will stay dirty until the whole styling is
recomputed, for instance, by closing the bold style. Since old styles are
forgotten, there are no way to close them gracefully. So, don't do that.

`Ocolor_format` also define a non standard `formatter` type that contains a
standard `Format.formatter` and some information about the state. It is useful
only for introspection: get the current style from the formatter. Otherwise,
only the standard (underlying) formatter is required.

More details in `ocolor_format.mli`.

### `Printf`

`Printf` has no semantic tags, so it does styling the old way: applying style
linearly, and forgetting the past. That's dangerous. Use with care.

When you apply a style, the previous style is totally forgotten. Thus, if you
want to restore the previous style, you have to do it manually. And of course, a
reset will really reset everything. Thus, this is dangerously non compositional.

I recommend to use it only when printing self-contains lines, only at the
toplevel of the printing process (not in a printer that will be called by a
  `%a`).

More details in `ocolor_printf.ml`.

## Beware

For now, when a formatter is prettified, tag-marking functions are overwritten
and tag-marking is enabled. It is yet not currently supported to call both
new and previous tag-marking functions, since it does not seem useful. However,
if needed, we could let the user decide if we should replace tag-marking
functions, call the previous ones before or after, or call after if styling
fails (unrecognized tag).

## License

OColor is under MIT Licence. In short, feel free to use it in pretty much every
way you want. I don't really care a lot.

You can also consider it as a mere example of what semantic tags are good to.
