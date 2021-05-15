# typeset-ocaml
An embedded DSL for defining source code pretty printers implemented in OCaml

## Concept
The layout language is designed such that it fits well over a structurally recursive pass of some inductive data-structure; an abstract representation of the thing you wish to pretty print.

A layout is a tree of text literals composed together with either padded, unpadded compositions or with a line-break. The layout solver will select compositions in a layout and convert them into line-breaks, in order to make the layout fit within a given layout buffer width. It will do this in a greedy way, fitting as many literals on a line as possible. While doing so it will respect the annotated properties that the compositions are constructed under.

The solver being an abstract concept, is concretely implemented via two accompanying functions, a _compiler_ implemented as __compile__, and a _renderer_ implemented as __render__.

## Grammar
```text
null      (Constructor for the empty layout)
~$x       (Constructor for a word/text layout literal over a string x)
fix u     (Constructor for a fixed layout over a layout u)
grp u     (Constructor for a group layout over a layout u)
seq u     (Constructor for a sequence layout over a layout u)
nest u    (Constructor for a indented/nested layout over a layout u)
pack u    (Constructor for a indexed margin layout over a layout u)
u </> v   (Forced linebreak composition of layouts u and v)
u <&> v   (Unpadded composition of layouts u and v)
u <!&> v  (Infix fixed unpadded composition of layouts u and v)
u <+> v   (Padded composition of layouts u and v)
u <!+> v  (Infix fixed padded composition of layouts u and v)
```

## Null constructor
Sometimes in a data-structure there might be optional data (e.g. of type "string option"), which when omitted should not have a layout. To make this case easy to handle, the __null__ element of layout composition was added. The alternative would have been that you would need to keep track of an accumulator variable of the so-far-built layout in your layout function.

```OCaml
let layout_option maybe_string =
  match maybe_string with
  | None -> null
  | Some text -> ~$text
```
Versus, e.g:
```OCaml
let layout_option maybe_string result =
  match maybe_string with
  | None -> result
  | Some text -> result <+> ~$text
```

The __null__ will be eliminated from the layout by the compiler, and will not be rendered, e.g:
```OCaml
let foobar = ~$"foo" <&> null <&> ~$"bar"
```

When rendering __foobar__, when the layout fits in the layout buffer, the result will be:
```Text
       7
       |
foobar |
       |
```

## Word literal constructor
These are the visible terminals that we are typesetting.
```OCaml
let foo = ~$"foo"
```
When rendering __foo__, when the layout fits in the layout buffer, the result will be:
```Text
    4
    |
foo |
    |
```
It will simply overflow the buffer when it does not:
```Text
  2
  |
fo|o
  |
```

## Fix constructor
Sometimes you need to render a part of some layout as inline, i.e. that its compositions should not be broken; this is what the __fix__ constructor is for. In other words a fixed layout is treated as a literal.

```OCaml
let foobar = fix (~$"foo" <&> ~$"bar")
```

When rendering the fixed layout __foobar__, when the layout fits in the layout buffer, the result will be:
```Text
       7
       |
foobar |
       |
```
It will overflow the buffer when it does not:
```Text
  2
  |
fo|obar
  |
```

## Grp constructor
The __grp__ constructor prevents the solver from breaking its compositions, as long as there are compositions to the left of the group which could still be broken. This is useful when you need part of the layout to be treated as an item.

```OCaml
let foobarbaz = ~$"foo" <&> grp (~$"bar" <&> ~$"baz")
```

When rendering __foobarbaz__, when the layout fits in the layout buffer, the result will be:
```Text
          10
          |
foobarbaz |
          |
```
If one of the literals does not fit within the layout buffer, the result will be:
```Text
       7
       |
foo    |
barbaz |
       |
```
In contrast, had the group not been annotated, the result would have been:
```Text
       7
       |
foobar |
baz    |
       |
```
Since the composition between _bar_ and _baz_ was not guarded, and the layout solver is greedy and wants to fit as many literals on the same line as possible without overflowing the buffer. If the group still does not fit within the layout buffer, the group will be broken and the result will be:
```Text
    4
    |
foo |
bar |
baz |
    |
```

## Seq constructor
The __seq__ constructor forces the solver to break all of its compositions as soon as one of them is broken. This is useful when you have data that is a sequence or is list-like in nature; when one item in the sequence is put on a new line, then so should the rest of the items in the sequence.

```OCaml
let foobarbaz = seq (~$"foo" <&> ~$"bar" <&> ~$"baz")
```

When rendering __foobarbaz__, when the layout fits in the layout buffer, the result will be:
```Text
          10
          |
foobarbaz |
          |
```
If one of the literals does not fit within the layout buffer, the result will be:
```text
       7
       |
foo    |
bar    |
baz    |
       |
```
Since the compositions were part of a sequence; i.e when one of them broke, they all broke.

## Nest constructor
The __nest__ constructor is simply there to provide an extra level of indentation for all literals that it ranges over. The width of each level of indentation is given as a parameter to the __render__ function.

```OCaml
let foobarbaz = ~$"foo" <&> nest (~$"bar" <&> ~$"baz")
```

When rendering __foobarbaz__ with a indentation width of 2, when the layout fits in the layout buffer, the result will be:
```Text
          10
          |
foobarbaz |
          |
```
If one of the literals does not fit within the layout buffer, the result will be;
```Text
       7
       |
foobar |
  baz  |
       |
```
And when the layout buffer will only hold one of the literals, the result will be:
```Text
    4
    |
foo |
  ba|r
  ba|z
    |
```
In this case _bar_ and _baz_ will overflow the layout buffer because of the given indentation.

## Pack constructor
The __pack__ constructor defines an indentation level, but implicitly sets the indentation width to the index of the first literal in the layout it annotates. This is e.g. useful if you are pretty printing terms in a lisp-like language, where all other arguments to an application is often "indented" to the same buffer index as the first argument.

```OCaml
let foobarbaz = ~$"foo" <&> pack (~$"bar" <&> ~$"baz")
```

When rendering __foobarbaz__, when the layout fits in the layout buffer, the result will be:
```Text
          10
          |
foobarbaz |
          |
```
When one of the literals do not fit, the result will be:
```Text
       7
       |
foobar |
   baz |
       |
```
When the layout buffer will only hold one literal, the result will be:
```Text
    4
    |
foo |
bar |
baz |
    |
```
The calculation of which buffer index to indent to is:
```
max (indent_level * indent_width) mark
```
I.e the mark index will only be chosen if it is greater than the current indentation.

## Forced linebreak composition
The forced linebreak composition does just that, it is a pre-broken composition.

```OCaml
let foobar = ~$"foo" </> ~$"bar"
```

When rendering __foobar__, when the layout fits in the layout buffer, the result will be:
```Text
        8
        |
foo     |
bar     |
        |
```

## Unpadded composition
The unpadded composition will compose two layouts without any whitespace.

```OCaml
let foobar = ~$"foo" <&> ~$"bar"
```

When rendering __foobar__, when the layout fits in the layout buffer, the result will be:
```Text
        8
        |
foobar  |
        |
```

## Padded composition
The padded composition will compose two layouts with whitespace.

```OCaml
let foobar = ~$"foo" <+> ~$"bar"
```

When rendering __foobar__, when the layout fits in the layout buffer, the result will be:
```Text
        8
        |
foo bar |
        |
```

## Infix fixed compositions
The infix fixed compositions are syntactic sugar for compositions where the leftmost literal of the left operand, and the rightmost literal of the right operand are fixed together. I.e. the two following layouts are equivalent:

```OCaml
let foobarbaz1 = ~$"foo" <&> ~$"bar" <!&> ~$"baz"
let foobarbaz2 = ~$"foo" <&> fix (~$"bar" <&> ~$"baz")
```

The example above might make it seem trivial, and that infix fixed compositions do not give you much value; but remember that you are composing layouts, not just literals. As such normalising the infix fixed composition is actually quite challenging since there are many different cases to consider when the fix is "sunk in place" in the layout tree; this is part of what the compiler is responsible for.

Infix fixed compositions are useful when you need to fix a literal to the beginning or end of some other layout, e.g. separators between items in a sequence or list-like data structure. Without this feature you would again need to use an accumulator variable if you want to fix to the next literal, and probably need continuations if you want to fix to the last literal.

## Solving the layout
Your custom layout function (pretty printer) will build a layout, which you then need to compile and render:
```OCaml
...
compile layout @@ fun document ->
render document 2 80 @@ fun result ->
...
```
I.e. the layout should be given to the compiler, which gives you back a document ready for rendering, which you in turn give to the renderer along with arguments for indentation width and layout buffer width; in the above case indentation width is 2 and the layout buffer width is 80.

The reason for splitting the solver into __compile__ and __render__, is in case the result is to be displayed in a buffer where the width is variable; i.e. you will not need to re-compile the layout between renderings using varying buffer width.

## Examples
For some examples of how to put all these layout constructors together into something more complex and useful, please reference in the examples directory.
