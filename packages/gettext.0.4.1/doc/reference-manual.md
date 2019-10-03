# OCaml-gettext reference manual

[gettext documentation]: http://www.gnu.org/software/gettext/manual/gettext.html
[ocaml-gettext-options manpage]: ocaml-gettext.5.md

Overview
--------

### What is ocaml-gettext?

ocaml-gettext is a library to support string translation in OCaml. It
provides a simple interface to help programmers and translators to
create programs that can support different languages. This allows the
internationalization of programs.

ocaml-gettext provides two main features:

- translate English strings into localized strings (depending on which
  gettext data are installed),
- convert charset from the one used by the translator into the charset
  of the user.

The library is build according three points of view:

- for the programmer: this library provides functions that can be used
  in OCaml,
- for the translator: this library provides a standard file format
  (PO file) to help the translator,
- for the user: this library provides a set of command line options to
  set the language and the charset.

ocaml-gettext was initially only a wrapper of gettext. It comes with a
patch against the source of xgettext to add support of the OCaml
language. As i already used this approach, i was convinced that this
library should be better integrated by using more advanced features of
OCaml (such as `camlp4`).

As a result of porting gettext to ocaml-gettext, we have:

- a library that can understood the native format of gettext file (MO
  file),
- two implementations: a wrapper around gettext and a pure OCaml
  implementation using camomile,
- `ocaml-gettext`: a command line tool to help you to extract, to
  merge and to install gettext data.

### How is ocaml-gettext related to gettext?

ocaml-gettext is a close cousin of gettext. In fact, the API is based on
gettext. Almost everything in ocaml-gettext is compatible with gettext:

- functions provided in the API are very close to the gettext one,
- the library contains a binding of the gettext library,
- all the file used are gettext compatible: the library uses PO file
  for translator and MO file for translation,
- the library tries to use the same initialisation sequence as
  gettext: it uses the same environment variable, tries to find MO
  files in the same places...

This documentation will not covered the point that are better explained
in the [gettext documentation]. It is highly recommended to read this
documentation, before reading this manual. Most of the point that are
explained there are not explained again here. However, we have tried to
be as precise as possible to enable programming with ocaml-gettext
without having the need to be a gettext expert.

Programming with ocaml-gettext
------------------------------

### Overview

The API of ocaml-gettext is really reduced. It is made on purpose. The
design is heavily based on modules and functors. There is no real reason
for this design, it was just really useful at the time the code was
written.

The library supposes that all the `textdomain` that will be used during
translation, are declared before using it. It is a real constraint, but
it enables more optimisation. Moreover, it allows having a more
"functional" use.

First of all, a parameter `t` should be defined. This parameter holds
all the required value to initialise ocaml-gettext. In particular, it
contains information about:

- which textdomain will be used,
- which language will be used,
- how the error should be handle,
- which directory to search.

This parameter is build and updated through internal functions. You
don't have direct access to it.

The parameter `t` is not directly used for translation. It must be
converted into a parameter `t'` which is a real function to access
translation. The transformation from `t` to `t'` is handled through a
function `realize`. The parameter `t'` is used in low level translation.

All the work of the library is done in the function `realize`. This
function is not provided in the base package. It is build out of real
implementation of ocaml-gettext (such as `gettext-camomile` or
`gettext-stub`). This function could handle all the parameters in
different ways. Concerning `gettext-camomile`, it builds a translation
table for all the files found which correspond to a declared
`textdomain`.

Since it should be very difficult to pass a parameter `t` or `t'` in all
functions that should use translation, we provide a more simple way to
use the library. The top level functions use a global reference to store
the parameters `t` and `t'`. This helps to integrate ocaml-gettext more
easily into existing application.

### Makefile and source layout

The source layout should conform to the one described in [gettext
documentation]. In particular, it should contain a `po` directory.
There should be in this directory :

- a file `LINGUAS` describing available translations,
- a file `POTFILES` containing the listing of all files that contain
  translatable strings,
- a `Makefile` to build the whole thing,
- a set of PO file which contains translated strings for
  different languages.

During the build, the `Makefile` should generate a file
`your-domain.pot` that contains a template PO file that can be used by
translator.

To build programs and libraries with ocaml-gettext, the preferred way is
to use ocamlfind. There are five findlib packages:

- gettext.base: the base package of ocaml-gettext. It contains the top
  level type required to compile any library,
- gettext.extension<sup>[1](#1)</sup>: a package used to extend
  ocaml-gettext. It is reserved for very particular functions (such as
  creating a new `realize` function),
- gettext.extract: a package that enables to create special `camlp4`
  program (such as `ocaml-xgettext`),
- gettext-stub: an implementation of ocaml-gettext using gettext,
- gettext-camomile: an implementation of ocaml-gettext using camomile.
  It is a pure ocaml implementation.

In order to link an application or a library using ocaml-gettext, you
should link with one of : gettext.base, gettext-camomile or
gettext-stub.

### Library

Library should use the module `Gettext.Library`. It doesn't need any
real implementation of ocaml-gettext. By this way, you can let the
library user choose the most appropriate ocaml-gettext implementation.
This point is essential : a library could be used as well in a GUI
program or in short run command line program. These two examples don't
require the same kind of implementation at all: GUI program loads most
of their translated strings, command line program only use one among
them. So by using the module `Gettext.Library` framework, you don't
restrict programs to use one particular implementation of ocaml-gettext.

The library should define, using the functor `Init` provided:

- his textdomain through the `textdomain` value,
- his dependencies through the `dependencies` value,
- if needed his charset and directory binding (but it is not
    recommended to do so).

After having instantiated the module `Gettext.Library` with the
appropriate `Init`, you should use the function provided :

- `s_`: for translating singular strings,
- `f_`: for translating singular strings which will be used with `Printf`
  function<sup>[2](#2)</sup>,
- `sn_`: for translating plural strings,
- `fn_`: for translating plural strings which will be used with
  `Printf` function,

> **Warning**
>
> You must keep the function name `s_`, `f_`, `sn_` and `fn_`. The
> extraction of translatable strings matches these names. If you don't
> keep it, the extraction of translatable strings will fail.

All the calls to translation functions, use the textdomain provided in
`Init`.

The only constraint when using ocaml-gettext in your library is to
provide an access to the value `Gettext.Library.init`. This value is
used as dependencies for other libraries and programs that depend on it.
For example, since you use the library ocaml-gettext, your primary
dependency is the function `init` provided in the top level (the
function `Gettext.string_of_exception` is localised).

> **Warning**
>
> If you distribute your library, don't forget to mention that
> ocaml-gettext will only be able to translate the string defined in
> your library, if and only if the MO file build with is also installed.
> If not installed ocaml-gettext is useless.

### Program

Program should use ocaml-gettext just as libraries do. The only
difference lies in the fact that you should provide a `realize` function
in the `InitProgram`. The other difference is that the `init` value is
not a dependency that should be used by other program. It is a `Arg`
usable value. It allows user to define some important parameters.

```shell
$>./program --help

    --my-name name                      Your name. Default : ""
    --gettext-failsafe {ignore|inform-stderr|raise-exception}
                                        Choose how to handle failure in ocaml-gettext. Default: ignore.
    --gettext-disable                   Disable the translation perform by ocaml-gettext. Default: enable.
    --gettext-domain-dir textdomain dir Set a dir to search ocaml-gettext files for the specified domain. Default: [  ].
    --gettext-dir dir                   Add a search dir for ocaml-gettext files. Default: [ "/usr/share/locale";
                                        "/usr/local/share/locale" ].
    --gettext-language language         Set the default language for ocaml-gettext. Default: (none).
    --gettext-codeset codeset           Set the default codeset for outputting string with ocaml-gettext. Default: ISO-8859-1.
    -help                               Display this list of options
    --help                              Display this list of options
```



If you want to include a manpage (or info file), that describes the
command line option of ocaml-gettext, you should use the Docbook XML
fragment distributed with this application. Docbook should be enough
generic to allow you to link it into your documentation. If you don't
want to link it with your documentation, you can refer to
[ocaml-gettext-options manpage].

You should take care of what implementation of ocaml-gettext you are
using. In order to choose the right implementation you should consider
your program and every characteristic of it (how many strings does it
need to fetch? Does it use already a C library that link with gettext?).

- GettextCamomile.Map
  - Pure OCaml implementation,
  - Full load of all MO files before any translation,
  - Use OCaml standard `Map`.
  - Usage
    - Pure OCaml program,
    - Program that requires to translate a lot of strings,
    - Threaded program (since it uses OCaml Map, it should be thread
      safe without problem).
- GettextCamomile.Hashtbl
  - Pure OCaml implementation,
  - Full load of all MO file before any translation,
  - Use OCaml standard `Hashtbl`.
  - Usage
    - Pure OCaml program,
    - Program that requires to translate a lot of strings,
    - Should work with threaded program, provided that the `Hashtbl`
      works in threaded environment.
- GettextCamomile.Open
  - Pure OCaml implementation,
  - Load strings from MO if needed,
  - Use OCaml standard `Hashtbl`,
  - Use a dichotomic search for the strings,
  - Compute MO file to open at initialisation,
  - Open a file when fetching string,
  - Doesn't memorize already translated strings,
  - Implementation design copied from gettext.
  - Usage
    - Pure OCaml program,
    - Program that require to translate very few strings,
    - Should work with threaded program, provided that `open_in`
      function call work.
- GettextStub.Native
  - Native gettext library,
  - Partial load of all MO file before any translation, use `mmap`.
  - Usage
    - OCaml program that uses library compiled with gettext,
    - Should work with threaded program, provided that the `gettext`
      work in threaded environment. To support a language, the
      corresponding locales need to be generated.
- GettextStub.Preload
  - Native gettext library,
  - Forced load of all MO file before any translation, the preload
    is realized by trying to load the string "" for all the
    textdomain defined.
  - Usage
    - OCaml program that uses library compiled with gettext,
    - Program that needs to translate a lot of strings,
    - Should work with threaded program, provided that the `gettext`
      work in threaded environment. To support a language, the
      corresponding locales need to be generated.

### Graphical user interface

Graphical user interface works just as a program or a library. The only
difference is that the file which contains the graphical user interface
should not be written in OCaml. You have two cases :

- You use glade file, so you should extract the strings from this file
  using `xgettext`
- You use a hand written interface written in OCaml, the extraction of
  the strings follow the same way as a library.

You should use the first alternative : it is easier for a translator to
extract strings, without having to compile your application (it enables
translators that don't know OCaml to help you). In order to do so, you
should use the native `xgettext` binary (provided with gettext). It
should support the format of the translatable strings found in your GUI
file (for example, `xgettext` supports glade file).

But for now you can only use the second alternative, because OCaml is
not yet supported in `xgettext`. This should be fixed, once
`ocaml-gettext` will be enough stable to become a back-end for
`xgettext`<sup>[3](#3)</sup>.

In the two cases, you just have to add your GUI file (in OCaml or native
form) to `POTFILES`.

Graphical user interfaces are good candidates for settings a fixed
`Init.codeset`. Typically, GTK2 interfaces require to have these
parameters set to UTF-8.

```ocaml
open GuiGettext.Gettext;;

(* Give access to the init of GuiGettext *)
let init =  Gettext.init

(* Build a simple window that display your name *)
let hello_you name =
  let spf x = Printf.sprintf x in
  let window =
		GWindow.window ~title:(s_ "Hello world !") ~border_width:12 ()
  in
  let label =
    GMisc.label ~text:(spf (f_ "Hello %s") name) ~packing:window#add ()
  in
  ignore (window#event#connect#delete ~callback:(fun _ -> false));
  ignore (window#connect#destroy ~callback:(fun _ -> GMain.Main.quit ()));
  window#show ();
  GMain.Main.main ()
```

`./program --gettext-dir ../build/share/locale --gettext-lang C --my-name Sylvain`:<br>
![Default GUI, not translated](gui.png "GUI")

`./program --gettext-dir ../build/share/locale --gettext-lang C --my-name Sylvain`:<br>
![GUI translated to french](gui-fr.png "GUI fr")

> **Warning**
>
> If you build lablgtk application, you must keep in mind that some
> locale settings are made in the function `GMain.Init`. Those settings
> could override those done through the command line options. In order
> to correctly use ocaml-gettext, you must be sure to call `GMain.Init`
> before using `Arg.parse` with the ocaml-gettext args.

Translating ocaml-gettext programs and libraries
------------------------------------------------

ocaml-gettext has been built around gettext. This allows translators to
use exactly the same technics as they should use with gettext. All the
documentation required for translating can be found in [gettext
documentation].

It is recommended to use GUI which allows easier translation such as :

- [gtranslator](http://gtranslator.sourceforge.net/),
- [kbabel](http://i18n.kde.org/tools/kbabel/).

### Using ocaml-gettext programs

ocaml-gettext program can be used just as any OCaml program. The only
difference with standard OCaml program is that they come with a bunch of
command line options which are specific to OCaml program. In most cases,
you just have to define a well suited `LC_ALL` or `LANG` environment
variable. Since ocaml-gettext is compatible with gettext, if your
environment variable works with gettext, it should also works with
ocaml-gettext.

You can find more details in the [gettextdocumentation] or in the
[ocaml-gettext-options manpage].

Tips and tricks
---------------

### Adding gettext support without depending on ocaml-gettext

You want to 'gettextify' your program, adding 's\_' and 'f\_'
annotations throughout. However now your program has an additional
dependency, ocaml-gettext. You can make ocaml-gettext optional by
creating a set of dummy functions which do nothing:

```ocaml
(* This file is generated automatically by ./configure. *)
module Gettext =
struct
  external s_ : string -> string = "%identity"
  external f_ : ('a -> 'b, 'c, 'd) format -> ('a -> 'b, 'c, 'd) format = "%identity"

  let sn_ : string -> string -> int -> string =
    fun s p n ->
      if n = 1 then s else p

  let fn_ : ('a -> 'b, 'c, 'd) format -> ('a -> 'b, 'c, 'd) format -> int -> ('a -> 'b, 'c, 'd) format =
    fun s p n ->
      if n = 1 then s else p
end
```


You have to arrange for your configure script to place the above content
or the real ProgramGettext module into 'prog\_gettext.ml', depending on
whether it detects that ocaml-gettext is installed (eg. using 'ocamlfind
query gettext' or some other method).

Links
-----

[Gettext documentation](http://www.gnu.org/software/gettext/manual/gettext.html)

<a name="1">1</a>: This feature is described here for your information
    only. Since it belongs to low level implementation of ocaml-gettext,
    it should not be used.

<a name="2">2</a>: Strings which should be used by `Printf` function are
    checked to be sure that the returned strings are equivalent to the provided
    English string. In particular, every "%"-symbol should be the same
    in the provided string and the returned string. If not, it is the
    untranslated string which is returned.

<a name="3">3</a>: For now, extracting strings from OCaml source file and glade
    file, requires to patch gettext. You can find this path in patches. This
    patch will be sent to upstream author once it will be considered
    enough stable.
