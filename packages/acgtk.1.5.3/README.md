# ACGtk: an ACG development toolkit

**ACGtk** is a software package ([2008-2021 INRIA](http://www.inria.fr)©) for the development of abstract categorial grammars. This distribution provides two executables file: `acgc` and `acg`.

It is distributed with the *CeCILL* license (see the [LICENSE](LICENSE.en) file or http://www.cecill.info). Contributors are listed in the [AUTHORS.md](AUTHORS.md) file.

A list of related publications is available at the [ACG web page](http://calligramme.loria.fr/acg).


## acgc

`acgc` is a "compiler" of ACG source code, i.e. files containing definitions of signatures and lexicons. It basically checks whether they are correctly written (syntactically and wrt types and constant typing) and outputs a `.acgo` object file. An interactive mode is available to parse terms according to signatures.

Run
```bash
./acgc --help
```
to get help.

## acg

`acg` is an interpreter of command meant to be useful when using ACGs. To get a list of command, run
```bash
./acg
```
then, on the prompt, type
```
	help;
```

Example files are given in the [examples directory](examples). Read the [README.md file](examples/README.md).

### Basic usage

Let's assume you defined a file `my_acg.acg` in directory `my_dir`. A basic usage of the `acgc` and `acg` commands could be:
```bash
$ acgc -o my_acg.acgo my_acg.acg
```
This will produce a `my_acg.acgo` file (note that this is the default name and location if the `-o` option is not provided).

Then, running :
```bash
$ acg
```
will open a prompt in which you can type:
```ocaml
# load o my_acg.acgo;
```
to load the data contained in the `my_acg.acg` file. Assuming you have defined the signature `Sig` and the lexicon `Lex`, you can then run the following commands:
```ocaml
# Sig check lambda x.some_cst x: NP ->S;
```
to check whether `lambda x.some_cst x` is a term of type `NP ->S` according to `Sig`.

You can type:
```ocaml
# Lex realize lambda x.cst x: NP ->S;
```
to compute the image of `lambda x.cst x` by `Lex` (assuming this term and this type are correct according to the abstract signature of `Lex`).

You can type:
```ocaml
# Lex parse John+loves+Mary: S;
```
to check whether the term `John+loves+Mary` has an antecend of type `S` by `Lex`, assuming that `John+loves+Mary` is a term of type `Lex (S)` in the object signature of `Lex`.

Type `CTRL-D` to exit from the program, or type:
```ocaml
# exit;
```

### SVG output

If the `--nsvg` option is not set when running `acg`, a file `realize.svg` (default name) is generated in the current directory whenever a `realize` command is invoked. In order to set another file name, use the option `--svg other_filename`.

This files contains a representation as a tree of the operations described by the term to realize (applications, abstractions). Each
node contains the abstract term and its realizations by each of the lexicons specified on the command line. The graphic file can for
instance been observed through a web browser.

Four rendering engines are available so far to render the terms in each node:

* the default engine: just generates a lambda-term following the signature/lexicon syntax

* the "logic" engine: formulas are rendered as logical formulas: non logical constants are in bold font, logical connectives are rendered using utf-8 if their names are as follows:

  * "Ex" -> "∃"
  * "ExUni" -> "∃!"
  * "Ex_l" -> "∃ₗ"
  * "Ex_t" -> "∃ₜ"
  * "All" -> "∀"
  * "All_t" -> "∀ₜ"
  * "TOP" -> "⊤"
  * "The" -> "ι"
  * "&" -> "∧"
  * ">" -> "⇒"
  * "~" -> "¬"

* the "trees" engine: terms are rendered as trees (e.g., derivation trees)

* the "unranked trees": terms are rendered as trees, but if a non-terminal is defined as `[a-zA-Z]+[0-9]*`, it is rendered only using
the `[a-zA-Z]` part.

The association between the name of a signature and a rendering engine is declared in a configuration file that can be loaded through the `--realize` option and that looks like:
```bash
$ cat config.json
{
    "signatures": [
	{ "name": "TAG", "engine": "trees" },
	{ "name": "DSTAG", "engine": "trees" },
	{ "name": "CoTAG", "engine": "trees" },
	{ "name": "derivations", "engine": "trees" },
	{ "name": "strings", "engine" : "strings"},
	{ "name": "Strings", "engine" : "strings"},
	{ "name": "logic", "engine" : "logic"},
	{ "name": "low_logic", "engine" : "logic"},
	{ "name": "derived_trees", "engine" : "unranked trees"},
	{ "name": "Derived_trees", "engine" : "unranked trees"},
	{ "name": "trees", "engine" : "unranked trees"}
    ],
  "colors": {
      "node-background": (239, 239, 239),
      "background": (255,255,255)
  }
}
```
An example file is given in [examples/config.json](examples/config.json)

## ACG emacs mode

There is an ACG emacs mode `acg.el` in the [emacs](emacs) directory.

Look at the [INSTALL.md](INSTALL.md) file to see how to install it and where you can find the `acg.el` file if automatically installed (in particular using opam).

It's main feature is to be loaded when editing an acg data file (with signatures and lexicons). It is automatically loaded for files with a `.acg` extension

It basically contains compilation directives and next-error searching.
1. First load an acg file
2. then run `M-x compile` (or `C-c C-c`) to call the compiler (`acgc`)
3. then run `M-x next-error` (or ``C-x` `) to search for the next error (if any) and highlights it

## Syntax of signature and lexicons

(See the [examples/tag.acg](examples/tag.acg) file for an example).

### Signatures are defined by:
```
signature my_sig_name=
	sig_entries
end
```

`sig_entries` is a list of `sig_entry`, separated with a `;`. A `sig_entry` can be:
* a type declaration as in
  ```
  NP,S : type;
  ```
* a type definition as in
  ```
  o :type;
  string = o -> o;
  ```
  Note that type constructors are `->` and `=>` for the linear and intuitionistic arrows respectively.

* a constant declarations as in
  ```
  foo:NP;
  bar,dummy:NP -> S;
  infix + : string -> string -> string;
  prefix - : bool -> bool;
  binder All : (e =>t) -> t;
  infix > : bool -> bool -> bool; (*This means implication*)
  ```
  Note that `infix` and `prefix` are keywords to introduce symbols (of **length 1**. This probably will change). Also notes that comments are surrounded by `(*` and `*)`.

* constant definitions as in
  ```
  n = lambda n. bar n : NP -> S;
  infix + = lambda x y z.x(y z): string -> string -> string;
  prefix - = lambda p.not p:bool -> bool;
  everyone = lambda P. All x. (human x) > (P x) ;
  ```
  Note the syntax for binders (`All` in the last example). Available construction for terms are:
  * `lambda x y z.t` for linear abstraction
  *	`Lambda x y z.t` for non-linear abstraction
  *	`t u v` for application (equivalent to to `(t u) v`)
  * `t SYM u` if `SYM` is a infix symbol (lowest priority). It is equal to `((SYM) t) u` where `SYM` is used as usual constant, with the priority of application
  * `SYM t` if `SYM` is a prefix symbol (highest priority)
  *	`BINDER x y z.t` if `BINDER` is a binder

* About associativity and precedence of operators

  Prefix operators have precedence over application, and application has precedence over infix operators. Relative precedence among infix operators can be defined.

  When no associativity specification is set, the default is left associative.

  When no precedece definition is given, the default is higher precedence over any infix operator defined so far.

  When declaring or defining an infix operator with the keyword 'infix', the optional specification for the associativity and the relative precedence can be set.

  A specification is given between square brackets. The syntax is as follows:
  ```
  infix [specification] SYM …
  ```
  (the remaining part of the declaration is the same as without the specification)

  A specification is non-empty comma-separated list of:

  + an (optional) associativity specification, given by one of the keywords `Left`, `Right`, or `NonAssoc`. If not present, left associativity is set by default to infix operators

  + an (optional) precedence declaration (if not present, the highest precedence over all the infix operators defined so far is given). It is defined as `< SYM` (where `SYM` is a symbol). It assigns to the operator being declared or defined the greates precedence *below* the precedence of `SYM`.

  It is possible to use an infix symbol as a normal constant by surrounding it with left and right parenthesis, so that	 `t SYM u = (SYM) t u`

  See [examples/infix-examples](examples/infix-examples) and [examples/infix-examples-script](examples/infix-examples) for examples.

### Lexicons

There are two ways to define a lexicon:
1. By using the keyword `lexicon` or `nl_lexicon` as in :
   	```
	lexicon my_lex_name(abstract_sig_name) : object_sig_name =
		lex_entries
	end
	```
	or
	```
	nl_lexicon my_lex_name(abstract_sig_name) : object_sig_name =
		lex_entries
	end
	```
	With the `lexicon` keyword, `lambda` (resp. `->`) is interpreted as `lambda` (resp. `->`), whereas with `nl_lexicon`, `lambda` (resp. `->`) is interpreted as `Lambda` (resp. `=>`). I.e., everything is interpreted non linearly. It is useful when not interested in linear constraints in the object signature (as, for instance, in the context-free lambda grammars).

	`Lex_entries` is a list of `lex_entry`, separated with a `;`. A `lex_entry` can be of the following forms:
	* `abstract_atomic_type1, abstract_atomic_type2 := object_type;`
	* `abstract_const1, abstract_const2 := object_term;`

2. By lexicon composition as in:
   ```
   lexicon my_new_lex = lex_2 << lex_1
   ```

## Keywords

The keywords are `signature`, `lexicon`, `nl_lexicon`, `end`, `type`, `prefix`, `infix`, `binder`, `lambda`, and `Lambda`.

The reserved symbols are `=`, `<<`, `;`, `:`, `,`, `(`, `)`, `.`, `->`, `=>`, and `:=`.

Inside a signature or a lexicon, `signature`, `lexicon` and `nl_lexicon` are not considered as keywords and can be used as identifier.

Other keywords can be used as identifier when escaped with `\` (e.g., `\end`).
