# Chase: A Model Finder for Finitary Geometric Logic

<p align="center">John D. Ramsdell<br>
The MITRE Corporation</p>

Chase is a model finder for first order logic with equality.  It finds
minimal models of a theory expressed in finitary geometric form, where
functions in models may be partial.  A formula is in finitary
geometric form if it is a sentence consisting of a single implication,
the antecedent is a conjunction of atomic formulas, and the consequent
is a disjunction.  Each disjunct is a possibly existentially
quantified conjunction of atomic formulas.  A function is partial if
it is defined only on a proper subset of its domain.

## Installing From OPAM

    $ opam install chase

## Installing From Sources

This software uses ocaml, opam, getopt, and dune.  See
<http://ocaml.org> for ocaml installation instructions.  Install getopt
and dune with:

    $ opam install getopt dune

Install the programs with:

    $ dune build @install
    $ dune install

## Usage

The user guide is in [chase.xhtml](https://ramsdell.github.io/chase/index.html).

```
$ chase -h
Usage: chase [OPTIONS] [INPUT]
Options:
  -o FILE  --output=FILE  output to file (default is standard output)
  -t       --terse        use terse output -- print only models
  -j       --just-one     find just one model
  -b INT   --bound=INT    set structure size bound (default 250)
  -l INT   --limit=INT    set step count limit (default 2000)
  -c       --compact      print structures compactly
  -s       --sexpr        print structures using S-expressions
  -m INT   --margin=INT   set output margin
  -q       --quant        read formulas using quantifier syntax
  -e       --explicit     print formulas using quantifier syntax
  -f       --flatten      print flattened formulas
  -p       --proc-time    print processor time in seconds
  -v       --version      print version number
  -h       --help         print this message

```

```
$ chasetree -h
Usage: chasetree [OPTIONS] [INPUT]
Options:
  -o FILE  --output=FILE  output to file (default is standard output)
  -r INT   --ratio=INT    set ratio between window heights (default 20%)
  -v       --version      print version number
  -h       --help         print this message

```

## Example

The syntax used for geometric theories is Geolog, a Prolog-like
syntax.  What follows is an example of a theory for conference
management.

```
$ cat cm.gl
% Conference Management

author(X) & paper(Y) & assigned(X, Y).
author(X) & paper(Y) => read_score(X, Y) | conflict(X, Y).
assigned(X, Y) & author(X) & paper(Y) => read_score(X, Y).
assigned(X, Y) & conflict(X, Y) => false.
$

```

A run of Chase produces the following output.

```
$ chase cm.gl
% chase version 1.3
% bound = 50, limit = 500
% ********
% author(X) & paper(Y) & assigned(X, Y). % (0)
% author(X) & paper(Y) => read_score(X, Y) | conflict(X, Y). % (1)
% assigned(X, Y) & author(X) & paper(Y) => read_score(X, Y). % (2)
% assigned(X, Y) & conflict(X, Y) => false. % (3)
% ********

(0)[]

(1,0){0}[assigned(x, y), author(x), paper(y)]

(2,1){1}![assigned(x, y), author(x), paper(y), read_score(x, y)]

(3,1){1}[assigned(x, y), author(x), conflict(x, y), paper(y)]

(4,3){2}[assigned(x, y), author(x), conflict(x, y), paper(y),
  read_score(x, y)]
```

A run of Chase produces structures assembled into a tree.  The root of
the tree is labeled (0).  A label of the form (n, p) gives the node
number of the tree node and its parent.  The form {r} records the rule
used to produce this structure.  A structure marked with ! is a model.
Thus in this output, there are two paths explored, <0,1,2> and
<0,1,3,4>, and one model found (2).

More examples are in the tst directory.

A graphical view of Chase output is constructed by chasetree.

```
$ chase -o cm.text cm.gl
$ chasetree -o cm.xhtml cm.text
```

## Makefile

The file chase.mk contains make rules for Chase.  A sample makefile
that uses chase.mk follows.

```
include chase.mk

TXTS	:= $(patsubst %.gl,%.txt,$(wildcard *.gl)) \
		$(patsubst %.glx,%.txt,$(wildcard *.glx))

all:	$(TXTS)

clean:
	-rm $(TXTS)
```

## Literate Theories Using Markdown

When the Chase input file name has the extension ".md", the input
syntax is treated as Markdown, and Chase input is extracted from
fenced code blocks.  Examples of literate theories is in the
[markdown](markdown/README.md) directory.

## Emacs Users

Syntax error messages produced by Chase include Emacs style
location information.  Use M-x compile to run Chase and C-x ` to
move to the sequent that caused the error message.  When other error
messages include position information, it points to the position of
period in the formula that caused the problem.

## Development

The software uses ocamlbuild for development and testing.  To run the
tests in the [tst directory](tst/README.md), type:

    $ make; (cd tst; make)

The instructions for debugging the program is in [debug.txt](debug.txt).
