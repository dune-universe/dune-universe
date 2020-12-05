# Version 1.5.1
## Remove dependency to the ocf package
## Arguments of acg and acgg are now handled by the Cmdliner library
* For acgc, arguments are now:
1. `--version` (instead of `-version`) to get the version number of the software
2. `-o` and `--output` (instead of only `-o`) to output the result in a specific file
3. `-I` and `--include` (instead of only `-I`) to add directories where to look for files
4. `-i` and `--interactive` (instead of only `-i`) to enter the interaction loop
5. `--help` (instead of `-help` and `--help`) to get help
* For acg, arguments are now:
1. `--version` (instead of `-version`) to get the version number of the software
2. `-I` and `--include` (instead of only `-I`) to add directories where to look for files
3. `--nc` and `--no-color` (instead of `-nc`) to toggle off output coloring
4. `--npp` and `--no-pretty-printing` (instead of `-npp`) to toggle off output pretty printing
5. `--nsvg` and `--no-svg` (instead of `-nsvg`) to toggle off svg output of the `realize` acg command
6. `--realize` (instead of `-realize`) to set the configuration file with the rendering engines
7. `--svg` (instead of `-svg`) to set the file name of the svg output of the `realize` acg command
8. `--help` (instead of `-help` and `--help`) to get help

# Version 1.5.0
## The acgc.opt/acgc compiler and the acg.opt/acg interpreter:
* Removed the dependency to BOLT (replaced by Logs) and dypgen (replaced by menhir)
* Some syntax changes:
  1. Prefix operators have the highest priority
  2. Application has precedence over infix operators
  3. An associativity property (none, left, right) can be set to infix operators (left is the default), and a precedence level as well. See the associativity and the precedence section in the [README](README.me).
  4. Infix operator can be used as usual constant by surrounding then with left and right parenthesis, so that `t SYM u` = `(SYM) t u`

# Version 1.4.0
## The acgc.opt/acgc compiler and the acg.opt/acg interpreter:
* Improved performance by better logging management (for debug)
* Extended set of latin1 characters allowed in .acg files

# Version 1.3.3
## The acgc.opt/acgc compiler and the acg.opt/acg interpreter:
* the build process now relies on `jbuilder`


# Version 1.3.1
## The acg.opt/acg interpreter
* Bug fix: constants that were given two interpretations in a lexicon (only the last one is available) generated two rules in the corresponding datalog program (hence possibly several parses)
* Bug fix: when an object constant was the direct interpretation of an abstract constant, parsing this object constant raised an exception
* an option `-svg filename` is added to `./acg` and `./acg.opt` to allow the user to provide another name than the default `realize.svg` to the svg output file.
## Configuration and opam file
* an option `--disable-warning-as-errors` to the `.configure` script in order to make opam compile without the `-warn-error A` option.

# Version 1.3.0
## The acg.opt/acg interpreter
* Colors and link between signature and rendering engines are now defined in a json configuration file. The option to load the configuration is `-realize`.

# Version 1.2
## The acg.opt/acg interpreter
* Added the generation of a `realize.svg` file when the `realize` command of `acg` is invoked. Colors and link between signature and rendering engines are hard-coded.
* An option is added to toggle of the `realize.svg` file generation (takes a bit of time)

# Version 1.1
## The acgc.opt/acgc compiler and the acg.opt/acg interpreter:
* Fixed severe bug that prevented finding some parses and sometimes caused a `Fatal Error`
* Added a control on the compiler version that generated the object (`.acgo`) files. The version has to be the same as the current version of the compiler or of the interpreter that is being used.

## The acgc.opt/acgc compiler:
* added a `nl_lexicon` keyword that causes the interpretation of any functional type (`->` or `=>`) of the abstract signature to be interpreted by the intuitionistic arrow `=>` in the object signature.

	Accordingly, the interpretation should use `Lambda`.

## The acg.opt/acg interpreter:
* Improved terminal output (colors and formatting)
* added a `-nc` option to disable colored outputs
* added a `-npp` option to disable formatting on the output

## The acg emacs mode
* improved handling of long files

# Version 1.0b
## The acgc.opt/acgc compiler:
* Now outputs an `file.acgo` file when compilation is successful
* Can declare a lexicon as the composition of two other lexicons using the following declaration:

	```
	lexicon lex_name = lex_name2 << lex_name1
	```

## The acg.opt/acg interpreter:
* Can load an acg object file using the command:
  ```
  load o file.acco;
  ```

* The `analyse` command is deprecated. It is replaced by:
  * A `check` command, prefixed by signatures, that typecheck the typing assignment of a term

	```
	Sig1 Sig2 check term:type;
	```
  * A `realize` command that must be preceded by lexicons and followed by the type assignment of a term. It checks that the term is well typed in the abstract signatures of the lexicons and compute its realizations through the lexicons.

	```
	Lex1 Lex2 realize term:type;
	```
  * A `parse` command has been added. It must be preceded with the name of a lexicon and it returns the antecedent by this lexicon of the input term with respect to some distinguished (atomic) type:

	```
	Lex parse object_term:distinguished_type;
	```

* A `query` command has been added. It follows the `parse` command syntax and it outputs the associated query and associated extensional database.
```
Lex query object_term:distinguished_type;
```

* A `idb` command has been added. It must be preceded with the name of a lexicon and it returns the intensional database associated with the lexicon
```
Lex idb;
```
  

## Current limitation:
* Only parse images of atomic types
* The result of parsing with non-linear lexicons is unspecified yet
