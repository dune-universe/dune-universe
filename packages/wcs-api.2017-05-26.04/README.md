# SDK and command line interface for Watson Conversation Service

wcs-ocaml is a source development kit in OCaml and command line interface for
[Watson Conversation Service (WCS)](https://www.ibm.com/watson/services/conversation/). It allows to program chat bots in OCaml.

* `wcs-lib` provides a framework to write WCS programs, called
  workspaces.
* `wcs-api` offers an OCaml binding to the
  [service API](https://www.ibm.com/watson/developercloud/conversation/api/v1/)
  and a generic client application.
* `wcs` is a command line tool that interact with the service.

The documentation is available [online](https://ibm.github.io/wcs-ocaml/) or in
the directory [docs](./docs).

# Install

## Quick install with Opam

You can install wcs-ocaml with the following command:
```
opam install wcs
```

This will install the three main packages:
- wcs-lib,
- wcs-api and,
- wcs


## Install from source with Opam

Opam can also be used to compile and install from the source
directory. For that you first need to pin the source directory.
So, from this directory, do:
```
opam pin add wcs-lib .
opam pin add wcs-api .
opam pin add wcs .
```

Then you can install using the command:
```
opam install wcs
```

If the source files are modified, the packages must be reinstalled
with the command:
```
opam reinstall wcs-lib
```


## Building from source
### Prerequistes

To build from source, you will need to install the dependencies
listed in the `depends` field of the `*.opam` files.

An easy way to get set up on most platforms is to use the OCaml
package manager (https://opam.ocaml.org). Once opam is installed, you
can just add the corresponding libraries:
```
opam install ocamlfind sedlex menhir rml ...
```

### Compiling

To compile, do:

```
make
```

# Make a new release

In order to do a new release, we have to do the following steps.

1. Make sure that the documentation is up to date:
```
make webdoc
```

2. Search and update the version number:
```
grep -r -e '\d\d\d\d-\d\d-\d\d.\d\d-dev' .
```

3. Update the `CHANGES.md` file.

4. Create a new release on the github interface:
   https://github.com/IBM/wcs-ocaml/releases

5. Create a new release of the opam packages.
  - Create or update the fork of https://github.com/ocaml/opam-repository
```
git checkout master
git fetch --all
git merge --ff-only upstream/master
git push
```
  - Create a new branch
```
git checkout -b wcs-XXXX-XX-XX.XX
```
  - Create the new packages from the old ones:
```
cp -R packages/wcs-lib/wcs-lib.YYYY-YY-YY.YY packages/wcs-lib/wcs-lib.XXXX-XX-XX.XX
cp -R packages/wcs-api/wcs-api.YYYY-YY-YY.YY packages/wcs-api/wcs-api.XXXX-XX-XX.XX
cp -R packages/wcs/wcs.YYYY-YY-YY.YY packages/wcs/wcs.XXXX-XX-XX.XX
```
  - Update the `opam` files:
```
cp WCS_OCAML_DIR/wcs-lib.opam packages/wcs-lib/wcs-lib.XXXX-XX-XX.XX/opam
cp WCS_OCAML_DIR/wcs-api.opam packages/wcs-api/wcs-api.XXXX-XX-XX.XX/opam
cp WCS_OCAML_DIR/wcs.opam packages/wcs/wcs.XXXX-XX-XX.XX/opam
```
  - Update the `url` files
```
emacs packages/wcs-lib/wcs-lib.XXXX-XX-XX.XX/url
cp packages/wcs-lib/wcs-lib.XXXX-XX-XX.XX/url packages/wcs-api/wcs-api.XXXX-XX-XX.XX/url
cp packages/wcs-lib/wcs-lib.XXXX-XX-XX.XX/url packages/wcs/wcs.XXXX-XX-XX.XX/url
```
  - Commit and push the changes
```
git push origin wcs-XXXX-XX-XX.XX
```
  - Create a pull request from the github interface:
	https://github.com/ocaml/opam-repository

6. Once the pull request is accepted update the version number.

# Contribute

Contributions and bug reports are welcome!
To contribute please follows the instructions given in the file (CONTRIBUTING.md)[./CONTRIBUTING.md].

