# OCaml binding to openai-gym

openai-gym is an OCaml binding for [openai-gym](https://github.com/openai/gym)
open-source library. It is built as a client for the [gym-http-api](https://github.com/openai/gym-http-api) REST API.

To use the openai-gym package, you need to have a [gym-http-api](https://github.com/openai/gym-http-api) server runnung:
```
python3 gym_http_server.py
```

The documentation of the openai-gym package is available [online](https://ibm.github.io/openai-gym-ocaml/) or in the directory [docs](./docs).

# Install

## Quick install with Opam

You can install openai-gym with the following command:
```
opam install openai-gym
```


## Install from source with Opam

Opam can also be used to compile and install from the source
directory. For that you first need to pin the source directory.
So, from this directory, do:
```
opam pin add openai-gym .
```

Then you can install using the command:
```
opam install openai-gym
```

If the source files are modified, the packages must be reinstalled
with the command:
```
opam reinstall openai-gym
```


## Building from source
### Prerequistes

To build from source, you will need to install the dependencies
listed in the `depends` field of the `*.opam` files.

An easy way to get set up on most platforms is to use the OCaml
package manager (https://opam.ocaml.org). Once opam is installed, you
can just add the corresponding libraries:
```
opam install ocamlfind dune atdgen atd cohttp-lwt-unix ...
```

### Compiling

To compile, do:

```
make
```

To test, do:
```
make test
```

To generate the documentation, do:
```
make doc
```

# Make a new release

In order to do a new release, we have to do the following steps.

1. Search and update the version number:
```
grep -r -e '\d.\d\d-dev' .
```

2. Make sure that the documentation is up to date:
```
make webdoc
```

3. Update the `CHANGES.md` file.

4. Commit the changes.

5. Create a new release on the github interface:
   https://github.com/IBM/openai-gym-ocaml/releases

6. Create a new release of the opam packages.
  - Create or update the fork of https://github.com/ocaml/opam-repository
```
git checkout master
git fetch --all
git merge --ff-only upstream/master
git push
```
  - Create a new branch
```
git checkout -b openai-gym-X.XX
```
  - Create the new packages from the old ones:
```
cp -R packages/openai-gym/openai-gym.Y.YY packages/openai-gym/openai-gym.X.XX
```
  - Update the `opam` files:
```
cp OPENAI-GYM_OCAML_DIR/openai-gym.opam packages/openai-gym/openai-gym.X.XX/opam
```
  - Update the `url` files
```
emacs packages/openai-gym/openai-gym.X.XX/url
```
  - Commit and push the changes
```
git push origin openai-gym-X.XX
```
  - Create a pull request from the github interface:
	https://github.com/ocaml/opam-repository

7. Once the pull request is accepted update the version number.


# Contribute

Contributions and bug reports are welcome!
To contribute please follows the instructions given in the file [CONTRIBUTING.md](./CONTRIBUTING.md).

