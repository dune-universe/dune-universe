## Get an OPAM image. By default, OCaml 4.06, but the tag can be
## changed by the `docker build` command line.

ARG tag=4.06
ARG image=ocaml/opam2:$tag
FROM $image
MAINTAINER Nicolas Jeannerod

## Install dependencies. `opam depext` installs first the non-opam
## dependencies that are required and then the OPAM packages.

RUN opam depext -i morbig

## Work in /home/opam/morsmall, copy all the file there with the right
## owner and group.

WORKDIR /home/opam/morsmall
ADD . .
RUN sudo chown -R opam .

## Build

RUN eval $(opam env) && make
