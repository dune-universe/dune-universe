FROM ocamlcodoc
COPY --chown=ci Makefile ocamlcodoc.opam dune-project /home/ci/ocamlcodoc/
COPY --chown=ci utils /home/ci/ocamlcodoc/utils/
COPY --chown=ci ocamlcodoc /home/ci/ocamlcodoc/ocamlcodoc/
COPY --chown=ci backend /home/ci/ocamlcodoc/backend/
RUN opam update
RUN opam pin add -y ocamlcodoc /home/ci/ocamlcodoc
