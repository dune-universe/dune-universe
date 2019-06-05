FROM ocaml/opam2:alpine
WORKDIR /home/opam/project
RUN opam update
RUN sudo apk add m4 linux-headers gmp-dev perl
RUN opam depext ssl
RUN opam install ssl alcotest
COPY nocoiner.opam ./
RUN opam install --deps-only .
COPY ./ ./
RUN sudo chmod a+rw -R ./
RUN eval $(opam env) && make test
RUN eval $(opam env) && make binary

FROM alpine
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL \
  org.label-schema.build-date=$BUILD_DATE \
  org.label-schema.name="nocoiner" \
  org.label-schema.description="A Commitment Scheme library for Coin Flipping/Tossing algorithms and sort." \
  org.label-schema.url="https://nocoiner.marcoonroad.dev/" \
  org.label-schema.vcs-ref=$VCS_REF \
  org.label-schema.vcs-url="https://github.com/marcoonroad/nocoiner" \
  org.label-schema.vendor="Marco Aurélio da Silva (marcoonroad)" \
  org.label-schema.version=$VERSION \
  org.label-schema.schema-version="1.0"
COPY --from=0 /home/opam/project/nocoiner.exe /usr/bin/nocoiner
RUN chmod a+rx /usr/bin/nocoiner
ENTRYPOINT ["/usr/bin/nocoiner"]
