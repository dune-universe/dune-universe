FROM ocaml/opam2:4.09
COPY --chown=opam id_rsa /home/opam/.ssh/
RUN echo gitlab.inria.fr,128.93.193.8 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAFldY2ft1ReZjFYPpe/wa5Vhl4YJfYz6IJOZaZxL924R44jrGWiR2/Misrug75NBsQB+UWs1iIyWZPk0AN45Sk= >/home/opam/.ssh/known_hosts
RUN git pull
RUN opam update
RUN opam pin add -n git+https://gitlab.inria.fr/memcad/clangml.git#snapshot
RUN opam depext -i clangml
RUN cd /home/opam && git clone git@gitlab.inria.fr:memcad/clangml-transforms.git
RUN . /home/opam/.profile && cd /home/opam/clangml-transforms && \
    dune build clangml-transforms.opam
RUN opam pin add -n file:///home/opam/clangml-transforms
RUN cd /home/opam && git clone git@gitlab.inria.fr:tmartine/memcad.git
RUN opam pin add -n /home/opam/memcad
RUN opam depext -i memcad

