FROM ocurrent/opam:fedora-30-ocaml-4.11
WORKDIR /home/opam/opam-repository
RUN git pull origin master
RUN opam update
WORKDIR /home/opam
RUN opam install dune
RUN git clone https://github.com/thierry-martinez/metapp.git
RUN cd metapp && opam exec dune build metapp.opam
RUN opam pin add --no-action -k path metapp
RUN git clone https://github.com/thierry-martinez/metaquot.git
RUN cd metaquot && opam exec dune build metaquot.opam
RUN opam pin add --no-action -k path metaquot
RUN git clone https://github.com/thierry-martinez/traverse.git
RUN cd traverse && opam exec dune build traverse.opam
RUN opam pin add --no-action -k path traverse
RUN git clone https://github.com/thierry-martinez/refl.git
RUN cd refl && opam exec dune build refl.opam
RUN opam pin add --no-action -k path refl
RUN git clone https://github.com/thierry-martinez/pattern.git
RUN cd pattern && opam exec dune build pattern.opam
RUN opam pin add --no-action -k path pattern
RUN opam pin add --no-action https://gitlab.inria.fr/memcad/clangml.git#snapshot
RUN opam depext clangml
RUN opam install --yes clangml