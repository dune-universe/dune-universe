from ocaml/opam2:alpine
user root
run apk add m4
user opam
run git pull
run opam update
workdir /home/opam
copy --chown=opam . src
workdir src
run opam install . -y -t -d
