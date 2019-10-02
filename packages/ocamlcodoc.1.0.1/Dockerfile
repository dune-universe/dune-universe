FROM debian:testing
RUN apt-get update && apt-get install --yes autoconf automake unzip aspcud \
  rsync git mercurial darcs wget build-essential sudo vim curl
RUN adduser --disabled-password --gecos ci --shell /bin/bash ci
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
RUN wget --output-document=$HOME/opam \
  https://github.com/ocaml/opam/releases/download/2.0.2/opam-2.0.2-x86_64-linux
RUN chmod +x ~/opam
RUN sudo mv ~/opam /usr/local/bin/opam
RUN opam init --disable-sandboxing --auto-setup --dot-profile=/home/ci/.bash_env
SHELL ["/bin/bash", "-c"]
ENV BASH_ENV /home/ci/.bash_env
RUN opam install dune