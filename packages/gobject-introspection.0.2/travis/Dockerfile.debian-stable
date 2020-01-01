FROM ocaml/opam2:debian-stable

RUN \
  sudo apt update && \
  sudo apt install -y \
    libgtk-3-dev \
    libglib2.0-0 \
    xvfb \
    git \
    darcs \
    mercurial \
    wget \
    gcc \
    g++ \
    make \
    libssl-dev \
    libreadline-dev \
    gstreamer1.0-plugins-good \
    gnumeric \
    gnome-icon-theme \
    dbus-x11 \
    software-properties-common \
    aspcud \
    m4 \
    unzip \
    pkg-config \
    libgirepository1.0-dev \
    libffi6 \
    libffi-dev

COPY . /home/opam/tests
RUN sudo chown -R opam:opam /home/opam/tests
WORKDIR /home/opam/tests
RUN opam switch $OCAML_VERSION
RUN eval $(opam env)
RUN ./travis/initialize_ocaml_environment.sh
RUN ./travis/show_versions.sh
CMD bash -ex ./travis/runtest.sh
