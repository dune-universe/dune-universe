FROM ocaml/opam2:centos

 RUN \
   sudo yum install -y epel-release
RUN \
  sudo yum install -y \
    gtk+-devel \
    glib-devel \
    gobject-introspection-devel \
    xorg-x11-server-Xvfb \
    which \
    git \
    darcs \
    mercurial \
    bzip2 \
    gcc \
    gcc-c++ \
    make \
    aspcud \
    m4 \
    unzip \
    openssl-devel \
    readline-devel \
    zlib-devel \
    dejavu-sans-fonts \
    gnome-icon-theme \
    adwaita-gtk2-theme \
    vte3 \
    webkitgtk \
    webkitgtk3 \
    libwebkit2-gtk \
    clutter-gtk \
    gtksourceview3 \
    dbus-x11 \
    wget \
    patch

COPY . /home/opam/tests
RUN sudo chown -R opam:opam /home/opam/tests
WORKDIR /home/opam/tests
RUN opam switch $OCAML_VERSION
RUN eval $(opam env)
RUN ./travis/initialize_ocaml_environment.sh
RUN ./travis/show_versions.sh
CMD bash -ex ./travis/runtest.sh
