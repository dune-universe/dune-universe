FROM gentoo/stage3-amd64
RUN emerge --sync
RUN emerge --oneshot portage
RUN emerge sudo dev-vcs/git
#RUN emerge opam --autounmask-write || ( \
#      etc-update --automode -3 && \
#      emerge opam )
RUN echo ACCEPT_KEYWORDS="~amd64" >> /etc/portage/make.conf && emerge opam
RUN useradd -m -s /bin/bash ci
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
RUN emerge wget
USER ci
WORKDIR /home/ci
RUN wget -O ~/opam \
  https://github.com/ocaml/opam/releases/download/2.0.2/opam-2.0.2-x86_64-linux
RUN chmod +x ~/opam
RUN sudo mv ~/opam /usr/local/bin/opam
RUN opam init --disable-sandboxing --auto-setup --dot-profile=/home/ci/.bash_env
RUN opam init --auto-setup --dot-profile=/home/ci/.bash_env
SHELL ["/bin/bash", "-c"]
ENV BASH_ENV /home/ci/.bash_env
RUN opam install -y depext
RUN sudo emerge sys-devel/clang
#RUN opam depext -y clangml
RUN opam update
RUN opam install -y refl
#RUN opam install -y clangml
