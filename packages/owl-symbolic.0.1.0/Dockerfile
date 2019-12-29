############################################################
# Dockerfile to build Owl-Symbolic image
# Based on owlbarn/owl_symbolic master branch
# By Liang Wang <liang.wang@cl.cam.ac.uk>
############################################################

FROM owlbarn/owl:latest
USER root

ENV OWLSYMPATH /home/opam/owl-symbolic
COPY . ${OWLSYMPATH}
RUN cd ${OWLSYMPATH} && opam pin .
RUN echo "#require \"owl-symbolic\";;" >> /home/opam/.ocamlinit

WORKDIR ${OWLSYMPATH}
ENTRYPOINT [ "/bin/bash" ]