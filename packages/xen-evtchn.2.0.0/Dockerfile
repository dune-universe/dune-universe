FROM unikernel/mirage
COPY xen-evtchn-unix.opam /src/xen-evtchn-unix.opam
RUN opam install depext -y
RUN opam pin add xen-evtchn-unix /src -n
RUN opam depext xen-evtchn-unix -y
RUN opam install xen-evtchn-unix --deps-only

