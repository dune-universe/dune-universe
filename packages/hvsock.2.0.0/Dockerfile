FROM alpine AS build
RUN apk update && apk add opam alpine-sdk bash
RUN opam init --comp=4.10.0 --switch=4.10.0 --disable-sandboxing -y
RUN opam install depext -y

COPY . /src
COPY hvsock.opam /src/hvsock.opam
RUN opam pin add hvsock /src/ -n
RUN opam depext hvsock -y
RUN opam install hvsock -y
# /root/.opam/4.10.0/bin/hvcat
RUN cp $(opam config exec -- which hvcat) /hvcat

FROM alpine
COPY --from=build /hvcat /hvcat
ENTRYPOINT [ "/hvcat" ]
