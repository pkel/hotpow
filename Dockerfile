FROM ocaml/opam2:alpine

USER root
RUN apk add m4
RUN mkdir /src && chown opam /src

USER opam
RUN opam update && opam install -y dune cmdliner ppx_deriving_cmdliner

WORKDIR /src
