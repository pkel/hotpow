FROM ocaml/opam2:alpine

USER root
RUN apk add m4
RUN mkdir /src && chown opam /src

USER opam
WORKDIR /src
COPY hotpow.opam hotpow.opam
RUN opam install . -y --deps-only
