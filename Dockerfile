FROM bprot/opam

USER root
RUN apk add m4
RUN mkdir /src && chown opam /src

USER opam
WORKDIR /src
COPY bprot.opam bprot.opam
RUN opam install . -y --deps-only
