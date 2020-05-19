docker=podman
# In theory, this should work as well:
# docker=sudo docker

opam_container=ocaml/opam2:alpine-3.10-ocaml-4.09

sim: export OCAMLRUNPARAM=b
sim: build
	dune exec bprot_sim

static: export OCAMLRUNPARAM=b
static: docker_build_static
	_build/static/bin/bprot_sim

build:
	dune build

watch:
	dune build -w

format:
	dune build @fmt --auto-promote

deps: bprot.opam bprot-devel.opam
	opam install . --deps-only

clean:
	rm -r _build

bprot.opam: dune-project
	dune build bprot.opam

bprot-devel.opam: dune-project
	dune build bprot-devel.opam

docker_build_static: bprot.opam *.ml dune dune-workspace.static
	$(docker) pull $(opam_container)
	$(docker) tag $(opam_container) bprot/opam
	$(docker) build -t bprotsim  .
	mkdir -p _build/static/bin
	tar -cf - $^ | $(docker) run -i bprotsim bash -c "tar -xf - \
	&& opam exec -- dune build --workspace dune-workspace.static \
	&& tar -chf - -C _build/install/default/bin bprot_sim" |\
		tar -xf - -C _build/static/bin
