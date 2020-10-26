docker=podman
# In theory, this should work as well:
# docker=sudo docker

opam_container=ocaml/opam2:alpine-3.10-ocaml-4.09

single: export OCAMLRUNPARAM=b
single: build
	mkdir -p output
	dune exec powsim -- --header ;\
	dune exec powsim -- -o output/blocks.csv

many: export OCAMLRUNPARAM=b
many: build
	mkdir -p output
	dune exec many

static: export OCAMLRUNPARAM=b
static: docker_build_static
	_build/static/bin/hotpow_sim

build:
	dune build

watch:
	dune build -w

format:
	dune build @fmt --auto-promote

deps: hotpow.opam hotpow-devel.opam
	opam install . --deps-only --working-dir

clean:
	rm -r _build

hotpow.opam: dune-project
	dune build hotpow.opam

hotpow-devel.opam: dune-project
	dune build hotpow-devel.opam

docker_build_static: hotpow.opam *.ml dune dune-workspace.static
	$(docker) pull $(opam_container)
	$(docker) tag $(opam_container) hotpow/opam
	$(docker) build -t hotpowsim  .
	mkdir -p _build/static/bin
	tar -cf - $^ | $(docker) run -i hotpowsim bash -c "tar -xf - \
	&& opam exec -- dune build --workspace dune-workspace.static \
	&& tar -chf - -C _build/install/default/bin hotpow_sim" |\
		tar -xf - -C _build/static/bin
