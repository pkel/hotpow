docker=podman
# In theory, this should work as well:
# docker=sudo docker

sim: export OCAMLRUNPARAM=b
sim: build
	dune exec hotpow_topogen | dune exec hotpow_sim | xmllint --format -

static: export OCAMLRUNPARAM=b
static: docker_build_static
	_build/static/bin/hotpow_topogen | _build/static/bin/hotpow_sim | xmllint --format -

build:
	dune build

watch:
	dune build -w

format:
	dune build @fmt --auto-promote

deps:
	opam install . --deps-only

clean:
	rm -r _build

docker_build_static: lib/* *.ml dune dune-workspace.static
	$(docker) pull ocaml/opam2:alpine
	$(docker) build -t hotpowsim  .
	mkdir -p _build/static/bin
	tar -cf - $^ | $(docker) run -i hotpowsim bash -c "tar -xf - \
	&& opam exec -- dune build --workspace dune-workspace.static \
	&& tar -chf - -C _build/install/default/bin hotpow_sim hotpow_topogen" |\
		tar -xf - -C _build/static/bin
