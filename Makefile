docker=podman
# In theory, this should work as well:
# docker=sudo docker

sim: export OCAMLRUNPARAM=b
sim: build
	_build/default/topogen.exe | _build/default/sim.exe | xmllint --format -

_build/static/sim.exe: *.ml *.patch patch.sh dune dune-workspace.static dune-project
	$(docker) pull ocaml/opam2:alpine
	$(docker) build -t hotpowsim  .
	mkdir -p _build/static
	tar -cf - $^ | $(docker) run -i hotpowsim bash -c "tar -xf - \
	&& opam exec -- dune build --workspace dune-workspace.static sim.exe \
	&& cat _build/default/sim.exe" > $@
	chmod +x $@

clean:
	rm -r _build

static: _build/static/sim.exe
	OCAMLRUNPARAM=b _build/static/sim.exe

build:
	dune build @all

watch:
	fd 'ml|dune|sh' | entr -s 'make build'

format:
	dune build @fmt --auto-promote

upload:
	git archive -o _build/hotpow.zip HEAD .
	curl -F "file=@_build/hotpow.zip" https://anonfiles.com/api/upload |\
		jq -r '.data.file.url.short' |\
		awk '{ print "\\url{" $$1 "}%"}' |\
		tee ../code-url.tex
