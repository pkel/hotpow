(lang dune 2.3)
(name hotpow)
(generate_opam_files true)

(license none)
(maintainers "Patrik Keller, <patrik@keller-re.de>")
(authors "Patrik Keller, <patrik@keller-re.de>")
(homepage "https://arxiv.org/abs/1907.13531")
(source (github pkel/hotpow))

(package
  (name hotpow)
  (description "")
  (synopsis "HotPoW. Consensus Protocol & Simulation")
  (depends
   (dune (>= 2.3))
   (base (< v0.15))
   (parany :build)
   (ppx_hash :build)
   (memtrace :build)
   (ppx_deriving_cmdliner :build)))

(package
  (name hotpow-devel)
  (description "")
  (synopsis "Development Tools")
  (depends
   (ocamlformat :build)
   (odig :build)
   (merlin (>= 3))
   (ocp-indent :build)
   (odoc :build)))
