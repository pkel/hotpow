(lang dune 1.10)
(name hotpow)
(using fmt 1.1)
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
   (dune (and :build (>= 1.10)))
   (sexplib :build)
   (ezxmlm :build)
   (ppx_sexp_conv :build)
   (ppx_deriving_cmdliner :build)))

(package
  (name hotpow-devel)
  (description "")
  (synopsis "Development Tools")
  (depends
   (ocamlformat (>= 0.12))
   (odig :build)
   (merlin (>= 3))
   (ocp-indent :build)
   (odoc :build)))
