(lang dune 1.10)
(name hotpow)
(using fmt 1.1)
(generate_opam_files true)

(license none)
(maintainers "anonymous, <no@mail.provided>")
(authors "anonymous, <no@mail.provided>")
(homepage "no.site")
(source (github n/a))

(package
  (name hotpow)
  (description "")
  (synopsis "HotPoW. Consensus Protocol & Simulation")
  (depends
   (dune (and :build (>= 1.10)))
   (base (< v0.13))
   (sexplib :build)
   (ezxmlm :build)
   (ppx_sexp_conv :build)
   (ppx_deriving_cmdliner :build)))

(package
  (name hotpow-devel)
  (description "")
  (synopsis "Development Tools")
  (depends
   (ocamlformat (and (< 0.13) (>= 0.12)))
   (odig :build)
   (merlin (>= 3))
   (ocp-indent :build)
   (odoc :build)))
