(lang dune 2.0)
; This file was generated by drom, using drom.toml
(name Ocaml2D)
(allow_approximate_merlin)
(generate_opam_files false)
(version 0.1.0)
(formatting (enabled_for ocaml reason))

(package
 (name Ocaml2D)
 (synopsis "The Ocaml2D project")
 (description "\n\nThis is the description\nof the Ocaml2D OCaml project\n")
 (depends
   (ocaml (>= 4.07.0))
   (raylib (and (>= 0.3.2) (< 1.0.0)))
   ppx_inline_test
   ppx_expect
   odoc
   ocamlformat
 ))

