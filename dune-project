(lang dune 2.9)
(name celeste_ocaml)

(generate_opam_files true)

(package
  (name celeste_ocaml)
  (depends
    (lua_parser (>= 1.0.2))
    (batteries (>= 3.6.0))
    (tqdm (>= 0.1))
    (ocamlgraph (>= 2.0.0))
    (ppx_deriving (>= 5.2.1))
    (ppx_inline_test (>= 0.14.1))
    (lua_lib (>= 1.8))))
