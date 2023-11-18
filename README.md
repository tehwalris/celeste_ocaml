```bash
nix-shell # all further commands inside this shell
opam switch create ./ # only needed if it doesn't exist
opam install . --deps-only --locked
eval (opam env)
dune exec ./frontend_example.exe --profile release
```

Dune generates the `celeste_ocaml.opam` file. More info on that [here](https://lambdafoo.com/posts/2021-10-29-getting-started-with-ocaml.html).

[OPAM for npm/yarn users](http://ocamlverse.net/content/opam_npm.html)
