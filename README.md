## Setup

```bash
nix-shell # all further commands inside this shell
opam switch create ./ # only needed if it doesn't exist
opam install . --deps-only --locked
eval (opam env)
dune exec ./frontend_example.exe --profile release
```

Dune generates the `celeste_ocaml.opam` file. More info on that [here](https://lambdafoo.com/posts/2021-10-29-getting-started-with-ocaml.html).

[OPAM for npm/yarn users](http://ocamlverse.net/content/opam_npm.html)

## Standard library

- Level 1 (builtin): Builtin functions in our interpreter that don't exist in normal Lua or on Pico-8 (e.g. `__new_unknown_boolean`)
- Level 2 (builtin): Builtin functions in our interpreter that also exist in normal Lua (e.g. `print`)
- Level 3 (lua): Native Lua functions that only exist on Pico-8, but are required by tests (e.g. `add`). These are used by our interpreter and by real Lua.
- Level 4 (builtin): Pico8 functions that are not required by tests (e.g. `sfx`). Some of these (typically drawing functions) may be empty stubs.
- Level 4 (lua): Same as above, but implemented as Lua functions. These are used by our interpreter, not by real Lua.
