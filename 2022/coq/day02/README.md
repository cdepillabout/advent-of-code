
# Advent of Code 2022 Problems in Coq

Get in the dev env:

```console
$ nix develop
```

Create the Makefile:

```console
$ coq_makefile -f _CoqProject Day*.v -o Makefile
```

Build all modules.  This also generates the extracted Haskell files (like
`Day01Generated.hs`):

```console
$ make
```

You can also open up any `.v` file in `coqide` in order to run it and generate
the extracted Haskell file, or just edit the `.v` file:

```console
$ coqide Day01.v
```

When you have the generated `.hs` file (like `Day01Generated.hs`), you can run
the associated script to make the real `.hs` output file:

```console
$ ./day01-make-real-hs.sh
```

This creates a `Day01Real.hs` file.  This file can be run:

```console
$ runghc ./Day01Real.hs
```
