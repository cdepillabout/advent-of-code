# Advent of Code 2018 Haskell Starter Kit

This is a starter kit for attempting the Advent of Code 2018 problems
in Haskell.

This repo gives you a few niceties:

1. an easy way to build, run, and test solutions
1. an opinionated file layout that makes it easy to attempt multiple problems
1. a couple popular libraries pre-installed
1. often-used language pragmas listed in the cabal file

## How to use

You can download and setup this repo as follows.  You will need
[`stack`](https://docs.haskellstack.org/en/stable/README/) installed:

```sh
$ git clone git@github.com:cdepillabout/advent-of-code2018.git
$ stack test
```

This should download GHC, as well as compile all necessary libraries.
It should also compile this library, as well as all tests.  This will probably
take 10 to 20 minutes the first time you do it.

The solutions for the individual days should be stored in the `app/` directory.
There is an `app/Day01.hs` file that you can base your solutions on.  There is some
documentation in that file that you should read.

You can build and run your solution like the following:

```sh
$ stack build --fast advent-of-code2018:day01
$ stack exec -- day01
```

The `--fast` flag specifies that you don't want GHC to do that much
optimization.  This makes compiling faster.  This is generally okay to use,
unless you need your code to be really fast.  The `advent-of-code2018:day01`
parameter says that you want to only build the executable called `day01` from
the `advent-of-code2018` package.

When you want to start working on the solution for a new day, you should copy
the `app/Day01.hs` file to a new file called `app/Day02.hs`.  You also need to
add a new `executable` section to the `advent-of-code2018.cabal` file.  You can
copy and paste the existing `executable day01` section.  Just be sure to rename
things as suggested in the documentation.

There is also a shared library in `src/` that can be accessed from any
executable.  If you put code in any module under `src/` you can use it in
multiple executables.  Although it will probably just be easier to copy and
paste code between executables.

## Other Stuff

There are a couple other nice things this repo sets up for you.

### Doctests

Doctests have been setup to run automatically if you run `stack test` as follows:

```sh
$ stack test --fast
```

You can see an example of using doctests if you look at any of the Haskell
files.  This lets you easily write tests inline.  This can be quite useful for
these types of programming competitions.

If you want to run the doctests for a new executable (for instance `Day02.hs`),
you have to add a parameter to the `doctests` section in the
`advent-of-code2018.cabal` file.  There is a note in the `test-suite doctests`
section that explains how to do this.  If you forget to do this, then the
doctests will not be run.

### Language Pragmas and Libraries

There is a big list of language pragmas and libraries that have already been
added to the cabal file.  These are widely-used language pragmas and libraries.
This makes it a little easier to just get to work on your solution without
having to manually add pragmas and libraries.

### Continuous Building

Some people like to have `stack` rebuild their code every time they save in
their editor.  This is easy to do with the `--file-watch` argument to stack:

```sh
$ stack build --fast --file-watch advent-of-code2018:day01
```

It is also possible with `test`:

```sh
$ stack test --fast --file-watch
```
