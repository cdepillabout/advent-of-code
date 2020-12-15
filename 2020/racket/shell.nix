
# Currently  using nixpkgs-20.09 as of 2020-12-12.
with import <nixpkgs> {};

mkShell {
  nativeBuildInputs = [racket python3Packages.xdot];
}
