
# Currently  using nixpkgs-20.09 as of 2020-12-12.
with import <nixpkgs> {};

mkShell {
  nativeBuildInputs = [
    racket

    # For visualizing graphs.
    python3Packages.xdot
    # xdot needs gtk icons.
    gnome3.adwaita-icon-theme
  ];
}
