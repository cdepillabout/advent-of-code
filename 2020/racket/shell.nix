
# Currently  using nixpkgs-20.09 as of 2020-12-12.
let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e79793fd859.tar.gz";
    sha256 = "1a7d1671qdwylxfcgs6hmm1idgsbczvrv2n591qgmba5jrrp07nb";
  };
in

with import nixpkgs-src {};

mkShell {
  nativeBuildInputs = [
    racket

    # For visualizing graphs.
    python3Packages.xdot
    # xdot needs gtk icons.
    gnome3.adwaita-icon-theme
  ];
}
