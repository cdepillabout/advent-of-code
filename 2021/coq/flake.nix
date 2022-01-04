{
  description = "advent of code 2021 in coq";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          coq-shell = self.stdenv.mkDerivation {
            name = "coq-shell";
            dontUnpack = true;
            nativeBuildInputs =
              let
                coqPkgs = self.coqPackages_8_13;
              in
              [ coqPkgs.coq
                coqPkgs.parsec
                (self.haskellPackages.ghcWithPackages (p: []))
              ];
            installPhase = "touch $out";
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
      {
        devShell = pkgs.coq-shell;
      }
    );
}

