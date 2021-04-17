let
  nixpkgs-src = builtins.fetchTarball {
    # nixpkgs 20.09 as of 2021-09-10
    url = "https://github.com/NixOS/nixpkgs/archive/389249fa9b35b3071b4ccf71a3c065e7791934df.tar.gz";
    sha256 = "1z087f1m1k4pii2v2xai8n0yd3m57svgslzzbm7fwdjjzhn8g2rl";
  };

  pkgs = import nixpkgs-src {};

in

pkgs.mkShell {
  buildInputs = with pkgs; [
    dotnet-sdk_3
  ];

  # shellHook = ''
  #   # cat ${ghc}/bin/ghc
  #   # eval $(egrep ^export ${ghc}/bin/ghc)
  # '';
}

