{
  description = "ForSyDe Development Flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { flake-utils, ... }@inputs:
    let
      ghcVersion = "ghc9102";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
            // {
              forsyde-devtools = hfinal.callCabal2nix "forsyde-devtools" ./. { };
            };
        };
        forsyde-devtools = final.haskell.packages.${ghcVersion}.forsyde-devtools;
      };
    in
    {
      inherit overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ overlay ];
          config = {
            haskell.compiler = pkgs.haskell.compilers.${ghcVersion};
          };
        };
        hspkgs = pkgs.haskell.packages.${ghcVersion};
        pypkgs = pkgs.python313Packages;
      in
      {
        devShells.default = hspkgs.shellFor {
          name = "forsyde-devtools";
          withHoogle = true;
          packages = (p: [ p.forsyde-devtools ]);
          buildInputs = [
            # Haskell specific dev tools
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            hspkgs.ormolu
            hspkgs.hmatrix
            # For making mkkdocs site
            pypkgs.mkdocs-material
            pypkgs.mkdocs-mermaid2-plugin
            # General dev tools
            pkgs.gnumake
            pkgs.clang-tools # For clang-format generated C code
          ];
          nativeBuildInputs = [ hspkgs.ghc ];
          shellHook = ''
            cp .githooks/* .git/hooks/
            chmod +x .git/hooks/*
          '';
        };
        packages.default = pkgs.forsyde-devtools;
      }
    );
}
