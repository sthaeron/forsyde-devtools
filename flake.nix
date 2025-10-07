{
  description = "ForSyDe Development Flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
            // {
              forsyde-devtools = hfinal.callCabal2nix "forsyde-devtools" ./. { };
            };
        };
        forsyde-devtools = final.haskellPackages.forsyde-devtools;
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
        };
        hspkgs = pkgs.haskellPackages;
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
            hspkgs.hlint
            hspkgs.ormolu
            # For making mkkdocs site
            pypkgs.mkdocs-material
            pypkgs.mkdocs-mermaid2-plugin
            # General dev tools
            pkgs.gnumake
          ];
          shellHook = ''
            cp .githooks/* .git/hooks/
            chmod +x .git/hooks/*
          '';
        };
        defaultPackage = pkgs.forsyde-devtools;
      }
    );
}
