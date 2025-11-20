{
  description = "ForSyDe Development Flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { self, flake-utils, ... }@inputs:
    let
      ghcVersion = "ghc9102";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${ghcVersion}" = prev.haskell.packages.${ghcVersion}.extend (
              hfinal: hprev: {
                forsyde-devtools =
                  let
                    unmodified = hprev.callCabal2nix "forsyde-devtools" ./. { };
                  in
                  prev.haskell.lib.enableSharedExecutables (
                    unmodified.overrideAttrs (old: {
                      buildInputs = (old.buildInputs or [ ]) ++ [
                        prev.makeWrapper
                      ];

                      postInstall = (old.postInstall or "") + ''
                        wrapProgram $out/bin/forsyde-devtools-exe \
                          --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
                          --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"
                      '';
                    })
                  );
              }
            );
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
            # For making mkkdocs site
            pypkgs.mkdocs-material
            pypkgs.mkdocs-mermaid2-plugin
            # General dev tools
            pkgs.gnumake
            pkgs.clang-tools # For clang-format generated C code
          ];
          nativeBuildInputs = [ hspkgs.ghc ];
          shellHook = ''
            # Add compiler and LSP executable paths to PATH. Pre-requisite for completion to work, also just
            # nice to be able to write forsyde-devtools-exe with completion instead of having cabal exec/run
            export PATH=$PATH:$(pwd)/dist-newstyle/build/x86_64-linux/ghc-9.10.2/forsyde-devtools-0.0.0.1/x/forsyde-devtools-exe/build/forsyde-devtools-exe/ 
            export PATH=$PATH:$(pwd)/dist-newstyle/build/x86_64-linux/ghc-9.10.2/forsyde-devtools-0.0.0.1/x/forsyde-lsp-exe/build/forsyde-lsp-exe/
            # Add completion script
            source .completions/forsyde-devtools-exe/$0
            source .completions/forsyde-lsp-exe/$0
            # Setup pre-commit hook
            cp .githooks/* .git/hooks/
            chmod u+x .git/hooks/*
          '';
        };
        packages.default = pkgs.forsyde-devtools;
        apps.forsyde-devtools = {
          type = "app";
          program = "${self.packages.${system}.forsyde-devtools}/bin/forsyde-devtools-exe";
        };
      }
    );
}
