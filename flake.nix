{
  description = "ForSyDe Development Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    forsyde-atom = {
      url = "github:forsyde/forsyde-atom/a24e65741832fe807cd530e160934d5156c76460";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      forsyde-atom,
      ...
    }@inputs:
    let
      ghcVersion = "ghc9102";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${ghcVersion}" = prev.haskell.packages.${ghcVersion}.extend (
              hfinal: hprev: {
                forsyde-atom =
                  let
                    pkg = hfinal.callCabal2nix "forsyde-atom" forsyde-atom { };
                  in
                  final.haskell.lib.compose.overrideCabal (old: {
                    # cabal fails due to haddock include wildcard fails, using a
                    # dummy file as a workaround
                    postPatch = ''
                      mkdir -p fig
                      touch fig/dummy.png
                    '';
                  }) pkg;
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
                        wrapProgram $out/bin/forsyde-compiler-exe \
                          --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
                          --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"
                        wrapProgram $out/bin/forsyde-lsp-exe \
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
            # General dev tools
            pkgs.clang-tools # For clang-format generated C code
            pkgs.clang
            pkgs.gcc
            pkgs.gnumake
            # Haskell specific dev tools
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            hspkgs.ormolu
            hspkgs.cabal-gild
            # For making mkkdocs site
            pypkgs.mkdocs-material
            pypkgs.mkdocs-mermaid2-plugin
          ];
          nativeBuildInputs = [ hspkgs.ghc ];
          shellHook = ''
            cp .githooks/* .git/hooks/
            chmod +x .git/hooks/*
          '';
        };
        packages.default = pkgs.forsyde-devtools;
        apps = {
          forsyde-compiler = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/forsyde-compiler-exe";
          };
          forsyde-lsp = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/forsyde-lsp-exe";
          };
          default = self.apps.${system}.forsyde-compiler;
        };
      }
    );
}
