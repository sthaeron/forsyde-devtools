let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-25.05";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    # OCaml compiler
    ocaml
    
    # OPAM package manager
    opam
    
    # Build tools
    ocamlPackages.menhir
    ocamlPackages.dune_3
    ocamlPackages.ocamlformat # needed by pre-commit hook
    ocamlPackages.utop

    # Development tools
    gnumake
  ];

  # Environment variables
  shellHook = ''
    export OCAML_TOPLEVEL_PATH="${pkgs.ocaml}/lib/ocaml"
    
    # Initialize OPAM if needed
    if [ ! -d "$HOME/.opam" ]; then
      echo "Initializing OPAM..."
      opam init --auto-setup --bare
    fi

    # copy githooks
    cp .githooks/* .git/hooks/
    chmod +x .git/hooks/*
    
    echo "OCaml ${pkgs.ocaml.version} environment ready"
    echo "Dune version: $(dune --version)"
    echo "OPAM version: $(opam --version)"
  '';
}
