# ForSyDe DevTools
Work in progress compiler and visualiser development tools for [ForSyDe Shallow](https://forsyde.github.io/forsyde-shallow/), a modelling framework written in Haskell.

## Building and Running
Currently, the project is only supported through a Nix flake. You can install Nix onto your system with the following instructions at [nixos.org](https://nixos.org/download/). With Nix installed, you can clone the repo and enter the development environment using the command `nix develop`. You can build the project with either `cabal build` or `nix build`. Use the following commands to try out the compiler:
```shell
# If using cabal:
cabal run forsyde-devtools-exe -- examples/model/SDF_example_003.hs --output-forsyde-ir --stdout
# If using nix:
./result/bin/forsyde-devtools-exe examples/model/SDF_example_003.hs --output-forsyde-ir --stdout
```

## Documentation
Detailed documentation relating to the different components of this project can be found in the [docs](docs) directory. When in the development environment, the documentation can be built and served as a webpage using the commands:
```shell
mkdocs build
mkdocs serve
```

## Project Contributors
- Michel Delli Abo
- Samuel Miksits
- Klara Modin
- Mohammad Afif Ramadhan
- Sebastian Thaeron
- Zicong Zhang
