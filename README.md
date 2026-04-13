# ForSyDe DevTools
The ForSyDe DevTools project provides a compiler and visualiser for [ForSyDe Shallow](https://forsyde.github.io/forsyde-shallow/), a modelling framework written in Haskell. The project was conducted as part of the course [IL2232 Embedded Systems Design Project](https://www.kth.se/student/kurser/kurs/IL2232?l=en) at KTH Royal Institute of Technology, under the supervision of Professor [Ingo Sander](https://www.kth.se/profile/ingo), who served as the project client.

A pre-study related to this project was conducted as part of the [II2211 Research Methodology and Scientific Writing for Embedded Systems](https://www.kth.se/student/kurser/kurs/II2211?l=en) course, where a literature review was carried out to investigate existing approaches to developing a compiler which translates ForSyDe models. This pre-study can be found under the [docs](docs/ForSyDe-DevTools-PreStudy.pdf).

The original algorithm for generating code from SDF (Synchronous Data Flow) models written in ForSyDe can be found in this [paper](https://www.icas.org/icas_archive/ICAS2022/data/papers/ICAS2022_0604_paper.pdf).

## Compiler and LSP installation
The ForSyDe DevTools compiler and LSP have been developed using a specific version of the GHC API as the frontend; the current version of GHC being used is `9.10.2`. Thus, `stack` is required to build the devtools. We suggest using [`ghcup`](https://www.haskell.org/ghcup/install/) to install it. Using your system's package manager should work too.

The ForSyDe DevTools compiler requires the OpenBLAS and LAPACK external libraries. These must be installed separately using your system's package manager. On Debian-based systems, use the command:
```
apt install libblas-dev liblapack-dev
```

With all dependencies resolved, you can install `forsyde-devtools` by cloning this repository and running `stack install` to build and install the compiler and language server.
```
git clone https://github.com/sthaeron/forsyde-devtools.git
cd forsyde-devtools
stack install
```

This installs the compiler and LSP to `~/.local/bin`. Make sure this directory is in your shell path. Check by running `echo $PATH`. If it is not included, add `export PATH="$HOME/.local/bin:$PATH"` to your shell configuration file. Then reinitialise your terminal shell. If you have successfully installed the devtools in your shell's path, you should be able to run the following commands:
```
stack exec forsyde-compiler-exe -- --help
stack exec forsyde-lsp-exe -- --help
```

For more information on how to use the compiler and LSP provided by the ForSyDe DevTools project, refer to the [user guide](docs/user-guide.md).

## Visualiser VSCode extension installation

First, install the compiler and LSP as described in the previous section. Then, install the [KLighD Diagrams](https://marketplace.visualstudio.com/items?itemName=kieler.klighd-vscode) and [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) VSCode extensions. You will also need `npm`, at least version `11.11.*`, to build and package the extension. We recommend installing `npm` through the following [guide](https://nodejs.org/en/download).

To build and install the VSCode extension, from the root of the `forsyde-devtools` repo, run the commands:
```sh
cd ./vscode-ext
npm install
npm run compile
npm run package
code --install-extension forsyde-vscode-extension-0.1.0.vsix
cd ..
```

For more information on how to setup and use the visualiser VSCode extension provided by the ForSyDe DevTools project, refer to the [user guide](docs/user-guide.md).

## Contributing
To contribute to the project we recommend using Nix to setup your Haskell development environment. You can install Nix onto your system with the following instructions at [nixos.org](https://nixos.org/download/). This project makes use of nix flakes which need to be manually enabled by adding the following to your `nix.conf`:
```
experimental-features = nix-command flakes
```

Automatically entering and exiting the Haskell development environment can be accomplished by installing the utilities [`direnv`](https://direnv.net/) and [`nix-direnv`](https://github.com/nix-community/nix-direnv). Upon entering the project directory or changing the `.envrc` file you will be prompted to run the command `direnv allow`, which will allow for the automatic loading and unloading of the nix flake.

The compiler and LSP can be individually built and run using the `nix build` and `nix run` commands, such as:
```sh
nix build
nix run ".#forsyde-lsp" -- --help
nix run ".#forsyde-compiler" -- examples/model/SDF_example_008.hs --stdout
```

For more information about contributing refer to our contribution [docs](docs/contribution.md).

## Project Contributors
- Michel Delli Abo
- Samuel Miksits
- Klara Modin
- Mohammad Afif Ramadhan
- Sebastian Thaeron
- Zicong Zhang

## Acknowledgements
We would like to thank:
- Professor Ingo Sander – for supervising, mentoring, and giving us the opportunity to contribute to his research project.
- [The Real-Time and Embedded Systems Group](https://www.uni-kiel.de/de/tf/forschen/institut-informatik/echtzeitsysteme/eingebettete-systeme) at [Kiel University](https://www.uni-kiel.de/de/) – for their time, support, and assistance related to the [KIELER](https://github.com/kieler) research project.
- Lecturer Elias Flening – for teaching and mentoring us in project management during the course IL2232 Embedded Systems Design Project at KTH.
- Associate Professor Jiantong Li and Associate Professor Johnny Öberg – for teaching us research methodology and scientific writing in the course II2211 Research Methodology and Scientific Writing for Embedded Systems at KTH.
