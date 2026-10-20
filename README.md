# ForSyDe DevTools
The ForSyDe DevTools project provides a compiler and visualiser for [ForSyDe Shallow](https://forsyde.github.io/forsyde-shallow/), a modelling framework written in Haskell. The project was conducted as part of the course [IL2232 Embedded Systems Design Project](https://www.kth.se/student/kurser/kurs/IL2232?l=en) at KTH Royal Institute of Technology, under the supervision of Professor [Ingo Sander](https://www.kth.se/profile/ingo), who served as the project client.

A pre-study related to this project was conducted as part of the [II2211 Research Methodology and Scientific Writing for Embedded Systems](https://www.kth.se/student/kurser/kurs/II2211?l=en) course, where a literature review was carried out to investigate existing approaches to developing a compiler which translates ForSyDe models. This pre-study can be found under the [docs](docs/ForSyDe-DevTools-PreStudy.pdf).

The original algorithm for generating code from SDF (Synchronous Data Flow) models written in ForSyDe can be found in this [paper](https://www.icas.org/icas_archive/ICAS2022/data/papers/ICAS2022_0604_paper.pdf).

## Compiler and LSP installation
The ForSyDe DevTools compiler and LSP have been developed using a specific version of the GHC API as the frontend; the current version of GHC being used is `9.10.2`. Thus, `stack` is required to build the devtools. We suggest using [`ghcup`](https://www.haskell.org/ghcup/install/) to install it. Using your system's package manager should work too.

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
The development environment is managed with [mise](https://mise.jdx.dev). It installs ghcup, cabal and the Haskell formatters, and its `setup` task installs GHC and HLS through ghcup and enables the git hooks:
```sh
mise install
mise run setup
```

GHC also needs a few system libraries, on Fedora `gmp-devel ncurses-devel zlib-devel` and on Debian/Ubuntu `libgmp-dev libncurses-dev zlib1g-dev`.

The compiler and LSP are built and tested with cabal:
```sh
cabal build
cabal test
./scripts/test.sh --cabal
```

The documentation site can be served locally with `mise run docs`.

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
