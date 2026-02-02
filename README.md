# ForSyDe DevTools
The ForSyDe DevTools project provides a compiler and visualiser for [ForSyDe Shallow](https://forsyde.github.io/forsyde-shallow/), a modelling framework written in Haskell. The project was conducted as part of the course [IL2232 Embedded Systems Design Project](https://www.kth.se/student/kurser/kurs/IL2232?l=en) at KTH Royal Institute of Technology, under the supervision of Professor [Ingo Sander](https://www.kth.se/profile/ingo), who served as the project client.

A pre-study related to this project was conducted as part of the [II2211 Research Methodology and Scientific Writing for Embedded Systems](https://www.kth.se/student/kurser/kurs/II2211?l=en) course, where a literature review was carried out to investigate existing approaches to developing a compiler which translates ForSyDe models. This pre-study can be found under the [docs](docs/ForSyDe-DevTools-PreStudy.pdf).

The original algorithm for generating code from SDF (Synchronous Data Flow) models written in ForSyDe can be found in this [paper](https://www.icas.org/icas_archive/ICAS2022/data/papers/ICAS2022_0604_paper.pdf).

## Compiler and LSP installation
The ForSyDe DevTools compiler and LSP have been developed using a specific version of the GHC API as the frontend. The current version of GHC being used is `9.10.2`. We suggest installing this specific version of GHC using `ghcup`. The installation instructions for `ghcup` can be found [here](https://www.haskell.org/ghcup/install/); all config options can be left as default.

Once you have `ghcup` installed, you can use the following commands to install and set GHC `9.10.2` as your current version:

```
ghcup install ghc 9.10.2
ghcup set ghc 9.10.2
```

The ForSyDe DevTools compiler depends on the OpenBLAS and LAPACK external libraries. These have to be installed separately, using your system's package manager. For a Debian-based system, this would be:

```
apt install libblas-dev liblapack-dev
```

To be able to run the ForSyDe DevTools compiler and LSP without using `stack` or `cabal`, the ForSyDe Shallow library needs to be installed globally on your system. This can be done with the following command:

```
cabal v1-install forsyde-shallow
```

From this point, you can simply `clone` this repository and use `stack install` to build and install the compiler and language server.

```
git clone https://github.com/sthaeron/forsyde-devtools.git
cd forsyde-devtools
stack install
```

This will install the compiler and LSP to `~/.local/bin`. Make sure this directory is in your shell path. Check by running `echo $PATH`. If it is not included, add `export PATH="$HOME/.local/bin:$PATH"` to your shell configuration file. Then reinitialise your terminal shell.

If you have successfully installed and added the compiler and LSP executables to your shell path, you should be able to run the following commands:

```
forsyde-compiler-exe --help
forsyde-lsp-exe --help
```

For more information on how to use the compiler and LSP provided by the ForSyDe DevTools project, refer to the [user guide](docs/user-guide.md).

## Visualiser VSCode extension installation

First, install the compiler and LSP as described in the previous section.

Install the VSCode extensions [KLighD
Diagrams](https://marketplace.visualstudio.com/items?itemName=kieler.klighd-vscode)
and
[Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
In addition, you also need
[npm](https://nodejs.org/en/learn/getting-started/an-introduction-to-the-npm-package-manager)
for building the extension.

To build and install the VSCode extension:
```sh
cd ./vscode-ext
npm install
npm run compile
./node_modules/vsce/vsce package
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
