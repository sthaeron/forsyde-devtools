# ForSyDe DevTools
Compiler and visualisation development tools for ForSyDe Shallow

### Project Contributors
- Michel Delli Abo
- Samuel Miksits
- Klara Modin
- Mohammad Afif Ramadhan
- Sebastian Thaeron
- Zicong Zhang

## Development environment
Enter development environment using `nix develop`.
Once in environment you can use `nix build` and run the binary at `./result/bin/forsyde-devtools-exe` or use `cabal repl` and run `main`.
Adding haskell build dependencies is done through the `forsyde-devtools.project`. Make sure to re-enter development environment for nix to install new dependency!

## Documentation
Documentation is available on the `docs` directory.

### Build the docs
Build the webpage for the documents by running:
```bash
nix develop
mkdocs build
```
The generated static site will be available at newly created `site` directory

### Serve the docs
Initialize the nix-shell as noted above.
Without building it, you can serve it on your own instance:
```bash
mkdocs serve
```
