# ForSyDe DevTools

Compiler and visualisation development tools for ForSyDe Shallow

### Project Contributors

- Michel Delli Abo
- Samuel Miksits
- Klara Modin
- Mohammad Afif Ramadhan
- Sebastian Thaeron
- Zicong Zhang

## Documentation

Documentation is available on the `docs` directory.

### Build the docs

Initialize the shell by invoking

```bash
nix-shell
```

Build the webpage for the documents by running:

```bash
mkdocs build
```

The generated static site will be available at newly created `site` directory

### Serve the docs

Initialize the nix-shell as noted above.
Without building it, you can serve it on your own instance:

```bash
mkdocs serve
```
