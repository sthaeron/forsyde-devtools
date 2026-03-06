# ForSyDe DevTools VS Code Extension

This extension provides LSP integration and diagram visualization for [ForSyDe Shallow](https://forsyde.github.io/forsyde-shallow/) models in VS Code.

## Prerequisites

1. **ForSyDe LSP Server** — `forsyde-lsp-exe` must be installed and available on your PATH.
   - See the main [ForSyDe DevTools README](../README.md) for installation instructions.

2. **KLighD Diagrams Extension** — Install from the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=kieler.klighd-vscode).

3. **ForSyDe Shallow Package** — If you installed `forsyde-shallow` via Stack, you may need to configure the package database path in VS Code settings.

## Installation

### From .vsix file

1. Download the `.vsix` file from the [releases](https://github.com/sthaeron/forsyde-devtools/releases) page.
2. In VS Code, open the Extensions sidebar (Ctrl+Shift+X).
3. Click the "..." menu at the top and select **Install from VSIX...**.
4. Select the downloaded `.vsix` file.
5. Reload VS Code when prompted.

### From source

```sh
cd vscode-ext
npm install
npm run compile
npm run package
code --install-extension forsyde-vscode-extension-0.1.0.vsix
```

## Configuration

Open VS Code settings and search for "ForSyDe DevTools LSP":

- **`forsydeDevtoolsLSP.stackPkgPath`** — Path to the Stack package database containing `forsyde-shallow`. Required if you installed via Stack instead of Cabal.

  To find this path, run:
  ```sh
  find $HOME/.stack -name '*forsyde-shallow*' | grep -o '^.*/pkgdb'
  ```

## Usage

1. Open a `.hs` file containing a ForSyDe model.
2. The extension activates automatically for Haskell files.
3. Click the diagram icon in the editor toolbar, or right-click and select **Open Diagram**.
4. The KLighD diagram view will display the model visualization.

## Development

### Running in development mode

1. Run `npm install` in the `vscode-ext` directory.
2. Open the folder in VS Code.
3. Press Ctrl+Shift+B to start the TypeScript compiler in watch mode.
4. Press F5 to launch the Extension Development Host.
5. Open a ForSyDe `.hs` file to test.

### Building a .vsix package

```sh
npm run package
```

This generates `forsyde-vscode-extension-0.1.0.vsix` in the current directory.

## Troubleshooting

### "Connection to Language Server got closed"

Ensure `forsyde-lsp-exe` is on your PATH:
```sh
which forsyde-lsp-exe
```

If it's not found, install the ForSyDe DevTools compiler and LSP following the main README instructions.

### Diagrams not rendering

Ensure the KLighD Diagrams extension is installed and enabled.

## License

MIT License — see [LICENSE](../LICENSE).
