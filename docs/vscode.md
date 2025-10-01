# Visual Studio Code Extension

Visualisation of the ForSyDe model is displayed inside the Visual Studio Code (VS Code) editor. The VS Code extension is explained as follows.

## VS Code Extension

A VS Code extension is a packaged program that can be released publicly for others to install in their VS Code instance.

The extension is a JavaScript/TypeScript application that runs on the VS Code engine (an Electron application based on Node.js).

## Diagram View via Webview API

The diagram is displayed in a VS Code Webview, which is an HTML window rendered inside a tab. We can consider it an `iframe` provided by VS Code that we inject our diagram into.

## Diagram Generation

The diagram is generated using KIELER, which uses Sprotty, which in turn uses the Eclipse Layout Kernel (ELK). The result is an SVG rendered by the KIELER program, which allows the diagram to be moved, selected, and highlighted.
