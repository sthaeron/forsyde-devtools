import { connect, NetConnectOpts, Socket } from "net";
import { ExtensionContext } from "vscode";
import * as vscode from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
} from "vscode-languageclient/node";

import { existsSync } from "fs";

let client: LanguageClient;
let socket: Socket;

export async function activate(context: ExtensionContext) {
  vscode.window.showInformationMessage("ForSyDe DevTools LSP activated.");

  vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration("forsydeDevtoolsLSP")) {
      vscode.window
        .showInformationMessage(
          "ForSyDe DevTools LSP config changed. Restart to apply changes. ",
          "Restart Visual Studio Code",
          "Restart Later",
        )
        .then((sel) => {
          if (sel === "Restart Visual Studio Code") {
            vscode.commands.executeCommand("workbench.action.reloadWindow");
          }
        });
    }
  });

  // Run the ForSyDe compiler on the file in the active editor (issue #322)
  context.subscriptions.push(
    vscode.commands.registerCommand("forsyde-devtools.compileFile", () => {
      const document = vscode.window.activeTextEditor?.document;
      if (!document || !document.fileName.endsWith(".hs")) {
        vscode.window.showErrorMessage(
          "ForSyDe DevTools: open a Haskell model file to compile it.",
        );
        return;
      }
      const terminal =
        vscode.window.terminals.find((t) => t.name === "ForSyDe Compiler") ??
        vscode.window.createTerminal("ForSyDe Compiler");
      terminal.show();
      const { stackPkgPath } =
        vscode.workspace.getConfiguration("forsydeDevtoolsLSP");
      let compileCommand = `forsyde-compiler-exe "${document.fileName}"`;
      if (stackPkgPath && stackPkgPath.length > 0) {
        compileCommand += ` --forsyde-pkgpath "${stackPkgPath}"`;
      }
      terminal.sendText(compileCommand);
    }),
  );

  const serverOptions: ServerOptions = createServerOptions(context);

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", pattern: "**/*.hs" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.hs"),
    },
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "ForSyDe DevTools LSP",
    serverOptions,
    clientOptions,
    true,
  );

  // Inform the KLighD extension about the LS client and supported file endings
  await vscode.commands.executeCommand<string>(
    "klighd-vscode.setLanguageClient",
    client,
    ["hs"],
  );

  // Start the client. This will also launch the server
  console.debug("Starting ForSyDe Language Server...");
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return new Promise<void>((resolve) => {
    if (socket) {
      socket.end(resolve);
      return;
    }
    client?.stop().then(resolve);
  });
}

function createServerOptions(context: ExtensionContext): ServerOptions {
  // Connect to language server via socket if a port is specified as an env variable
  if (typeof process.env.dev !== "undefined") {
    const connectionInfo: NetConnectOpts = {
      port: 5007,
    };
    console.log("Connecting to language server on port: ", connectionInfo.port);

    return async () => {
      socket = connect(connectionInfo);
      const result: StreamInfo = {
        writer: socket,
        reader: socket,
      };
      return result;
    };
  } else {
    const { stackPkgPath } =
      vscode.workspace.getConfiguration("forsydeDevtoolsLSP");
    console.log("Spawning to language server as a process.");
    const lsp_executable = context.asAbsolutePath(`server/forsyde-lsp-exe`);
    const stack_config = context.asAbsolutePath(`client/stack.yaml`);

    const args = ["exec", "--stack-yaml", stack_config, "--", "forsyde-lsp-exe", "--stdio"];
    if (stackPkgPath && stackPkgPath.length > 0) {
      args.push("--forsyde-pkgpath", stackPkgPath);
    }

    if (existsSync(lsp_executable)) {
      return {
        run: { command: lsp_executable, args },
        debug: { command: lsp_executable, args },
      };
    } else {
      return {
        run: { command: `stack`, args },
        debug: { command: `stack`, args },
      };
    }
  }
}
