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
import { readdir, stat } from "fs/promises";
import { homedir } from "os";
import * as path from "path";

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

  const serverOptions: ServerOptions = await createServerOptions(context);

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

async function createServerOptions(
  context: ExtensionContext,
): Promise<ServerOptions> {
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
    // An explicitly configured package DB path always wins; otherwise try to
    // auto-detect it in the Stack root.
    let stackPkgPath: string =
      vscode.workspace.getConfiguration("forsydeDevtoolsLSP").stackPkgPath;
    if (!stackPkgPath || stackPkgPath.length === 0) {
      const detected = await detectStackPkgPath();
      if (detected) {
        console.log("Auto-detected forsyde-shallow package DB: ", detected);
        stackPkgPath = detected;
      } else {
        vscode.window.showErrorMessage(
          "ForSyDe DevTools LSP: no package database containing " +
            "forsyde-shallow was found in the Stack root. Run `stack install` " +
            "in the forsyde-devtools repository, or set " +
            "`forsydeDevtoolsLSP.stackPkgPath` manually in the settings.",
        );
      }
    }
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

/**
 * Locate the Stack package DB containing forsyde-shallow by scanning
 * <stackRoot>/snapshots/<arch>/<hash>/<ghc-version>/pkgdb for a
 * forsyde-shallow-*.conf file. The Stack root is taken from the STACK_ROOT
 * environment variable, falling back to ~/.stack. If several package DBs
 * match, the one with the most recently modified .conf file is returned.
 */
async function detectStackPkgPath(): Promise<string | undefined> {
  const stackRoot = process.env.STACK_ROOT ?? path.join(homedir(), ".stack");
  const snapshotsDir = path.join(stackRoot, "snapshots");

  let bestPkgdb: string | undefined;
  let bestMtimeMs = -Infinity;

  for (const pkgdb of await findPkgdbDirs(snapshotsDir)) {
    try {
      const conf = (await readdir(pkgdb)).find(
        (name) => name.startsWith("forsyde-shallow-") && name.endsWith(".conf"),
      );
      if (!conf) {
        continue;
      }
      const { mtimeMs } = await stat(path.join(pkgdb, conf));
      if (mtimeMs > bestMtimeMs) {
        bestMtimeMs = mtimeMs;
        bestPkgdb = pkgdb;
      }
    } catch {
      // Unreadable package DB; skip it.
    }
  }

  return bestPkgdb;
}

/**
 * List all pkgdb directories located at <snapshotsDir>/<arch>/<hash>/<ghc-version>/pkgdb.
 */
async function findPkgdbDirs(snapshotsDir: string): Promise<string[]> {
  let dirs = [snapshotsDir];

  // Descend the three fixed levels: <arch>/<hash>/<ghc-version>.
  for (let depth = 0; depth < 3; depth++) {
    const subdirs: string[] = [];
    for (const dir of dirs) {
      try {
        for (const entry of await readdir(dir, { withFileTypes: true })) {
          if (entry.isDirectory()) {
            subdirs.push(path.join(dir, entry.name));
          }
        }
      } catch {
        // Unreadable directory; skip it.
      }
    }
    dirs = subdirs;
  }

  return dirs
    .map((dir) => path.join(dir, "pkgdb"))
    .filter((pkgdb) => existsSync(pkgdb));
}
