module ArgumentsLSP where

import Options.Applicative

-- Which file to use for input
data Input
  = InputFile FilePath
  | FromClient

data Output
  = ToClient
  | StdOut

-- Host IP(V4) address
data IP
  = Host String

-- Port for the server
data Port
  = TCP String

-- LSP communication method
data ProtocolCommunication
  = CommTcp
  | CommStdio

-- Arguments, kind of horrible names but used to not shadow usage of ip/port
-- in main. Contains an ip address, port and file name for synthesis in the
-- server
data Arguments = Arguments
  { -- LSP protocol communication
    communication :: ProtocolCommunication,
    -- Server connection information, if protocol communication is TCP
    hostIp :: IP,
    serverPort :: Port,
    -- What source file to use. Runs in single shot mode if not FromClient
    input :: Input
  }

inputTop :: Parser Input
inputTop =
  inputFile
    <|> inputFromClient

-- Handle file input. Default is to get the file from your client, but
-- allow user to pass a filename directly to server for debugging.
inputFile :: Parser Input
inputFile =
  argument
    (InputFile <$> str)
    ( metavar "INPUT"
        <> value FromClient
        <> help "Input filename (debug option), if unset get file from client"
    )

inputFromClient :: Parser Input
inputFromClient =
  flag'
    FromClient
    ( long "input-client"
        <> help "Input from client (default)"
    )

-- IP - String "127.0.0.1" by default, otherwise provide -a or --address and
-- the ip address
ipAddress :: Parser IP
ipAddress =
  option
    (Host <$> str)
    ( short 'a'
        <> long "address"
        <> metavar "IP"
        <> value (Host "127.0.0.1")
        <> help "Host IP"
    )

-- Port - String "5007" by default, otherwise provide -p or --port and
-- the port
tcpPort :: Parser Port
tcpPort =
  option
    (TCP <$> str)
    ( short 'p'
        <> long "port"
        <> metavar "PORT"
        <> value (TCP "5007")
        <> help "Host TCP Port"
    )

-- How to communicate with the LSP client, TCP by default
protocolComm :: Parser ProtocolCommunication
protocolComm =
  flag
    CommTcp
    CommTcp
    ( long "tcp"
        <> help "Protocol communcation over TCP (default)"
    )
    <|> flag'
      CommStdio
      ( long "stdio"
          <> help "Protocol communication over standard input/output"
      )

-- Top level argument parsing function
arguments :: Parser Arguments
arguments =
  Arguments
    <$> protocolComm
    <*> ipAddress
    <*> tcpPort
    <*> inputTop
