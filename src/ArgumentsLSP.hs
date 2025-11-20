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

-- Arguments, kind of horrible names but used to not shadow usage of ip/port
-- in main. Contains an ip address, port and file name for synthesis in the
-- server
data Arguments = Arguments
  { -- Files
    hostIp :: IP,
    serverPort :: Port,
    -- Formats
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
        <> action "file"
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

-- Top level argument parsing function. 3 mandatory arguments but
-- ip and port have default values.
arguments :: Parser Arguments
arguments =
  Arguments
    <$> ipAddress
    <*> tcpPort
    <*> inputTop
