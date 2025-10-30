{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Server
import Network.Socket
import Prettyprinter
import System.IO

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        pure (),
      requestHandler (SMethod_CustomMethod (Proxy @"diagram/accept")) $ \_req _resp -> do
        pure ()
    ]

runServerC :: Handle -> Handle -> ServerDefinition config -> IO Int
runServerC =
  runServerWithHandles
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))

main :: IO Int
main =
  runTCPServer (Just "127.0.0.1") "5007" lsp
  where
    lsp s = do
      handle <- socketToHandle s ReadWriteMode
      runServerC handle handle $
        ServerDefinition
          { parseConfig = const $ const $ Right (),
            onConfigChange = const $ pure (),
            defaultConfig = (),
            configSection = "demo",
            doInitialize = \env _req -> pure $ Right env,
            staticHandlers = \_caps -> handlers,
            interpretHandler = \env -> Iso (runLspT env) liftIO,
            options = defaultOptions
          }

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a1) -> IO a2
runTCPServer host port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) host (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
