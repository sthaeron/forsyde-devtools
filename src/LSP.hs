{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Server
import Network.Socket
import Prettyprinter
import SKGraphSchema
import System.IO

diagramAcceptMethod :: SMethod (Method_CustomMethod "diagram/accept")
diagramAcceptMethod = (SMethod_CustomMethod (Proxy @"diagram/accept"))

setPreferencesMethod :: SMethod (Method_CustomMethod "keith/preferences/setPreferences")
setPreferencesMethod = (SMethod_CustomMethod (Proxy @"keith/preferences/setPreferences"))

setSynthesis :: A.Value
setSynthesis =
  A.object
    [ "clientId" .= T.pack "sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "setSyntheses",
            "syntheses"
              .= Seq.fromList
                [ A.object
                    [ "id" .= T.pack "se.kth.forsyde-devtools.dummyid",
                      "displayName" .= T.pack "ForSyDe Shallow"
                    ]
                ]
          ]
    ]

updateOptions :: A.Value
updateOptions =
  A.object
    [ "clientId" .= T.pack "sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "updateOptions",
            "valuedSynthesisOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "layoutOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "actions" .= (Seq.empty :: Seq.Seq A.Object),
            "modelUri" .= T.pack "file:///home/klara/git/plyghd-ls-demonstrator/empty.kgt"
          ]
    ]

requestBounds :: A.Value
requestBounds =
  A.object
    [ "clientId" .= T.pack "sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "requestBounds",
            "newRoot"
              .= KGraph
                { gid = T.pack "file:///home/klara/git/plyghd-ls-demonstrator/empty.kgt",
                  properties = [],
                  child =
                    KNode
                      { gid = "$root",
                        renderings = [],
                        properties = [],
                        children =
                          [ KNode
                              { children =
                                  [ KLabel {label = "A", gid = "$root$Na$$L0"},
                                    KPort
                                      { children = [],
                                        renderings = [],
                                        properties = [],
                                        gid = "$root$Na$$P0"
                                      }
                                  ],
                                renderings = [KEllipse],
                                properties =
                                  [ (NodeLabelsPlacement, [1, 4, 6]),
                                    (NodeSizeConstraints, [3]),
                                    (NodeSizeMinimum, [64, 64])
                                  ],
                                gid = "$root$Na"
                              },
                            KNode
                              { children =
                                  [ KLabel {label = "B", gid = "$root$Nb$$L0"},
                                    KPort
                                      { children = [],
                                        renderings = [],
                                        properties = [],
                                        gid = "$root$Nb$$P0"
                                      }
                                  ],
                                renderings = [KEllipse],
                                properties =
                                  [ (NodeLabelsPlacement, [1, 4, 6]),
                                    (NodeSizeConstraints, [3]),
                                    (NodeSizeMinimum, [64, 64])
                                  ],
                                gid = "$root$Nb"
                              },
                            KEdge
                              { children = [],
                                renderings = [KPolyline],
                                properties = [],
                                gid = "$root$Na$$P0$E0",
                                source = "$root$Na$$P0",
                                target = "$root$Nb$$P0"
                              }
                          ]
                      }
                }
          ]
    ]

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        pure (),
      notificationHandler setPreferencesMethod $ \_not -> do
        pure (),
      notificationHandler diagramAcceptMethod $ \_not -> do
        sendNotification diagramAcceptMethod setSynthesis
        sendNotification diagramAcceptMethod updateOptions
        sendNotification diagramAcceptMethod requestBounds
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
