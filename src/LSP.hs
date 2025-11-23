{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import ArgumentsLSP
import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import CoreIRToForSyDeIR
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty as AP
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import ForSyDeIR
import Language.LSP.Protocol.Message
import Language.LSP.Server
import Network.Socket
import Options.Applicative
import Prettyprinter
import SKGraphSchema
import System.IO
import Utilities

diagramAcceptMethod :: SMethod (Method_CustomMethod "diagram/accept")
diagramAcceptMethod = (SMethod_CustomMethod (Proxy @"diagram/accept"))

setPreferencesMethod :: SMethod (Method_CustomMethod "keith/preferences/setPreferences")
setPreferencesMethod = (SMethod_CustomMethod (Proxy @"keith/preferences/setPreferences"))

-- | Convert ForSyDe IR into the graph representation understood by KLighD
forSyDeIRToGraph :: FilePath -> IRSystem -> GraphElement
forSyDeIRToGraph file (IRSystem (inputs, outputs) actors signals _) = graph
  where
    -- \| Create a port with a label for signal rate
    createPortWithRate pid renderings properties (n, r) =
      createPort' renderings properties [label] (id, r)
      where
        id = pid <> "$P$" <> T.show n
        label = KLabel {gid = id <> "$L$" <> T.show n, label = T.show r}
    createPortWithoutRate pid renderings properties (n, r) =
      createPort' renderings properties [] (id, r)
      where
        id = pid <> "$P$" <> T.show n
    -- \| Create a port with the passed renderings and children
    createPort' renderings properties children (gid, _) =
      KPort
        { children = children,
          renderings = renderings,
          properties = properties,
          gid = gid
        }
    -- \| Create a node based on an IRActor
    createNode = \case
      (IRActor name _ _ _) ->
        createNode'
          name
          (createPortWithRate)
          (Just name)
          [KRoundedRectangle [KBackgroundColor 160 160 240] 4 4]
          [ (NodeLabelsPlacement [1, 4, 6]),
            (NodeSizeConstraints [0, 1, 2, 3]),
            (NodeSizeMinimum [64, 64])
          ]
      (IRDelay name _ _) ->
        createNode'
          name
          (createPortWithoutRate)
          (Nothing :: Maybe IRId)
          [KEllipse [KBackgroundColor 0 0 0]]
          [ (NodeLabelsPlacement [1, 4, 6]),
            (NodeSizeConstraints [3]),
            (NodeSizeMinimum [12, 12])
          ]
    -- \| Find all signals which the process is the source of
    findOutputSignals signals proc =
      foldr f [] signals
      where
        f s acc =
          let IRSignal n (p, rate) _ = s
           in if p == proc then (n, rate) : acc else acc
    -- \| Find all signals which the process is the target of
    findInputSignals signals proc =
      foldr f [] signals
      where
        f s acc =
          let IRSignal n _ (p, rate) = s
           in if p == proc then (n, rate) : acc else acc
    -- \| Helper for createNode and global inputs / outputs
    createNode' name createPort l r p = node
      where
        nid = "$root$N$" <> T.show name
        insignals = findInputSignals signals name
        outsignals = findOutputSignals signals name
        inports = map (createPort nid (maybe [] (\_l -> [KText "◆" []]) l) []) insignals
        outports = map (createPort nid [] []) outsignals
        nl = maybe [] (\l -> [KLabel {gid = nid <> "$L$" <> T.show name, label = T.show l}]) l
        c = inports ++ outports ++ nl
        node =
          KNode
            { gid = nid,
              children = c,
              renderings = r,
              properties = p
            }
    -- \| Create an edge from an IRSignal, depends on port id
    createEdge (IRSignal n (sname, _) (tname, _)) = edge
      where
        sn = "$root$N$" <> T.show sname <> "$P$" <> T.show n
        tn = "$root$N$" <> T.show tname <> "$P$" <> T.show n
        name = sn <> "$E$" <> T.show n
        sigid = name <> "$L$" <> T.show n
        children =
          if n == sname || n == tname
            then []
            else [KLabel {gid = sigid, label = T.show n}]
        edge =
          KEdge
            { gid = name,
              children = children,
              renderings = [KRoundedBendsPolyline [] 4],
              properties = [],
              source = sn,
              target = tn
            }
    nodes =
      map createNode actors
        ++ map (\n -> createNode' n (createPortWithoutRate) (Just n) [] [LayerConstraint 2]) inputs
        ++ map (\n -> createNode' n (createPortWithoutRate) (Just n) [] [LayerConstraint 4]) outputs
    edges = map createEdge signals
    graph =
      KGraph
        { gid = T.pack ("file://" ++ file),
          child =
            KNode
              { gid = "$root",
                children = nodes ++ edges,
                properties = [],
                renderings = []
              },
          properties = []
        }

-- | Send our supported syntheses to the LSP client (KLighD-VSCode)
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

-- | Send the supported options for the file
updateOptions :: FilePath -> A.Value
updateOptions f =
  A.object
    [ "clientId" .= T.pack "sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "updateOptions",
            "valuedSynthesisOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "layoutOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "actions" .= (Seq.empty :: Seq.Seq A.Object),
            "modelUri" .= T.pack ("file://" ++ f)
          ]
    ]

-- | Send the graph for layout and display to the LSP client (KLighD-VSCode)
requestBounds :: FilePath -> IRSystem -> A.Value
requestBounds f ir =
  A.object
    [ "clientId" .= T.pack "sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "requestBounds",
            "newRoot"
              .= forSyDeIRToGraph f ir
          ]
    ]

-- | The static notification and request handlers we support
handlers :: Input -> Handlers (LspM (Maybe FilePath))
handlers f =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        pure (),
      notificationHandler setPreferencesMethod $ \_not -> do
        pure (),
      notificationHandler diagramAcceptMethod $ \TNotificationMessage {_params = p} -> do
        -- In the case where the client does not provide a sourceUri, use the
        -- old one. This is the case for e.g. the refreshDiagram action
        c <- getConfig
        let file = maybe (getFile p) id c
        _ <- setConfig (Just file)
        sendNotification diagramAcceptMethod setSynthesis
        sendNotification diagramAcceptMethod (updateOptions file)
        (core, dflags) <- withRunInIO (\_u -> compileToCore file)
        let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
        let graphMessage = requestBounds file forsydeIR
        sendNotification diagramAcceptMethod graphMessage
        pure ()
    ]
  where
    getFile params = case f of
      FromClient -> case getFilePathFromClient params of
        Just _file -> _file
        Nothing -> ""
      InputFile fn -> fn
    getKey key = \case
      A.Object o -> o !? key
      _ -> Nothing
    getFilePathFromClient params =
      Just params
        >>= getKey "action"
        >>= getKey "options"
        >>= getKey "sourceUri"
        >>= \case
          A.String _a -> Just $ T.unpack $ snd $ T.splitAt 7 _a
          _ -> Nothing

runServerC :: Handle -> Handle -> ServerDefinition config -> IO Int
runServerC =
  runServerWithHandles
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))

-- | Process arguments for the LSP and run it
main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (arguments <**> helper)
        ( fullDesc
            <> progDesc "Run a ForSyDe LSP"
            <> header "ForSyDe DevTools"
        )

-- | Start the LSP
run :: Arguments -> IO ()
run (Arguments (Host ip) (TCP p) i_f) =
  case i_f of
    FromClient ->
      runTCPServer (Just ip) p lsp
      where
        lsp s = do
          handle <- socketToHandle s ReadWriteMode
          -- server returns IO Int, wrapper with "pure ()" so that expression
          -- returns IO ()
          _ <-
            runServerC handle handle $
              ServerDefinition
                { parseConfig = const $ const $ Right Nothing,
                  onConfigChange = const $ pure (),
                  defaultConfig = Nothing,
                  configSection = "demo",
                  doInitialize = \env _req -> pure $ Right env,
                  staticHandlers = \_caps -> (handlers FromClient),
                  interpretHandler = \env -> Iso (runLspT env) liftIO,
                  options = defaultOptions
                }
          pure ()
    InputFile f -> do
      (core, dflags) <- withRunInIO (\_u -> compileToCore f)
      let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
      let graphMessage = requestBounds f forsydeIR
      BSL8.putStrLn $ AP.encodePretty graphMessage

-- | Listen on host and port, as well as accept and fork off connections
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
