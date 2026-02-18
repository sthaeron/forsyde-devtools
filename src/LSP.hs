{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import ArgumentsLSP
import Colog.Core ((<&))
import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import CoreIRToForSyDeIR (translateCoreProgram)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import Data.Function
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import ForSyDeIR
import qualified Language.LSP.Logging as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Network.Socket
import Options.Applicative
import Prettyprinter
import SKGraphSchema
import System.IO
import Utilities (compileToCoreWithForSyDePath)

diagramAcceptMethod :: LSP.SMethod (LSP.Method_CustomMethod "diagram/accept")
diagramAcceptMethod = (LSP.SMethod_CustomMethod (Proxy @"diagram/accept"))

diagramOpenInTextEditor :: LSP.SMethod (LSP.Method_CustomMethod "diagram/openInTextEditor")
diagramOpenInTextEditor = (LSP.SMethod_CustomMethod (Proxy @"diagram/openInTextEditor"))

setPreferencesMethod :: LSP.SMethod (LSP.Method_CustomMethod "keith/preferences/setPreferences")
setPreferencesMethod = (LSP.SMethod_CustomMethod (Proxy @"keith/preferences/setPreferences"))

-- | Convert ForSyDe IR into the graph representation understood by KLighD
forSyDeIRToGraph :: FilePath -> IRSystem -> GraphElement
forSyDeIRToGraph filename (IRSystem (inputs, outputs) actors signals _) = graph
  where
    -- \| Create a port with a label for signal rate
    createPortWithoutRate parent rends props (n, r) =
      createPort' rends props [] (pid, r)
      where
        pid = parent <> "$P$" <> T.show n
    -- \| Create a port with the passed renderings and children
    createPort' rends props c (pid, _) =
      KPort
        { children = c,
          renderings = rends,
          properties = props,
          gid = pid
        }
    -- \| Create a node based on an IRActor
    createNode = \case
      (IRActor name _ _ _) ->
        createNode'
          name
          (createPortWithoutRate)
          (Just name)
          [KRoundedRectangle [KBackgroundColor 160 160 240] 4 4]
          [ (NodeLabelsPlacement [1, 4, 6]),
            (NodeSizeConstraints [0, 1, 2, 3]),
            (NodeSizeMinimum [64, 64]),
            (PortConstraints 2)
          ]
      (IRDelay name _ _) ->
        createNode'
          name
          (createPortWithoutRate)
          (Nothing :: Maybe IRId)
          [KEllipse [KBackgroundColor 0 0 0]]
          [ (NodeLabelsPlacement [1, 4, 6]),
            (NodeSizeConstraints [3]),
            (NodeSizeMinimum [12, 12]),
            (PortConstraints 2)
          ]
    -- \| Find all signals which the process is the source of
    findOutputSignals sigs proc =
      foldr f [] sigs
      where
        f s acc =
          let IRSignal n (p, rate) _ = s
           in if p == proc then (n, rate) : acc else acc
    -- \| Find all signals which the process is the target of
    findInputSignals sigs proc =
      foldr f [] sigs
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
        inports = map (createPort nid (maybe [] (\_l -> [KText "▶" []]) l) [PortSide 4]) insignals
        outports = map (createPort nid [] [PortSide 2]) outsignals
        nl = maybe [] (\lc -> [KLabel {gid = nid <> "$L$" <> T.show name, label = T.show lc, properties = []}]) l
        c = inports ++ outports ++ nl
        node =
          KNode
            { gid = nid,
              children = c,
              renderings = r,
              properties = p
            }
    -- \| Helper to check if an id belongs to a delay
    delayId did lactors = case lactors of
      IRDelay aid _ _ : xs
        | aid == did -> True
        | otherwise -> delayId did xs
      IRActor aid _ _ _ : xs
        | aid == did -> False
        | otherwise -> delayId did xs
      [] -> False
    -- \| Create an edge from an IRSignal, depends on port id
    createEdge (IRSignal n (sname, srate) (tname, trate)) = edge
      where
        sn = "$root$N$" <> T.show sname <> "$P$" <> T.show n
        tn = "$root$N$" <> T.show tname <> "$P$" <> T.show n
        name = sn <> "$E$" <> T.show n
        sigid = name <> "$L$" <> T.show n
        srclabel = if delayId sname actors then [] else [KLabel {gid = sigid <> T.show sname, label = T.show srate, properties = [EdgeLabelsPlacement 2]}]
        tgtlabel = if delayId tname actors then [] else [KLabel {gid = sigid <> T.show tname, label = T.show trate, properties = [EdgeLabelsPlacement 1]}]
        c =
          if n == sname
            then tgtlabel
            else
              if n == tname
                then srclabel
                else
                  [KLabel {gid = sigid, label = T.show n, properties = []}]
                    <> srclabel
                    <> tgtlabel
        edge =
          KEdge
            { gid = name,
              children = c,
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
        { gid = T.pack ("file://" ++ filename),
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
setSynthesis :: T.Text -> A.Value
setSynthesis _clientId =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("setSyntheses" :: T.Text),
            "syntheses"
              .= Seq.fromList
                [ A.object
                    [ "id" .= ("se.kth.forsyde-devtools.dummyid" :: T.Text),
                      "displayName" .= ("ForSyDe Shallow" :: T.Text)
                    ]
                ]
          ]
    ]

-- | Send the supported options for the file
updateOptions :: FilePath -> T.Text -> A.Value
updateOptions f _clientId =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("updateOptions" :: T.Text),
            "valuedSynthesisOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "layoutOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "actions" .= (Seq.empty :: Seq.Seq A.Object),
            "modelUri" .= T.pack ("file://" ++ f)
          ]
    ]

-- | Send the graph for layout and display to the LSP client (KLighD-VSCode)
requestBounds :: FilePath -> T.Text -> IRSystem -> A.Value
requestBounds f _clientId ir =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("requestBounds" :: T.Text),
            "newRoot"
              .= forSyDeIRToGraph f ir
          ]
    ]

diagramOpenInTextEditorMessage :: String -> Int -> Int -> Int -> Int -> A.Value
diagramOpenInTextEditorMessage uri sline scol eline ecol =
  A.object
    [ "location"
        .= A.object
          [ "uri" .= ("file://" <> uri),
            "range"
              .= A.object
                [ "start"
                    .= A.object
                      [ "line" .= (sline - 1),
                        "character" .= (scol - 1)
                      ],
                  "end"
                    .= A.object
                      [ "line" .= (eline - 1),
                        "character" .= (ecol - 1)
                      ]
                ]
          ],
      "forceOpen" .= False
    ]

data Config = Config
  { file :: Maybe FilePath,
    clientId :: Maybe T.Text,
    system :: Maybe IRSystem,
    forSyDePkg :: Maybe FilePath
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { file = Nothing,
      clientId = Nothing,
      system = Nothing,
      forSyDePkg = Nothing
    }

-- | The static notification and request handlers we support
handlers :: LSP.Handlers (LSP.LspM Config)
handlers =
  mconcat
    [ LSP.notificationHandler LSP.SMethod_Initialized $ \_not -> pure (),
      LSP.notificationHandler setPreferencesMethod $ \_not -> pure (),
      LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_not -> pure (),
      LSP.requestHandler LSP.SMethod_Initialize $ \_req _resp -> do
        _resp
          ( Right $
              LSP.InitializeResult
                { _capabilities =
                    LSP.ServerCapabilities
                      { _positionEncoding = Nothing,
                        _textDocumentSync = Nothing,
                        _notebookDocumentSync = Nothing,
                        _completionProvider = Nothing,
                        _hoverProvider = Nothing,
                        _signatureHelpProvider = Nothing,
                        _declarationProvider = Nothing,
                        _definitionProvider = Nothing,
                        _typeDefinitionProvider = Nothing,
                        _implementationProvider = Nothing,
                        _referencesProvider = Nothing,
                        _documentHighlightProvider = Nothing,
                        _documentSymbolProvider = Nothing,
                        _codeActionProvider = Nothing,
                        _codeLensProvider = Nothing,
                        _documentLinkProvider = Nothing,
                        _colorProvider = Nothing,
                        _workspaceSymbolProvider = Nothing,
                        _documentFormattingProvider = Nothing,
                        _documentRangeFormattingProvider = Nothing,
                        _documentOnTypeFormattingProvider = Nothing,
                        _renameProvider = Nothing,
                        _foldingRangeProvider = Nothing,
                        _selectionRangeProvider = Nothing,
                        _executeCommandProvider = Nothing,
                        _callHierarchyProvider = Nothing,
                        _linkedEditingRangeProvider = Nothing,
                        _semanticTokensProvider = Nothing,
                        _monikerProvider = Nothing,
                        _typeHierarchyProvider = Nothing,
                        _inlineValueProvider = Nothing,
                        _inlayHintProvider = Nothing,
                        _diagnosticProvider = Nothing,
                        _workspace = Nothing,
                        _experimental = Nothing
                      },
                  _serverInfo = Nothing
                }
          )
        pure (),
      LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeWatchedFiles $ \_not -> do
        recomputeModel
        sendModel,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ \_not -> do
        recomputeModel
        sendModel,
      LSP.notificationHandler diagramAcceptMethod $ \LSP.TNotificationMessage {_params = p} -> do
        initialConfig <- LSP.getConfig
        -- What file should we use?
        let newfile = maybe (maybe "" id $ file initialConfig) id (getFilePathFromClient p)
        -- What clientId should we use?
        let newId = maybe (maybe ("sprotty" :: T.Text) id $ clientId initialConfig) id (getClientId p)
        -- Update config with new data
        LSP.setConfig
          initialConfig
            { file = Just newfile,
              clientId = Just newId
            }

        -- Recompute model and send if the client wants it
        if shouldUpdate p
          then recomputeModel >> sendModel
          else pure ()

        -- Get location information on selected object
        config <- LSP.getConfig
        let sel = getSelected p & map (T.split (\_c -> _c == '$')) & map last & map T.unpack
        let spans = getSelectedSpans sel config
        if length spans > 0
          then liftIO $ stderrLogger <& ("Selected: " <> T.show spans) `L.WithSeverity` L.Debug
          else pure ()

        -- Send selection messages for all of the currently selected elements
        _ <-
          traverse
            ( \(fname, sl, sc, el, ec) ->
                LSP.sendNotification diagramOpenInTextEditor $
                  diagramOpenInTextEditorMessage fname sl sc el ec
            )
            spans

        pure ()
    ]
  where
    getSelectedSpans sel Config {system = ir} = case ir of
      Nothing -> []
      Just (IRSystem _ procs sigs _) ->
        let s = findSignalSpan . IRString <$> sel <*> [sigs] & mconcat
            a = findProcessSpan . IRString <$> sel <*> [procs] & mconcat
         in s <> a
    sendModel :: LSP.LspT Config IO ()
    sendModel = do
      config@Config {file = f, clientId = c, system = s} <- LSP.getConfig
      case (f, c, s) of
        (Just curFile, Just curId, Just curSystem) ->
          LSP.sendNotification diagramAcceptMethod (setSynthesis curId)
            >> LSP.sendNotification diagramAcceptMethod (updateOptions curFile curId)
            >> LSP.sendNotification diagramAcceptMethod (requestBounds curFile curId curSystem)
        _ -> dualLogger <& ("does not have enough information to send diagram: " <> T.show config) `L.WithSeverity` L.Error
    recomputeModel :: LSP.LspT Config IO ()
    recomputeModel = do
      config <- LSP.getConfig
      out <- case config of
        Config {file = Just newfile} ->
          compileToModelMaybe newfile
        _ -> pure Nothing
      case out of
        Nothing -> pure ()
        Just _ ->
          LSP.setConfig config {system = out}
    compileToModelMaybe f = do
      config <- LSP.getConfig
      dualLogger <& ("Compiling: " <> T.show f) `L.WithSeverity` L.Debug
      result <- liftIO $ E.try $ compileToCoreWithForSyDePath (forSyDePkg config) f
      case result of
        Left e -> do
          dualLogger <& ("Failed to compile into core: " <> T.show (e :: E.SomeException)) `L.WithSeverity` L.Error
          pure $ system config
        Right (core, dflags) -> do
          s <- liftIO $ E.try $ E.evaluate $ translateCoreProgram dflags core
          case s of
            Left e -> do
              dualLogger <& ("Failed to compile into ForSyDe IR: " <> T.show (e :: E.ErrorCall)) `L.WithSeverity` L.Error
              pure $ system config
            Right (ir, _) -> pure $ Just ir
    findSignalSpan :: IRId -> [IRSignal] -> [IRSpan]
    findSignalSpan sig l = mapMaybe match l
      where
        match (IRSignal n _ _) = if sig == n then varToSpan n else Nothing
    findProcessSpan :: IRId -> [IRConstructor] -> [IRSpan]
    findProcessSpan proc l = mapMaybe match l
      where
        match = \case
          IRDelay n _ _ -> if proc == n then varToSpan n else Nothing
          IRActor n _ _ _ -> if proc == n then varToSpan n else Nothing
    getKey key = \case
      A.Object o -> o !? key
      _ -> Nothing
    getKind params =
      Just params
        >>= getKey "action"
        >>= getKey "kind"
        >>= \case
          A.String _a -> Just $ T.unpack _a
          _ -> Nothing
    shouldUpdate params =
      case getKind params of
        Just "requestModel" -> True
        Just "refreshDiagram" -> True
        _ -> False
    getSelected params =
      Just params
        >>= getKey "action"
        >>= getKey "selectedElementsIDs"
        >>= \case
          A.Array _a -> Just $ F.toList _a
          _ -> Nothing
        & maybeToList
        & mconcat
        & foldr
          ( \j l ->
              case j of
                A.String s -> s : l
                _ -> l
          )
          []
    getFilePathFromClient params =
      Just params
        >>= getKey "action"
        >>= getKey "options"
        >>= getKey "sourceUri"
        >>= \case
          A.String _a -> Just $ T.unpack $ snd $ T.splitAt 7 _a
          _ -> Nothing
    getClientId params =
      Just params
        >>= getKey "clientId"
        >>= \case
          A.String _a -> Just _a
          _ -> Nothing

logToText :: LSP.LspServerLog -> T.Text
logToText = T.show . pretty

formatOut :: L.WithSeverity T.Text -> String
formatOut (L.WithSeverity m s) =
  show s <> ": " <> T.unpack m

stderrLogger :: L.LogAction IO (L.WithSeverity T.Text)
stderrLogger = L.cmap formatOut L.logStringStderr

clientLogger :: L.LogAction (LSP.LspM Config) (L.WithSeverity T.Text)
clientLogger = LSP.defaultClientLogger

dualLogger :: L.LogAction (LSP.LspM Config) (L.WithSeverity T.Text)
dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

runServerC :: Handle -> Handle -> LSP.ServerDefinition Config -> IO Int
runServerC =
  LSP.runServerWithHandles
    (L.cmap (fmap logToText) stderrLogger)
    (L.cmap (fmap logToText) dualLogger)

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
run (Arguments comm (Host ip) (TCP p) i_f pkgPath) =
  case (i_f, comm) of
    (FromClient, CommTcp) ->
      runTCPServer (Just ip) p lsp
      where
        lsp s = do
          handle <- socketToHandle s ReadWriteMode
          void $ runServerC handle handle serverDef
    (FromClient, CommStdio) ->
      void $ runServerC stdin stdout serverDef
    (InputFile f, _) -> do
      (core, dflags) <- liftIO $ compileToCoreWithForSyDePath pkgPath f
      let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
      let graphMessage = requestBounds f "sprotty" forsydeIR
      BSL8.putStrLn $ AP.encodePretty graphMessage
  where
    serverDef =
      LSP.ServerDefinition
        { parseConfig = const $ const $ Right defaultConfig {forSyDePkg = pkgPath},
          onConfigChange = const $ pure (),
          defaultConfig = defaultConfig {forSyDePkg = pkgPath},
          configSection = "demo",
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = \_caps -> handlers,
          interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
          options = LSP.defaultOptions
        }

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
