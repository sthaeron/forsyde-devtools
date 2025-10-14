module Main where

import Arguments
import CoreIR (compileToCore, prettyCoreBindList)
import GHC
import GHC.Core (CoreProgram)
import GHC.Data.EnumSet (fromList)
import GHC.Driver.Monad
import GHC.Driver.Ppr
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut)
import GHC.Paths
import GHC.Utils.Outputable hiding ((<>))
import Options.Applicative

{-

    Specification

    Compiler steps:

    ForSyDe model ->
    Core ->
    ForSyDe IR ->
    Procedural IR ->
    C

    - First we need a source file.
    - Input mode:
        - This should be the format for the input file, if not otherwise
          specified, this should be a ForSyDe model (.hs) file

    - Output mode:
        - This should be the format of the output file, if not otherwise
          specified, this should be C code
        - if --output-core, output should be in core format
        - if --output-ir-forsyde, output should be in ForSyDe-IR format
        - if --output-ir-procedural, output should be in procedural IR format

    - Output file argument:
        - Use "-o" flag, outputs to desired file
        - Dump to stdout if unspecified

-}

-- toCore input_file =
--   defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
--     runGhc (Just libdir) $ do
--       dflags <- getSessionDynFlags
--       setSessionDynFlags
--         dflags
--           { ghcLink = LinkInMemory,
--             ghcMode = CompManager,
--             generalFlags =
--               fromList
--                 [ Opt_SuppressTicks,
--                   Opt_SuppressCoercions,
--                   Opt_SuppressCoercionTypes,
--                   Opt_SuppressVarKinds,
--                   Opt_SuppressModulePrefixes,
--                   Opt_SuppressTypeApplications,
--                   Opt_SuppressIdInfo,
--                   Opt_SuppressUnfoldings,
--                   Opt_SuppressTypeSignatures,
--                   Opt_SuppressUniques,
--                   Opt_SuppressStgExts,
--                   Opt_SuppressStgReps,
--                   Opt_SuppressTimestamps,
--                   Opt_SuppressCoreSizes
--                 ]
--           }
--       compileToCore input_file

write_output :: Output -> OutputFormat -> [Char] -> IO ()
write_output StdOut _ s =
  putStrLn s
write_output (OutputFile (output_file, WithFileExtension)) _ s =
  writeFile output_file s
write_output (OutputFile (output_file, NoFileExtension)) extension s =
  writeFile (generateDefaultFileName output_file extension) s

-- Function for generating default file names
generateDefaultFileName :: FilePath -> OutputFormat -> FilePath
generateDefaultFileName f OutputC = f ++ ".c"
generateDefaultFileName f OutputCore = f ++ ".hcr"
generateDefaultFileName f OutputIRForSyDe = f ++ ".irf"
generateDefaultFileName f OutputIRProcedural = f ++ ".irp"

-- Main function for running after arguments have been returned from main.
-- Need to pattern match based on the arguments used. They are matched on
-- manually since a lot of combinations are invalid.
-- You can't for intance start with procedural IR as input and go to Core
-- since the compiler doesn't go backwards.

run :: Arguments -> IO ()
-- "Normal run"
run (Arguments (InputFile input_file) output_file OutputC) =
  putStrLn "To C"
run (Arguments (InputFile input_file) output_file OutputIRForSyDe) =
  putStrLn "To ForSyDe IR"
run (Arguments (InputFile input_file) output_file OutputIRProcedural) =
  putStrLn "To Procedural IR"
-- What we have so far, take input file and write out core
run (Arguments (InputFile input_file) outputFile OutputCore) = do
  -- core_output <- toCore input_file
  -- dflags <- runGhc (Just libdir) $ getSessionDynFlags
  core <- compileToCore input_file
  write_output outputFile OutputCore (prettyCoreBindList core)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (arguments <**> helper)
        ( fullDesc
            <> progDesc "Compile a ForSyDe model"
            <> header "ForSyDe DevTools"
        )
