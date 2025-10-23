module Main where

import Arguments
import CoreIR (compileToCore, prettyCoreProgram)
import CoreToForSyDeIR
import ForSyDeIR (prettyIRSystem)
import Options.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC

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
        - if --output-forsyde-ir, output should be in ForSyDe-IR format
        - if --output-procedural-ir, output should be in procedural IR format

    - Output file argument:
        - Use "-o" flag, outputs to desired file
        - Dump to stdout if unspecified

-}

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
generateDefaultFileName f OutputForSyDeIR = f ++ ".fir"
generateDefaultFileName f OutputForSyDeIRJSON = f ++ ".json"
generateDefaultFileName f OutputProceduralIR = f ++ ".pir"

-- Main function for running after arguments have been returned from main.
-- Need to pattern match based on the arguments used. They are matched on
-- manually since a lot of combinations are invalid.
-- You can't for intance start with procedural IR as input and go to Core
-- since the compiler doesn't go backwards.

run :: Arguments -> IO ()
-- "Normal run"
run (Arguments (InputFile _) _ OutputC) =
  putStrLn "To C"
run (Arguments (InputFile input_file) output_file OutputForSyDeIR) = do
  (core, dflags) <- compileToCore input_file
  let ir = translateCoreProgram dflags core
  write_output output_file OutputForSyDeIR (prettyIRSystem dflags ir)
run (Arguments (InputFile input_file) output_file OutputForSyDeIRJSON) = do
  (core, dflags) <- compileToCore input_file
  let ir = translateCoreProgram dflags core
  let ir_json = (BSC.unpack (encode ir))::String
  write_output output_file OutputForSyDeIRJSON ir_json
run (Arguments (InputFile _) _ OutputProceduralIR) =
  putStrLn "To Procedural IR"
-- What we have so far, take input file and write out core
run (Arguments (InputFile input_file) output_file OutputCore) = do
  (core, dflags) <- compileToCore input_file
  write_output output_file OutputCore (prettyCoreProgram dflags core)

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
