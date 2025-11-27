module Main where

import ArgumentsMain
import CoreIR (prettyCoreProgram)
import CoreIRToForSyDeIR (translateCoreProgram)
import ForSyDeIR (prettyIRJSON, prettyIRSystem)
import ForSyDeIRToProceduralIR (translateIRSystemToProgram)
import Options.Applicative
import ProceduralIR (prettyProgram)
import ProceduralIRToC (formatWithClang, translateProgram)
import SDFSchedule (computeScheduleAndBuffers)
import Utilities (compileToCore)

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
        - If --output-c, output file should be in C
        - If --output-core, output should be in core format
        - If --output-forsyde-ir, output should be in ForSyDe-IR format
        - If --output-forsyde-ir-json, output should be in ForSyDe-IR-JSON
          format
        - If --output-procedural-ir, output should be in procedural IR format

    - Output file argument:
        - Use "-o" flag, outputs to desired file
        - Dump to a file "main.xxx" where file format depends on output format
          flag if unspecified.
        - If "--stdout", dump to stdout.

-}

write_output :: Output -> OutputFormat -> [Char] -> IO ()
write_output StdOut _ s =
  putStr s
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
run (Arguments (InputFile input_file) output_file OutputC) = do
  (core, dflags) <- compileToCore input_file
  let (forsydeIR, lookupSignals) = translateCoreProgram dflags core
  let (schedule, buffers, delayBuffers) = computeScheduleAndBuffers forsydeIR
  let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals forsydeIR
  let c = translateProgram proceduralIR True
  cFormated <- formatWithClang c
  write_output output_file OutputC cFormated
run (Arguments (InputFile input_file) output_file OutputForSyDeIR) = do
  (core, dflags) <- compileToCore input_file
  let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
  write_output output_file OutputForSyDeIR (prettyIRSystem dflags forsydeIR)
run (Arguments (InputFile input_file) output_file OutputForSyDeIRJSON) = do
  (core, dflags) <- compileToCore input_file
  let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
  let ir_json = prettyIRJSON forsydeIR
  write_output output_file OutputForSyDeIRJSON ir_json
run (Arguments (InputFile input_file) output_file OutputProceduralIR) = do
  (core, dflags) <- compileToCore input_file
  let (forsydeIR, lookupSignals) = translateCoreProgram dflags core
  let (schedule, buffers, delayBuffers) = computeScheduleAndBuffers forsydeIR
  let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals forsydeIR
  write_output output_file OutputProceduralIR (prettyProgram proceduralIR)
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
