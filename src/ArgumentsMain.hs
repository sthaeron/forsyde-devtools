module ArgumentsMain where

import Options.Applicative

-- Which file to use for input
data Input
  = InputFile FilePath

-- Output, can either be a file or stdout
data Output
  = OutputFile (FilePath, HasExtension)
  | StdOut

data HasExtension
  = WithFileExtension
  | NoFileExtension

-- Stubbed out this type for now, will probably not implement
-- reading separate input formats
data InputFormat
  = InputForSyDe
  | InputCore
  | InputForSyDeIR
  | InputForSyDeIRJSON
  | InputProceduralIR

data OutputFormat
  = OutputC
  | OutputCore
  | OutputForSyDeIR
  | OutputForSyDeIRJSON
  | OutputProceduralIR
  | OutputSchedule

data Target
  = PC
  | PICO2

data InputType
  = StdIn
  | Predefined

data Runs
  = Limited Int
  | Perpetual

data Arguments = Arguments
  { -- Files
    input :: Input,
    output :: Output,
    -- Formats
    output_format :: OutputFormat,
    -- Target
    target :: Target,
    -- Input Format (whether to get SDF Input from scanf or predetermined file)
    iType :: InputType,
    runs :: Runs,
    -- Path to the package DB containing ForSyDe Shallow
    forSyDePath :: Maybe FilePath
  }

-- Handle file input, always need to define a file
inputFile :: Parser Input
inputFile =
  InputFile
    <$> strArgument
      ( metavar "INPUT"
          <> help "Input filename"
      )

outputFileTop :: Parser Output
outputFileTop =
  outputFile
    <|> outputFileStdOut

-- Function to create a filepath with file extension
fileName :: ReadM (FilePath, HasExtension)
fileName = str >>= returnFile
  where
    returnFile s = pure (s, WithFileExtension)

-- Output to file optionally, otherwise stdout by default
outputFile :: Parser Output
outputFile =
  option
    (OutputFile <$> fileName)
    ( short 'o'
        <> long "output"
        <> metavar "OUTPUT"
        <> value (OutputFile ("main", NoFileExtension))
        <> help "Output filename"
    )

outputFileStdOut :: Parser Output
outputFileStdOut =
  flag'
    StdOut
    ( long "stdout"
        <> help "Print output to stdout"
    )

-- Top-level. All of these 4 flags are optional, but one of them
-- need to be active. How does that work? inputFormatCore has a
-- default value for InputForSyde if no flag is provided
inputFormatTop :: Parser InputFormat
inputFormatTop =
  inputFormatCore
    <|> inputFormatForSyDeIR
    <|> inputFormatProceduralIR

inputFormatCore :: Parser InputFormat
inputFormatCore =
  flag
    InputForSyDe
    InputCore
    ( long "input-core"
        <> help "Input file in ForSyDe"
    )

inputFormatForSyDeIR :: Parser InputFormat
inputFormatForSyDeIR =
  flag'
    InputForSyDeIR
    ( long "input-forsyde-ir"
        <> help "Input file in ForSyDe-IR"
    )

inputFormatProceduralIR :: Parser InputFormat
inputFormatProceduralIR =
  flag'
    InputProceduralIR
    ( long "input-procedural-ir"
        <> help "Input file in Procedural-IR"
    )

-- Top-level. All of these flags are optional, but one of them
-- need to be active.
outputFormatTop :: Parser OutputFormat
outputFormatTop =
  outputFormatC
    <|> outputFormatCore
    <|> outputFormatForSyDeIR
    <|> outputFormatForSyDeIRJSON
    <|> outputFormatProceduralIR
    <|> outputFormatSchedule

outputFormatC :: Parser OutputFormat
outputFormatC =
  flag
    OutputC
    OutputC
    ( long "output-c"
        <> help "Output file in C"
    )

outputFormatCore :: Parser OutputFormat
outputFormatCore =
  flag'
    OutputCore
    ( long "output-core"
        <> help "Output file in Core (default)"
    )

outputFormatForSyDeIR :: Parser OutputFormat
outputFormatForSyDeIR =
  flag'
    OutputForSyDeIR
    ( long "output-forsyde-ir"
        <> help "Output file in ForSyDe-IR"
    )

outputFormatForSyDeIRJSON :: Parser OutputFormat
outputFormatForSyDeIRJSON =
  flag'
    OutputForSyDeIRJSON
    ( long "output-forsyde-ir-json"
        <> help "Output file in ForSyDe-IR-JSON"
    )

outputFormatProceduralIR :: Parser OutputFormat
outputFormatProceduralIR =
  flag'
    OutputProceduralIR
    ( long "output-procedural-ir"
        <> help "Output file in Procedural-IR"
    )

outputFormatSchedule :: Parser OutputFormat
outputFormatSchedule =
  flag'
    OutputSchedule
    ( long "output-schedule"
        <> help "Output the SDF schedule to file instead of code"
    )

targetName :: ReadM Target
targetName = str >>= returnTarget
  where
    returnTarget s = case s of
      "PC" -> pure (PC)
      "PICO2" -> pure (PICO2)
      t -> error ("Unsupported target: " ++ t)

-- Parse target platform. Uses pattern matching to handle raw strings which
-- means it is used  like "--target={target}" as opposed to having --target-PC,
-- target-PICO2, etc
targetTop :: Parser Target
targetTop =
  option
    (targetName)
    ( short 't'
        <> long "target"
        <> metavar "TARGET"
        <> value PC
        <> help "Target platform for C code. (PC default, PC and PICO2 supported)"
    )

-- Parse InputType. Implementation is similar to `target` option
ioName :: ReadM InputType
ioName = str >>= returnIO
  where
    returnIO s = case s of
      "stdin" -> pure (StdIn)
      "predefined" -> pure (Predefined)
      t -> error ("Unsupported input source: " ++ t)

-- Parse target platform. Uses pattern matching to handle raw strings which
-- means it is used  like "--target={target}" as opposed to having --target-PC,
-- target-PICO2, etc
ioTop :: Parser InputType
ioTop =
  option
    (ioName)
    ( long "input-type"
        <> metavar "INPUTTYPE"
        <> value StdIn
        <> help "Source of input tokens for the SDF models in the C code. (stdin default, stdin and predefined supported)"
    )

runsParse :: ReadM Runs
runsParse = str >>= returnRuns
  where
    returnRuns s = case s of
      "inf" -> pure (Perpetual)
      x -> pure (Limited (read x))

runsTop :: Parser Runs
runsTop =
  option
    (runsParse)
    ( long "runs"
        <> metavar "RUNS"
        <> value (Limited 1)
        <> help "How many times to loop input data, when input mode is set to 'predefined'. 1 By default, pass an integer for a limited number or 'inf' to run the program perpetually"
    )

-- Path to the package DB containing ForSyDe Shallow
forSyDePathOption :: Parser (Maybe FilePath)
forSyDePathOption =
  option
    (str >>= \s -> pure $ Just s)
    ( long "forsyde-pkgpath"
        <> value Nothing
        <> help "The path to the ForSyDe Shallow package (unspecified by default)"
    )

-- Top level argument parsing function, takes 4 flags.
arguments :: Parser Arguments
arguments =
  Arguments
    <$> inputFile
    <*> outputFileTop
    <*> outputFormatTop
    <*> targetTop
    <*> ioTop
    <*> runsTop
    <*> forSyDePathOption
