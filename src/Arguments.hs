module Arguments where

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

data Arguments = Arguments
  { -- Files
    input :: Input,
    output :: Output,
    -- Formats
    output_format :: OutputFormat
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

-- Top-level. All of these 4 flags are optional, but one of them
-- need to be active. Same as inputFormatTop, where the function
-- for FormatCore has InputC as default but InputCore if flag is
-- turned on
outputFormatTop :: Parser OutputFormat
outputFormatTop =
  outputFormatC
    <|> outputFormatForSyDeIR
    <|> outputFormatForSyDeIRJSON
    <|> outputFormatProceduralIR

-- Temporary implementation. Sets output to core as default functionality,
outputFormatC :: Parser OutputFormat
outputFormatC =
  flag
    OutputCore
    OutputC
    ( long "output-c"
        <> help "Output file in C"
    )

-- Temporarily commented out, this is how it will work in the end where the
-- Core output format function has C as default if flag is unspecified, and Core
-- if set.
{-
outputFormatCore :: Parser OutputFormat
outputFormatCore =
  flag
    OutputC
    OutputCore
    ( long "output-core"
        <> help "Output file in Core"
    )
-}

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

-- Top level argument parsing function, takes 4 flags.
arguments :: Parser Arguments
arguments =
  Arguments
    <$> inputFile
    <*> outputFileTop
    <*> outputFormatTop
