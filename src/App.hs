module App where
import Options.Applicative
    ( (<**>),
      Parser,
      ParserInfo,
      auto,
      fullDesc,
      help,
      info,
      long,
      option,
      progDesc,
      short,
      strOption,
      execParser,
      helper )

import Day1 ( program )
import Day2 ( program )
import Day3 ( program )
import Day4 ( program )
import Day5 ( program )
import Day6 ( program )

program :: IO ()
program = execParser parserInfo >>= program'

program' :: Args -> IO ()
program' (Args 1 f) = Day1.program f
program' (Args 2 f) = Day2.program f
program' (Args 3 f) = Day3.program f
program' (Args 4 f) = Day4.program f
program' (Args 5 f) = Day5.program f
program' (Args 6 f) = Day6.program f
program' _ = putStrLn "day not found"


data Args = Args {
   day :: Int,
   input :: FilePath
   } deriving (Eq,Show)

parser :: Parser Args
parser = Args <$> option auto
            ( long "day"
           <> short 'd'
           <> help "day" ) <*> strOption ( long "filename"
           <> short 'f'
           <> help "filename" )

parserInfo :: ParserInfo Args
parserInfo = info (parser <**> helper)
  ( fullDesc
  <> progDesc "advent of code 2022")
