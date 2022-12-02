module App where
import Options.Applicative

import Day1
import Day2

program :: IO ()
program = execParser parserInfo >>= program'

program' :: Args -> IO ()
program' (Args 1 f) = Day1.program f
program' (Args 2 f) = Day2.program f
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
