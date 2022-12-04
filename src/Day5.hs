module Day5 where
import qualified Data.Text as T
import qualified Data.Text.IO as T

program :: FilePath -> IO ()
program = (=<<) print . fmap pureProgram . T.readFile

data Report = Report deriving (Eq, Show)

pureProgram :: T.Text -> Report
pureProgram = const Report
