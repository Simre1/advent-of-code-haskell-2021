module Solution where 

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Solution = StringSolution (String -> String) | TextSolution (T.Text -> String)

makeStringSolution :: Show a => (String -> a) -> Solution
makeStringSolution f = StringSolution $ show . f

makeTextSolution :: Show a => (T.Text -> a) -> Solution
makeTextSolution f = TextSolution $ show . f

runSolution :: FilePath -> Solution -> IO String
runSolution filepath = \case
  StringSolution f -> f <$> readFile filepath
  TextSolution f -> f <$> T.readFile filepath

execSolution :: String -> Solution -> String
execSolution s = \case
  StringSolution f -> f s  
  TextSolution f -> f (T.pack s)


    


