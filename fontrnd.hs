import Data.List
import Fontrnd.Parser
import Fontrnd.Generate
import System.Environment
import System.Random

main = do
    glyphs <- mapM glyphs =<< getArgs
    rnd <- newStdGen

    putStrLn . generate rnd $ foldr1 intersect glyphs
