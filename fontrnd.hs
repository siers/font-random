import Data.List
import Fontrnd.Parser
import System.Environment

main = do
    glyphs <- mapM glyphs =<< getArgs

    print . length $ foldr1 intersect glyphs
