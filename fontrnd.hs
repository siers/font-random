import Data.List (union, intersect)
import Data.Map (Map, keys, fromList, lookup)
import Data.Maybe
import Fontrnd.Generate
import Fontrnd.Parser
import System.Environment
import System.Random
import Prelude hiding (lookup)

choices :: Map String ([Integer] -> [Integer] -> [Integer])
choices = fromList
    [ ("union", union)
    , ("or", union)
    , ("and", intersect)
    , ("intersect", intersect)
    ]

usage :: String
usage =
    "usage: ./fontrnd <algos> <fonts>+\n" ++
    "  possible algos: " ++ show (keys choices)

main = do
    (algo:fonts) <- getArgs

    glyphs <- mapM glyphs fonts
    rnd <- newStdGen

    let zipper = algo `lookup` choices

    maybe
        (putStrLn usage)
        (putStrLn . generate rnd . flip foldr1 glyphs)
        zipper
