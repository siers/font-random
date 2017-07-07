module Fontrnd.Generate where

import System.Random
import Data.Char

generate :: StdGen -> [Integer] -> String
generate rnd list = filter isPrint . map chr $ take 320 indexes
    where
        indexes = randomRs (0, (length list) - 1) rnd
