module Fontrnd.Generate
  ( generate
  ) where

import Data.Char
import Data.List
import Data.List.Split
import System.Random

import Debug.Trace
--t = show >>= trace

width, height :: Int
width  = 80
height = 20

badCategory = flip elem bad . generalCategory
  where
    bad =
      [ NonSpacingMark
      , SpacingCombiningMark
      , EnclosingMark
      , Space
      , LineSeparator
      , Control
      ]

printStats :: [Char] -> [(GeneralCategory, Int)]
printStats =
    fmap (flip ((,) . head) =<< length)
    . group . sort
    . fmap generalCategory

generate :: StdGen -> [Integer] -> [String]
generate rnd list =
    chunksOf width
    . filter (\c -> isPrint c && not (badCategory c))
    -- . (concatMap ((++"\n") . show) . printStats >>= trace)
    . fmap chr
    $ take (width * height) indexes
  where
    indexes = randomRs (0, (length list) - 1) rnd
