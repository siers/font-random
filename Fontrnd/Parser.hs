module Fontrnd.Parser where

import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Numeric
import System.Process
import Text.Regex.Base
import Text.Regex.TDFA

import Debug.Trace
--t = show >>= trace

naiveHexRead = fst . head . readHex

-- Expects input of form:
-- 	charset:
-- 	0000: 00000000 ffffffff ffffffff 7fffffff 00000000 ffffffff ffffffff ffffffff
-- 	0001: ffffffff ffffffff ffffffff ffffffff 00048000 00018003 1fffe000 fc000000
parseCharset :: [String] -> [(Integer, String)]
parseCharset =
    map (\(lineNr:bits) -> (naiveHexRead lineNr, filter (/= ' ') (concat bits))) .
    groups "([0-9a-f]{4}):(( [0-9a-f]{8}){8})" .
    concat

    where
        groups :: String -> String -> [[String]]
        groups r s = map tail $ ((s =~ r) :: [[String]])

extractCharset :: [String] -> [String]
extractCharset =
    takeWhile (matches "^[\t ]+[0-9a-f]+:") . tail .
    dropWhile (not . matches "^[\t ]+charset: *$")

    where
        matches :: String -> String -> Bool
        matches r s = isJust ((s =~~ r) :: Maybe String)

listAvailable :: (Integer, String) -> [Integer]
listAvailable (prefix, list) = parseOctet =<< zip [0..255] list
    where
        parseOctet :: (Integer, Char) -> [Integer]
        parseOctet (count, octet) = map (+ (16 * count + 256* prefix)) $ listIfBitSet octet =<< [0..3]

        listIfBitSet :: Char -> Integer -> [Integer]
        listIfBitSet octet bit = if andbit /= 0 then [bit] else []
            where andbit = (2 ^ bit) .&. (naiveHexRead [octet])

glyphs :: String -> IO [Integer]
glyphs name = extract <$> readCreateProcess (shell ("fc-query " ++ name)) ""
    where
        extract = (listAvailable =<<) . parseCharset . extractCharset . lines
