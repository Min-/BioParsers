{-#LANGUAGE OverloadedStrings#-}

{-
Project name: MergeFastq 
Min Zhang
Date: April 5, 2016
Version: v0.1.0
README: Merge Paired-end fastq file to one file (stitch reads together)

NOTE: Currently in v0.1.0, it assumes that two fastq files are sorted in the same order; non-paired reads are thrown away

-}

module ParsingUtils
where

import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.Maybe as Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 (putStrLn, pack)
import qualified Data.ByteString.Internal as Bi
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8


nl = Bi.c2w '\n'
ps = Bi.c2w '+'
pd = Bi.c2w '#'
tb = Bi.c2w '\t'
sm = Bi.c2w ';'
qt = Bi.c2w '\"'

toTuple x y = (x, y)

bsToInt :: B.ByteString -> Int
bsToInt = read . init . drop 1 . show

intToBs :: Int -> Bi.ByteString
intToBs = B8.pack . show 


isEndOfLine w = w == 13 || w == 10

isDigit w = w - 48 <= 9

isQt w = w == qt

