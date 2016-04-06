{-#LANGUAGE OverloadedStrings#-}

{-
Project name: MergeFastq 
Min Zhang
Date: April 5, 2016
Version: v0.1.0
README: Merge Paired-end fastq file to one file (stitch reads together)
-}
module MergeFastq
where

import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import qualified Data.Attoparsec.Text as AP

data Fastq = Fastq
    { name_fq      :: T.Text
    , namelabel_fq :: T.Text
    , seq_fq       :: T.Text
    , qual_fq      :: T.Text
    } deriving (Show, Read, Eq)

--fastqParser :: Bl.ByteString -> AP.Parser Fastq
--fastqParser = do
--  return

fastqParser = do
  name <- AP.takeTill ((==) ' ')
  namelabel <- AP.takeTill AP.isEndOfLine
  _ <- AP.endOfLine
  seq <- AP.takeTill AP.isEndOfLine
  _ <- AP.endOfLine
  _ <- AP.char '+'
  _ <- AP.endOfLine
  qual <- AP.takeTill AP.isEndOfLine
  _ <- AP.endOfLine
  return $ Fastq name namelabel seq qual

readFastq = AP.many1' $ fastqParser

parseFastq fq = AP.maybeResult $ AP.feed (AP.parse readFastq fq) (T.pack "")



mergeFastq [] b = b
mergeFastq a [] = a
mergeFastq (x:xs) (y:ys) 
  | name_fq x == name_fq y = Fastq (name_fq x)
                                   (namelabel_fq x)
                                   (T.append (seq_fq x) (seq_fq y))
                                   (T.append (qual_fq x) (qual_fq y)) : 
                             mergeFastq xs ys
  | name_fq x < name_fq y  = mergeFastq xs (y:ys)
  | otherwise              = mergeFastq (x:xs) ys

example = do
  f1 <- TextIO.readFile "../data/s1.1000.fastq"
  return $ parseFastq f1

main = do
  f1 <- parseFastq <$> TextIO.readFile "../data/s1.1000.fastq"
  f2 <- parseFastq <$> TextIO.readFile "../data/s2.1000.fastq"
  return $ liftA2 mergeFastq f1 f2
