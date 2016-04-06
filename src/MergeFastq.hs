{-#LANGUAGE OverloadedStrings#-}

{-
Project name: MergeFastq 
Min Zhang
Date: April 5, 2016
Version: v0.1.0
README: Merge Paired-end fastq file to one file (stitch reads together)

NOTE: Currently in v0.1.0, it assumes that two fastq files are sorted in the same order; non-paired reads are thrown away

-}

module MergeFastq
  (mergePairedEndFq)
where

import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import qualified Data.Maybe as Maybe
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
    { name_fq      :: !T.Text
    , namelabel_fq :: !T.Text
    , seq_fq       :: !T.Text
    , qual_fq      :: !T.Text
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

readFq = AP.many1' $ fastqParser

parseFq fq = AP.maybeResult $ AP.feed (AP.parse readFq fq) (T.pack "")



mergeFastq [] b = b
mergeFastq a [] = a
mergeFastq (x:xs) (y:ys) 
  | name_fq x == name_fq y = Fastq (name_fq x)
                                   (namelabel_fq x)
                                   (T.append (T.take 20 $ seq_fq x) (seq_fq y))     -- for dropseq only, needs to fix later
                                   (T.append (T.take 20 $ qual_fq x) (qual_fq y)) : 
                             mergeFastq xs ys
  | name_fq x < name_fq y  = mergeFastq xs (y:ys)
  | otherwise              = mergeFastq (x:xs) ys

showFastq fq = T.concat [ name_fq fq
                        , namelabel_fq fq, "\n"
                        , seq_fq fq, "\n"
                        , "+", "\n"
                        , qual_fq fq, "\n"]

example = do
  f1 <- TextIO.readFile "../data/s1.1000.fastq"
  return $ parseFq f1

mergePairedEndFq input1 input2 = do
  f1 <- parseFq <$> TextIO.readFile input1
  f2 <- parseFq <$> TextIO.readFile input2
  let res = liftA2 mergeFastq f1 f2
  if res == Nothing 
  then TextIO.putStrLn "nothing"
  else TextIO.putStr $ T.concat $ map showFastq (Maybe.fromJust res)

