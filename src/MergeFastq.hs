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
import qualified Data.ByteString as T
import qualified Data.ByteString.Char8 as TextIO (putStrLn)

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Internal as Bi

data Fastq = Fastq
    { name_fq      :: !T.ByteString
    , namelabel_fq :: !T.ByteString 
    , seq_fq       :: !T.ByteString
    , qual_fq      :: !T.ByteString
    } deriving (Show, Read, Eq)

--fastqParser :: Bl.ByteString -> AP.Parser Fastq
--fastqParser = do
--  return
nl = Bi.c2w '\n'
ps = Bi.c2w '+'

isEndOfLine w = w == 13 || w == 10

fastqParser = do
  name <- AP.takeTill Bi.isSpaceWord8
  namelabel <- AP.takeTill isEndOfLine
  _ <- AP.word8 nl
  seq <- AP.takeTill isEndOfLine
  _ <- AP.word8 nl
  _ <- AP.word8 ps
  _ <- AP.word8 nl
  qual <- AP.takeTill isEndOfLine
  _ <- AP.word8 nl
  return $ Fastq name namelabel seq qual

readFq = AP.many1' $ fastqParser

parseFq fq = AP.maybeResult $ AP.feed (AP.parse readFq fq) T.empty



mergeFastq [] y = []
mergeFastq x [] = []
mergeFastq (x:xs) (y:ys) a b
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
  f1 <- T.readFile "../data/s1.1000.fastq"
  return $ parseFq f1

mergePairedEndFq input1 input2 = do
  f1 <- parseFq <$> T.readFile input1
  f2 <- parseFq <$> T.readFile input2
  let res = liftA2 mergeFastq f1 f2
  if res == Nothing 
  then TextIO.putStrLn "nothing"
  else T.putStr $ T.concat $ map showFastq (Maybe.fromJust res)

