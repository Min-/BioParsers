{-#LANGUAGE OverloadedStrings#-}

{-
Project name: MergeFastq 
Min Zhang
Date: April 5, 2016
Version: v0.1.0
README: Merge Paired-end fastq file to one file (stitch reads together)

NOTE: Currently in v0.1.0, it assumes that two fastq files are sorted in the same order; non-paired reads are thrown away

-}

module ParseGtf
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as TextIO (putStrLn)

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.ByteString.Internal as Bi


data Gtf = Gtf
    { chr_gtf     :: !B.ByteString
    , source_gtf  :: !B.ByteString
    , feature_gtf :: !B.ByteString
    , start_gtf   :: !Int
    , end_gtf     :: !Int
    , score_gtf   :: !B.ByteString
    , strand_gtf  :: !B.ByteString
    , frame_gtf   :: !B.ByteString
    , gene_name_gtf :: !B.ByteString
    } deriving (Show, Read)

instance Eq Gtf where
    (Gtf chr1 _ _ start1 end1 _  strand1 _ _) == (Gtf chr2 _ _ start2 end2 _ strand2 _ _)
      | and [chr1 == chr2, start1 <= start2, start2 <= end1, end1 <= end2, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start2 <= start1, start1 <= end2, end2 <= end1, start1 < end1, start2 < end2] = True
      | otherwise = False


instance Ord Gtf where
    (Gtf chr1 _ _ start1 end1 _ strand1 _ _) `compare` (Gtf chr2 _ _ start2 end2 _ strand2 _ _)  
       | and [chr1 == chr2, end1 < start2, start1 < end1, start2 < end2] = LT
       | and [chr1 == chr2, end2 < start1, start1 < end1, start2 < end2] = GT
       | otherwise = compare chr1 chr2
         

--fastqParser :: Bl.ByteString -> AP.Parser Fastq
--fastqParser = do
--  return
nl = Bi.c2w '\n'
ps = Bi.c2w '+'
pd = Bi.c2w '#'
tb = Bi.c2w '\t'
sm = Bi.c2w ';'
qt = Bi.c2w '\"'

bsToInt :: B.ByteString -> Int
bsToInt = read . init . drop 1 . show

isEndOfLine w = w == 13 || w == 10

isDigit w = w - 48 <= 9

isQt w = w == qt

example = do
    input <- B.take 10000 <$> B.readFile "/Users/minzhang/Documents/private_git/BioParsers/data/gencode.vM8.annotation.exon.gtf"
    let xs = Maybe.fromJust $ parseGtf input
    print $ map length $ sortGtf xs
    let ys = map mergeGtfList $ sortGtf xs
    print $ length $ head ys 
    print $ length $ last ys

parseGtf g = AP.maybeResult $ AP.feed (AP.parse (AP.many1' gtfLineParser) g) B.empty
  
gtfLineParser = do
    chr <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    source <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    feature <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    start <- AP.takeWhile isDigit
    _ <- AP.word8 tb
    end <- AP.takeWhile isDigit
    _ <- AP.word8 tb
    score <- B.singleton <$> AP.anyWord8
    _ <- AP.word8 tb
    strand <- B.singleton <$> AP.anyWord8
    _ <- AP.word8 tb
    frame <- B.singleton <$> AP.anyWord8
    _ <- AP.word8 tb
    _ <- AP.manyTill' AP8.anyChar (AP.string ("gene_name \""))
    name <- AP.takeTill isQt 
    _ <- AP.manyTill' AP8.anyChar (AP.string "\n")
    let startInt = bsToInt start
    let endInt = bsToInt end
    return $ Gtf chr source feature startInt endInt score strand frame name

sortGtf :: [Gtf] -> [[Gtf]]
sortGtf = map L.sort . L.groupBy ((==) `on` strand_gtf) . L.sortBy (comparing strand_gtf) 

mergeGtf :: Gtf -> Gtf -> Gtf
mergeGtf g1 g2 = Gtf (chr_gtf g1)
                     (source_gtf g1) 
                     (feature_gtf g1)
                     (min (start_gtf g1) (start_gtf g2))
                     (max (end_gtf g1) (end_gtf g2))
                     (score_gtf g1)
                     (strand_gtf g1)
                     (frame_gtf g1)
                     (if n1 == n2 then n1 else B.append n1 n2)
                         where n1 = gene_name_gtf g1
                               n2 = gene_name_gtf g2

mergeGtfList :: [Gtf] -> [Gtf]
mergeGtfList [] = []
mergeGtfList [x] = [x]
mergeGtfList [x, y] = if x == y then [mergeGtf x y] else [x, y]
mergeGtfList xs
  | a == b = mergeGtfList (mergeGtf a b : drop 2 xs)
  | a /= b = a : mergeGtfList (drop 1 xs)
    where a = head xs
          b = head $ drop 1 xs
