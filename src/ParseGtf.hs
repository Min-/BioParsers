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

import System.IO.Posix.MMap (unsafeMMapFile)

import ParsingUtils

-- TODO: need to think about what if we want to add more fields, is this structure general enough?
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
    (Gtf chr1 _ _ start1 end1 _ strand1 _ _) == (Gtf chr2 _ _ start2 end2 _ strand2 _ _)
       | and [chr1 == chr2, start1 == start2, end1 == end2, strand1 == strand2] = True
       | otherwise = False

instance Ord Gtf where
    (Gtf chr1 _ _ start1 end1 _ strand1 _ _) `compare` (Gtf chr2 _ _ start2 end2 _ strand2 _ _)  
       | chr1 < chr2 = LT
       | chr1 > chr2 = GT
       | and [chr1 == chr2, end1 < start2, start1 < end1, start2 < end2] = LT
       | and [chr1 == chr2, end2 < start1, start1 < end1, start2 < end2] = GT
       | chr1 == chr2 = compare (start1 + end1) (end1 + end2) 
       | otherwise = compare start1 start2

-- 
data Bed = Bed 
           { chr_bed   :: !B.ByteString
           , start_bed :: !Int
           , end_bed   :: !Int
           , name_bed  :: !B.ByteString
           , score_bed :: !B.ByteString
           , strand_bed  :: !B.ByteString}
           deriving (Read, Show, Eq)

--
data Interval = Interval
           { chr_interval   :: !B.ByteString
           , start_interval :: !Int
           , end_interval   :: !Int
           } deriving (Read, Show)

instance Ord Interval where
    (Interval chr1 start1 end1) `compare` (Interval chr2 start2 end2)
       | and [chr1 == chr2, start1 < start2, start1 < end1, start2 < end2] = LT
       | and [chr1 == chr2, start2 < start1, start1 < end1, start2 < end2] = GT
       | otherwise = compare chr1 chr2

instance Eq Interval where
    (Interval chr1 start1 end1) == (Interval chr2 start2 end2)
      | and [chr1 == chr2, start1 <= start2, start2 <= end1, end1 <= end2, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start2 <= start1, start1 <= end2, end2 <= end1, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start1 <= start2, end1 >= end2, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start2 <= start1, end2 >= end1, start1 < end1, start2 < end2] = True
      | otherwise = False

overlapGtf :: Gtf -> Gtf -> Bool
overlapGtf  (Gtf chr1 _ _ start1 end1 _ strand1 _ _) (Gtf chr2 _ _ start2 end2 _ strand2 _ _)
      | and [chr1 == chr2, start1 <= start2, start2 <= end1, end1 <= end2, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start2 <= start1, start1 <= end2, end2 <= end1, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start1 <= start2, end1 >= end2, start1 < end1, start2 < end2] = True
      | and [chr1 == chr2, start2 <= start1, end2 >= end1, start1 < end1, start2 < end2] = True
      | otherwise = False

gtfToInterval :: Gtf -> Interval
gtfToInterval (Gtf chr _ _ start end _ strand _ _) = Interval chr start end

bedToInterval :: Bed -> Interval
bedToInterval (Bed chr start end _ _ _) = Interval chr start end

readGtf g = AP.maybeResult $ AP.feed (AP.parse (AP.many1' gtfLineParser) g) B.empty
  
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

-- sort by strand
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
                     (if n1 == n2 then n1 else mergeName n1 n2)
                         where n1 = gene_name_gtf g1
                               n2 = gene_name_gtf g2
                               mergeName x1 x2 = B.intercalate "_" $ Set.toList $ Set.fromList $ x2 : B.split (Bi.c2w '_') x1

checkIfGtfSorted :: [Gtf] -> (Bool, Int)
checkIfGtfSorted [] = (True, 0)
checkIfGtfSorted [x] = (True, 0)
checkIfGtfSorted gs = S.headDef (True, 0) $ L.dropWhile (\x -> (fst x) == False) $ L.scanl' moveComparisonGtf (True, 0) $ map compareGtf $ zip gs (drop 1 gs)
     where compareGtf (g1, g2) = (g1 <= g2, 1)
           moveComparisonGtf (b1, n1) (b2, n2) = (b1 && b2, n1 + n2)

example1 = do
    input <- unsafeMMapFile "/Users/minzhang/Documents/private_git/BioParsers/data/gencode.vM8.annotation.exon.10k.gtf"
    let xs = Maybe.fromJust $ readGtf input
    let ys = map mergeGtfList $ sortGtf xs
    return xs
--    print $ length $ head ys 
--    print $ last ys
--    let z = B.concat $ map showGtf $ xs
--    let res = Maybe.fromJust $ readGtf z
--    print res


mergeGtfList :: [Gtf] -> [Gtf]
mergeGtfList [] = []
mergeGtfList [x] = [x]
mergeGtfList [x, y] = if overlapGtf x y then [mergeGtf x y] else [x, y]
mergeGtfList xs
  | overlapGtf a b = mergeGtfList (mergeGtf a b : drop 2 xs)
  | otherwise = a : mergeGtfList (drop 1 xs)
    where a = head xs
          b = head $ drop 1 xs

mergeGtfList2 :: [Gtf] -> [Gtf]
mergeGtfList2 xs = 
  if xs == mergeGtfList xs
  then mergeGtfList xs
  else mergeGtfList2 $ L.sort $ mergeGtfList xs

showGtf :: Gtf -> Bi.ByteString
showGtf (Gtf chr source feature start end score strand frame gene_name) =
    B.intercalate "\t" [ chr, source, feature
                       , intToBs start
                       , intToBs end
                       , score
                       , strand
                       , frame
                       , B.concat ["gene_id \"", gene_name, "\"; "
                                 , "transcript_name \"", B.append gene_name ".tx", "\"; "
                                 , "transcript_id \"", B.append gene_name ".1", "\"; "
                                 , "gene_name \"", gene_name, "\";\n"]
                       ]

outputGtf :: [Gtf] -> Bi.ByteString
outputGtf = B.concat . map showGtf 

gtfToBed :: Gtf -> Bed
gtfToBed (Gtf chr source feature start end score strand frame name) =
    Bed chr start end (B.concat [name, "_", intToBs start]) score strand 

showBed :: Bed -> Bi.ByteString
showBed (Bed chr start end name score strand) =
    B.intercalate "\t" [ chr
                       , intToBs start
                       , intToBs end
                       , name, score, strand
                       , "\n"
                       ]

outputBed :: [Bed] -> Bi.ByteString
outputBed = B.concat . map showBed


example = do
    input <- B.take 40000 <$> B.readFile "/Users/minzhang/Documents/private_git/BioParsers/data/gencode.vM8.annotation.exon.gtf"
    let xs = Maybe.fromJust $ readGtf input
    print $ map length $ sortGtf xs
    let ys = map mergeGtfList $ sortGtf xs
    print $ length $ head ys 
    print $ last ys
--    let z = B.concat $ map showGtf $ xs
--    let res = Maybe.fromJust $ readGtf z
--    print res

collapseGtf input outputs = do
    gtf <- Maybe.fromJust . readGtf <$> unsafeMMapFile input
    let [plusStrand, minusStrand] = sortGtf gtf
    let plusStrandMerged = mergeGtfList2 plusStrand
    let minusStrandMerged = mergeGtfList2 minusStrand
    let outputPlus = outputs ++ "plus.gtf" 
    let outputMinus = outputs ++ "minus.gtf"
    B.writeFile outputPlus (outputGtf plusStrandMerged)
    B.writeFile outputMinus (outputGtf minusStrandMerged)
 -- output Bed files to look in IGV
    B.writeFile (outputPlus ++ ".original.bed") (outputBed $ map gtfToBed plusStrand)
    B.writeFile (outputMinus ++ ".original.bed") (outputBed $ map gtfToBed minusStrand)
    B.writeFile (outputPlus ++ ".bed") (outputBed $ map gtfToBed plusStrandMerged)
    B.writeFile (outputMinus ++ ".bed") (outputBed $ map gtfToBed minusStrandMerged)
    
    
