{-#LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell #-}

{-
Project name: Parsing sam file using Haskell Parser
Min Zhang
Date: March 28, 2016
Version: v0.1.0
         v0.1.1 (April 7, 2016): rewrite after finishing ParseGtf for Dropseq 
README: 

-}
module ParseSam
where

import qualified Data.Set as Set
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Maybe as Maybe

import Control.Applicative
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 (putStrLn, pack)
import qualified Data.ByteString.Internal as Bi

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8

import System.IO.Posix.MMap (unsafeMMapFile)

import Control.Lens hiding (element)

import ParsingUtils
import ParseGtf

data Sam = Sam
    { _name_sam     :: !B.ByteString
    , _flag_sam     :: !Int
    , _chr_sam      :: !B.ByteString
    , _start_sam    :: !Int
    , _mapq_sam     :: !Int
    , _cigar_sam    :: !B.ByteString
    , _rnext_sam    :: !B.ByteString
    , _pnext_sam    :: !Int
    , _tlen_sam     :: !Int
    , _seq_sam      :: !B.ByteString
    , _qual_sam     :: !B.ByteString
    , _tag_sam      :: !B.ByteString
    } deriving (Show, Read)

makeLenses ''Sam

instance Eq Sam where
    (Sam name1 flag1 chr1 start1 _ _ _ _ _ _ _ _) ==
          (Sam name2 flag2 chr2 start2 _ _ _ _ _ _ _ _) 
        | and [name1 == name2, flag1 == flag2, chr1 == chr2, start1 == start2] = True
        | otherwise = False

instance Ord Sam where
    (Sam name1 flag1 chr1 start1 mapq1 cigar1 r1 p1 t1 s1 q1 tag1) `compare`
          (Sam name2 flag2 chr2 start2 mapq2 cigar2 r2 p2 t2 s2 q2 tag2) 
        | and [chr1 == chr2, start1 == start2, flag1 == flag2] = compare name1 name2
        | and [chr1 == chr2, start1 == start2] = compare flag1 flag2
        | chr1 == chr2 = compare start1 start2
        | otherwise = EQ

samParser = do
    name  <- AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    flag  <- bsToInt <$> AP.takeTill Bi.isSpaceWord8  -- 16 is reverse
    _     <- AP.word8 tb
    chr   <- AP.takeTill Bi.isSpaceWord8 
    _     <- AP.word8 tb
    start <- bsToInt <$> AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    mapq  <- bsToInt <$> AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    cigar <- AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    rnext <- AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    pnext <- bsToInt <$> AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    tlen  <- bsToInt <$> AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    seqs  <- AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    qual  <- AP.takeTill Bi.isSpaceWord8
    _     <- AP.word8 tb
    tag   <- AP.takeTill isEndOfLine
    _     <- AP.word8 nl
    return $ Sam name flag chr start mapq cigar rnext pnext tlen seqs qual tag

readSam s = AP.maybeResult $ AP.feed (AP.parse (AP.many1' samParser) s) "\n"

sortSam :: [Sam] -> [[Sam]]
sortSam = map L.sort .  L.groupBy ((==) `on` (\x -> view flag_sam x)) . L.sortBy (comparing (\x-> view flag_sam x))

samToInterval :: Int -> Sam -> Interval
samToInterval n (Sam name flag chr start _ _ _ _ _ _ _ _ ) = Interval chr start (start + n - 1)

showSam :: Sam -> Bi.ByteString
showSam (Sam name flag chr start mapq cigar rnext pnext tlen seqs qual tag) =
    B.intercalate "\t" [ name, intToBs flag, chr
                       , intToBs start, intToBs mapq
                       , cigar, rnext, intToBs pnext
                       , intToBs tlen, seqs, qual, B.append tag "\n"]

outputSam :: [Sam] -> Bi.ByteString
outputSam = B.concat . map showSam

-- assume [Sam] and [Gtf] are strand specific and sorted
annotateSamWithGtf :: [Sam] -> [Gtf] -> [Sam]
annotateSamWithGtf [] _ = []
annotateSamWithGtf s [] = s
annotateSamWithGtf (s:ss) (g:gs)
   | samToInterval 50 s == gtfToInterval g = (addGeneName s g) : (annotateSamWithGtf ss (g:gs))
   | samToInterval 50 s < gtfToInterval g =  (addNoName s) : annotateSamWithGtf ss (g:gs)
   | otherwise = annotateSamWithGtf (s:ss) gs
      where addGeneName sam gtf = set tag_sam (appendTag (gene_name_gtf gtf) (view tag_sam sam)) sam
            appendTag gene oldtag = B.concat [oldtag, "\tGE:Z:", gene] 
            addNoName sam = set tag_sam (appendTag "noname" (view tag_sam sam)) sam
            

example2 = do
    input <- B.take 20000 <$> B.readFile "/Users/minzhang/Documents/private_git/BioParsers/data/merged.tagged.aligned.2M.sam"
 --   return input
    let res = map (map (\x-> view qual_sam x)) $ sortSam $ Maybe.fromJust $ readSam input    
    return res

-- test annoataionSamWithGtf
example3 inputGtf inputSam outputGtfPath outputSamPath = do
    [gtfPlus, gtfMinus] <- sortGtf . Maybe.fromJust . readGtf <$> B.readFile inputGtf
    [samPlus, samMinus] <- sortSam . Maybe.fromJust . readSam <$> B.readFile inputSam
    let resPlus = annotateSamWithGtf samPlus gtfPlus
    let resMinus = annotateSamWithGtf samMinus gtfMinus
    let res = resPlus ++ resMinus
    return $ liftA2 toTuple length (length . filter (\x-> (B.reverse $ B.take 6 $ B.reverse (view tag_sam x)) == "noname")) res
--    B.writeFile outputGtfPath (outputGtf gtfMinus) 
--    B.writeFile outputSamPath (outputSam res)
--    print $ take 10 gtfPlus
--    print $ take 10 samPlus
    
test = example3 "/Users/minzhang/Documents/private_git/BioParsers/data/gencode.vM8.annotation.exon.merged.plusminus.gtf" "/Users/minzhang/Documents/private_git/BioParsers/data/merged.tagged.aligned.50k.sam" "/Users/minzhang/Documents/private_git/BioParsers/data/gtfminus.gtf" "/Users/minzhang/Documents/private_git/BioParsers/data/50k.tagged.sam"

tagSamWithGtf :: FilePath -> FilePath -> FilePath -> IO ()
tagSamWithGtf inputGtf inputSam outputSamPath = do
    [gtfPlus, gtfMinus] <- sortGtf . Maybe.fromJust . readGtf <$> B.readFile inputGtf
    [samPlus, samMinus] <- sortSam . Maybe.fromJust . readSam <$> B.readFile inputSam
    let resPlus = annotateSamWithGtf samPlus gtfPlus
    let resMinus = annotateSamWithGtf samMinus gtfMinus
    let res = resPlus ++ resMinus
    B.writeFile outputSamPath (outputSam res)

----
taggedSamParser = do
    _      <- AP.manyTill' AP8.anyChar (AP.string ("XC:Z:"))
    cellbc <- AP.takeTill Bi.isSpaceWord8
    _      <- AP.manyTill' AP8.anyChar (AP.string ("XM:Z:"))
    umi    <- AP.takeTill Bi.isSpaceWord8
    _      <- AP.manyTill' AP8.anyChar (AP.string ("GE:"))
    gene   <- AP.takeTill isEndOfLine
    _      <-  AP.word8 nl
    return $ (cellbc, [(gene, 1)])

readTaggedSam s = AP.maybeResult (AP.feed (AP.feed (AP.parse (AP.many1' taggedSamParser) s) "\n") B.empty) -- last is not new line, need an extra empty to stop

--TODO:estimateUmi :: [TaggedSam] -> [(Bi.ByteString, Int, Int, Int)]

example4 = do
    input <- B.readFile "/Users/minzhang/Documents/private_git/BioParsers/data/merged.tagged.aligned.50k.taggenes.sam"
    let res = Maybe.fromJust $ readTaggedSam input
    let cellbarcodes = M.filter ((>= 10) . M.size) $ M.map (M.fromListWith (+)) $ M.filter ((>= 50) . length) $ M.fromListWith (++) res
    return $ cellbarcodes

samToHashMap :: FilePath
               -> IO (M.HashMap Bi.ByteString (M.HashMap Bi.ByteString Int))
samToHashMap inputpath = do
    input <-  Maybe.fromJust . readTaggedSam <$> unsafeMMapFile inputpath
    let cellbarcodes = M.filter ((>=) 200 . M.size) $ M.map (M.fromListWith (+)) $ M.filter ((>= 800) . length) $ M.fromListWith (++) input
    return $ cellbarcodes

unionTwoMaps m1 m2 =
    M.unionWith (M.unionWith (+)) m1 m2

countGeneTags inputpaths = do
    inputs <- mapM samToHashMap inputpaths
    let f1 = head inputs
    let rest = drop 1 inputs
    return $ L.foldl' unionTwoMaps f1 rest

-- count genes per cell
example5 = do
    m <- countGeneTags (take 3 $ repeat "/Users/minzhang/Documents/private_git/BioParsers/data/merged.tagged.aligned.50k.taggenes.sam")
    return $ take 1000 $ L.reverse $ L.sort $ map (M.size . snd) $ M.toList m

getGenes :: M.HashMap a (M.HashMap Bi.ByteString v)
     -> M.HashMap Bi.ByteString Int
getGenes = M.fromList .
           map (\x-> (x, 0)) .
           concat . 
           map (M.keys . snd) .
           M.toList

example6 = do
    m <- countGeneTags (take 3 $ repeat "/Users/minzhang/Documents/private_git/BioParsers/data/merged.tagged.aligned.50k.taggenes.sam")
    return $ getGenes m
--   
geneCountMatrix  ms = B.intercalate "\n" $ 
                      map (B.intercalate "\t") $ 
                      L.transpose $ 
                      combineCountName $ 
                      M.toList $
                      M.map (M.toList . M.unionWith (+) m0) ms 
    where m0 = getGenes ms
          takeGeneCount (bc, genes) = bc : map (intToBs . snd) genes
          takeGeneName (bc, genes) = "genename" : map fst genes
          combineCountName = liftA2 (:) (takeGeneName . head) (map takeGeneCount)
          
          
--
samToMatrix inputpaths outputpath = do
    m <- countGeneTags inputpaths
    B.writeFile outputpath (geneCountMatrix m)


