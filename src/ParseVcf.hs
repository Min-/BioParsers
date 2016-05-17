{-#LANGUAGE OverloadedStrings#-}

{-
Project name: MergeFastq 
Min Zhang
Date: April 5, 2016
Version: v0.1.0
README: Merge Paired-end fastq file to one file (stitch reads together)

NOTE: Currently in v0.1.0, it assumes that two fastq files are sorted in the same order; non-paired reads are thrown away

-}

module ParseVcf
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
import qualified Data.ByteString.Char8 as B8 (putStrLn, pack, unpack)
import qualified Data.ByteString.Internal as Bi
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8

import System.IO.Posix.MMap (unsafeMMapFile)

import ParsingUtils

data Vcf = Vcf
    { chr_vcf     :: !B.ByteString
    , loc_vcf     :: Int
    , id_vcf      :: !B.ByteString
    , ref_vcf     :: !B.ByteString
    , alt_vcf     :: [B.ByteString]
    , qual_vcf    :: Float
    , filter_vcf  :: !B.ByteString
    , ac_vcf      :: Int     -- allele count
    , af_vcf      :: [Float]   -- allele freq
    , an_vcf      :: Int     -- total number of alleles
    , dp_vcf      :: Int     -- total depth
    , dps_vcf     :: [Int]
    } deriving (Read, Ord, Eq)

instance Show Vcf where
    show (Vcf chr loc id ref alt qual filter ac af an dp dps) = B8.unpack $ B.intercalate  "\t" 
                                                                [chr, intToBs loc, id, ref, B.intercalate "," alt
                                                               , floatToBs qual, filter, B.intercalate "," $ map floatToBs af
                                                               , intToBs an, intToBs dp, B.intercalate "," $ map intToBs dps]

data Snp = Snp
    { chr_snp     :: !B.ByteString
    , loc_snp     :: Int
    , ref_snp     :: !B.ByteString
    } deriving (Show, Read, Eq, Ord)

readVcf g = AP.maybeResult $ AP.feed (AP.parse (AP.many1' vcfLineParser) g) B.empty
  
vcfLineParser = do
    chr <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    loc <-  bsToInt <$> AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    id <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    ref <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    alt <- B.split cm <$> AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    qual <- bsToFloat <$> AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    filter <- AP.takeTill Bi.isSpaceWord8
    _ <- AP.word8 tb
    _ <- AP.manyTill' AP8.anyChar (AP.string ("AC="))
    ac <- bsToInt <$> AP.takeTill isSm
    _ <- AP.manyTill' AP8.anyChar (AP.string ("AF="))
    af <- map bsToFloat . B.split cm <$> AP.takeTill isSm
    _ <- AP.manyTill' AP8.anyChar (AP.string ("AN="))
    an <- bsToInt <$> AP.takeTill isSm
    _ <- AP.manyTill' AP8.anyChar (AP.string ("GT:AD:DP:GQ:PL\t"))
    _ <- AP.takeTill isCl     
    _ <- AP.word8 cl
    dps <- map bsToInt . B.split cm <$> AP.takeTill isCl
    _ <- AP.word8 cl
    dp <- bsToInt <$> AP.takeWhile isDigit
    _ <- AP.manyTill' AP8.anyChar (AP.string "\n")
    return $ Vcf chr loc id ref alt qual filter ac af an dp dps

oneline = "chr3\t89881777\t.\tCT\tC,CTT\t317.73\t.\tAC=1,1;AF=0.500,0.500;AN=2;DP=15;ExcessHet=3.0103;FS=0.000;MLEAC=1,1;MLEAF=0.500,0.500;MQ=39.25;QD=22.70;SOR=1.765\tGT:AD:DP:GQ:PL\t1/2:0,8,6:14:99:355,166,188,225,0,266\n" :: B.ByteString

example1 = do
    input <- unsafeMMapFile "/Users/minzhang/Documents/data/P41_Pitx2EnhancerPaper/ctcf_chipmentation/mapped_ctcf_chipseq/s5.sam.sorted.bam.chr3.raw_variants.vcf.gz.ctcf.vcf"
    let xs = Maybe.fromJust $ readVcf input
    print $ map dp_vcf xs 
--    print $ last ys
--    let z = B.concat $ map showGtf $ xs
--    let res = Maybe.fromJust $ readGtf z
--    print res

--showGtf :: Gtf -> Bi.ByteString
--showGtf (Gtf chr source feature start end score strand frame gene_name) =
--    B.intercalate "\t" [ chr, source, feature
--                       , intToBs start
--                       , intToBs end
--                       , score
--                       , strand
--                       , frame
--                       , B.concat ["gene_id \"", gene_name, "\"; "
--                                 , "transcript_name \"", B.append gene_name ".tx", "\"; "
--                                 , "transcript_id \"", B.append gene_name ".1", "\"; "
--                                 , "gene_name \"", gene_name, "\";\n"]
--                       ]
--
--outputGtf :: [Gtf] -> Bi.ByteString
--outputGtf = B.concat . map showGtf 
--
--gtfToBed :: Gtf -> Bed
--gtfToBed (Gtf chr source feature start end score strand frame name) =
--    Bed chr start end (B.concat [name, "_", intToBs start]) score strand 
--
getSnp :: Vcf -> Snp
getSnp v = Snp (chr_vcf v) (loc_vcf v) (ref_vcf v) 

getSnpByRange :: Set.Set Vcf -> B.ByteString -> Int -> Int -> Set.Set Vcf
getSnpByRange vcfs chr s1 s2 = Set.filter (\v -> and [ loc_vcf v >= s1
                                                     , loc_vcf v <= s2
                                                     , chr_vcf v == chr]) vcfs

filterVcfBySnp :: Set.Set Snp -> Set.Set Vcf -> Set.Set Vcf
filterVcfBySnp snps vcfs = Set.filter ((\s -> Set.member s snps) . getSnp) vcfs

samplePaths = [ "/Users/minzhang/Documents/data/P41_Pitx2EnhancerPaper/ctcf_chipmentation/mapped_ctcf_chipseq/s2.sam.sorted.bam.chr3.raw_variants.vcf.gz.ctcf.vcf"
             , "/Users/minzhang/Documents/data/P41_Pitx2EnhancerPaper/ctcf_chipmentation/mapped_ctcf_chipseq/s12.chr3.raw_variants.vcf.gz.ctcf.vcf"
             , "/Users/minzhang/Documents/data/P41_Pitx2EnhancerPaper/ctcf_chipmentation/mapped_ctcf_chipseq/s3.chr3.raw_variants.vcf.gz.ctcf.vcf"
             , "/Users/minzhang/Documents/data/P41_Pitx2EnhancerPaper/ctcf_chipmentation/mapped_ctcf_chipseq/s5.sam.sorted.bam.chr3.raw_variants.vcf.gz.ctcf.vcf"]

-- common snps in all 4 samples
allSamples = do
  inputs <- mapM unsafeMMapFile samplePaths
  let samples = map (Set.fromList . Maybe.fromJust . readVcf) inputs
  let snpsInEachSample = map (Set.map getSnp) samples
  let unionSnp = Set.unions snpsInEachSample
  let intersectSnp = L.foldl' Set.intersection unionSnp snpsInEachSample
  let commonVcf_samples = map (filterVcfBySnp intersectSnp) samples
  -- see if we get differences 
  let res = map (map show . Set.toList . (\v -> getSnpByRange v "chr3" 129010965 129011425)) commonVcf_samples
  print $ Set.size unionSnp
  print $ Set.size intersectSnp
  mapM (mapM putStrLn) res

