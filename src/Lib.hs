{-#LANGUAGE OverloadedStrings#-}

module Lib
    ( bioParser
    ) where

import MergeFastq (mergePairedEndFq)
import ParseGtf   (collapseGtf)
import ParseSam   (tagSamWithGtf)

import System.Environment
import qualified Safe as S

import qualified Data.ByteString.Char8 as B8 (putStrLn)


bioParser = do
  args <- getArgs
  case (S.headDef "mergePairedEndFq" args) of 
     "mergePairedEndFq" -> mergePairedEndFq (args!!1) (args!!2) 
     "collapseGtf"      -> collapseGtf (args!!1) (args!!2)
     "tagSamWithGtf"    -> tagSamWithGtf (args!!1) (args!!2) (args!!3)
     otherwise -> B8.putStrLn "Not an option"


