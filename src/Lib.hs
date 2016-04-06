{-#LANGUAGE OverloadedStrings#-}

module Lib
    ( bioParser
    ) where

import System.Environment
import MergeFastq (mergePairedEndFq)
import qualified Safe as S

import qualified Data.ByteString.Char8 as TextIO (putStrLn)


bioParser = do
  args <- getArgs
  case (S.headDef "mergePairedEndFq" args) of 
     "mergePairedEndFq" -> mergePairedEndFq (args!!1) (args!!2) 
     otherwise -> TextIO.putStrLn ""
