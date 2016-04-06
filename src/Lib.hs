{-#LANGUAGE OverloadedStrings#-}

module Lib
    ( bioParser
    ) where

import System.Environment
import MergeFastq (mergePairedEndFq)

import Data.Text.IO as TextIO

bioParser = do
  args <- getArgs
  case (head args) of 
     "mergePairedEndFq" -> mergePairedEndFq (args!!1) (args!!2) 
     otherwise -> TextIO.putStrLn ""
