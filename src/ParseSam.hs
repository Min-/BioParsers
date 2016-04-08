{-#LANGUAGE OverloadedStrings#-}

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

data Sam = Sam
    { 
    }
