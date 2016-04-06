{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Parsing sam file using Haskell Parser
Min Zhang
Date: March 28, 2016
Version: v0.1.0
README: Serious parsing using Haskell Parser instead of data munging.

-}
module ParseSam
where

import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import qualified Data.Set as Set
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified Data.ByteString.Lazy.Char8 as Bl
import qualified Data.ByteString.Char8 as B

import Control.Monad (fmap)

import Data.Ord (comparing)
import Data.Function (on)

import qualified Safe as S

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC


inputpath = "/Users/minzhang/Documents/private_git/BioParsers/data/sample.sam"

data HD = HD { vn_hd :: T.Text
             , so_hd :: T.Text} 
          deriving (Show, Read, Eq)

data SQ = SQ { sn_sq :: T.Text
             , ln_sq :: T.Text
             }
          deriving (Show, Read, Eq)

parser_HD_line = do
  A.string "@HD\t"
  a <- A.takeTill (\x->x == '\n')
  A.endOfLine
  return $ a

main1 = A.parse parser_HD_line "@HD\taDfa\n"

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                           "" -> []
                           (c:cs) -> [(c, cs)])

parse (Parse a) = a

instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | 
                                     (a, cs') <- parse p cs])
