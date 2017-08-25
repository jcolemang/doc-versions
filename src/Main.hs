module Main where

import LatexDocs
import System.Environment

main :: IO ()
main = do
  ss <- getArgs
  if length ss < 1
    then print "Must supply an input file."
    else expandLatex (head ss) "./output/"
