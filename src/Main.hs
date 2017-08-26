
{-# LANGUAGE QuasiQuotes #-}

module Main where

import LatexDocs
import System.Environment
import Path

main :: IO ()
main = do
  ss <- getArgs
  p <- parseRelFile (head ss)
  if length ss < 1
    then print "Must supply an input file."
    else expandLatex p [reldir|out/|]
