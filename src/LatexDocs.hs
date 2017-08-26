
{-# LANGUAGE QuasiQuotes #-}

module LatexDocs where

import Parser
import Eval
import ParseTree

import Control.Monad.Catch
import Control.Monad
import System.Process
import System.Directory
import Control.Lens
import Data.List
import Path


fname :: Path Rel File
fname = [relfile|out.tex|]

expandLatex :: Path Rel File -> Path Rel Dir -> IO ()
expandLatex input dir = do
  c <- readFile $ toFilePath input
  let parsed = parseDoc c
  void $ case parsed of
           Right dt -> do
             let docs = createDocuments dt
             setupDocuments dir docs
             mapM_ (\d -> do
                       docFolder <- getDocumentFolder d
                       compileLatex (dir </> docFolder </> fname) (dir </> docFolder)
                   ) docs
           Left e -> do
             putStrLn "Could not parse"
             print e

getDocumentFolder :: MonadThrow m => OutputDocument -> m (Path Rel Dir)
getDocumentFolder doc =
  let cs = view contextStack doc
  in parseRelDir $ intercalate "/" (reverse cs)

setupDocuments :: Path a Dir -> [OutputDocument] -> IO ()
setupDocuments fp =
  mapM_ (\doc -> do
            df <- getDocumentFolder doc
            let loc = fp </> df
            createDirectoryIfMissing True (toFilePath loc)
            writeFile (toFilePath $ loc </> fname) $ view content doc
        )

compileLatex :: Path Rel File -> Path Rel Dir -> IO ()
compileLatex path out =
  let outDir = toFilePath out
      inFile = toFilePath path
      command = "pdflatex -halt-on-error -output-directory " ++ outDir ++ " " ++ inFile
      p = shell command
  in void $ createProcess p
