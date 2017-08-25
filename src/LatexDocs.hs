
module LatexDocs where

import Parser
import Eval
import ParseTree

import Control.Monad
import System.Process
import System.Directory
import Control.Lens
import Data.List


expandLatex :: FilePath -> FilePath -> IO ()
expandLatex input dir = do
  c <- readFile input
  let parsed = parseDoc c
  void $ case parsed of
           Right dt -> do
             let docs = createDocuments dt
             setupDocuments dir docs
             mapM_ (\d -> let out = dir ++ getDocumentFolder d
                              texfile = out ++ "/tmp.tex"
                          in compileLatex texfile out
                   ) docs
           Left e -> do
             putStrLn "Could not parse"
             print e


getDocumentFolder :: OutputDocument -> String
getDocumentFolder doc =
  intercalate "-" $ view contextStack doc


setupDocuments :: FilePath -> [OutputDocument] -> IO ()
setupDocuments fp docs = do
  _ <- mapM (\doc -> do
                let loc = fp ++ "/" ++ getDocumentFolder doc
                createDirectoryIfMissing True loc
                writeFile (loc ++ "/tmp.tex") $ view content doc
            ) docs
  return ()


compileLatex :: FilePath -> FilePath -> IO ()
compileLatex path out =
  let p = shell $ "pdflatex" ++ " -output-directory " ++ out ++ " " ++ path
  in void $ createProcess p
