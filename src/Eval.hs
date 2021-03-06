
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Control.Monad
import ParseTree
import Control.Lens
import Data.Maybe


data Environment
  = Env
  { _ctxts :: ContextStack
  } deriving (Show)
makeLenses ''Environment

pushContext :: String -> Environment -> Environment
pushContext c e = Env (c:view ctxts e)

popContext :: Environment -> Environment
popContext = over ctxts tail

peekContext :: Environment -> Maybe String
peekContext e =
  case view ctxts e of
    [] -> Nothing
    xs -> Just $ head xs

mergeContexts :: ContextStack -> ContextStack -> Environment -> ContextStack
mergeContexts a b e =
  let a' = reverse a
      b' = reverse b
      e' = reverse $ view ctxts e
      drop' = drop $ length e'
      result = reverse $ mappend a' (drop' b')
  in result


createDocuments :: DocTree -> [OutputDocument]
createDocuments t =
  evaluate t (Env [])

evaluate :: DocTree -> Environment -> [OutputDocument]
evaluate (ContextBlock cs dt) e = do
  c <- cs
  evaluate dt (pushContext c e)
evaluate (IfBlock cs body) e =
  if fromMaybe False ((`elem` cs) <$> peekContext e)
  then evaluate body e
  else [Doc "" $ view ctxts e]
evaluate (DocText body) e =
  return Doc
  { _content = body
  , _contextStack = view ctxts e
  }
evaluate (DocTrees ts) e =
  let cross xs ys = join $ map (\x -> map (\y -> (x, y)) ys) xs
      mergeDocs (a, b) = Doc (view content a ++ view content b) (mergeContexts (view contextStack a) (view contextStack b) e)
  in foldl (\docs t -> (map mergeDocs $ cross docs (evaluate t e))) [Doc "" $ view ctxts e] ts
