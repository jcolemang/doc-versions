
{-# LANGUAGE TemplateHaskell #-}

module ParseTree ( DocTree (..)
                 , DocStatement (..)
                 , DocValue (..)
                 , DocContext
                 , OutputDocument (..), content, contextStack
                 , ContextStack
                 ) where

import Control.Lens

type ContextStack
  = [String]

type DocContext = String
data OutputDocument
  = Doc
  { _content      :: String
  , _contextStack :: ContextStack
  }
makeLenses ''OutputDocument

-- | DocTree --> <% Contexts ... %> DocTree <% EndContexts %>
--   DocTree --> <% If DocStatement %> DocTree <% EndIf %>
--   DocTree -->
--   DocTree --> Text
--   DocTree --> DocTree DocTree

data DocTree
  = ContextBlock [String] DocTree
  | IfBlock [String] DocTree
  | DocText String
  | DocTrees [DocTree]
  deriving (Show)


-- | DocStatement --> DocStatement or DocStatement
--   DocStatement --> Peek
--   DocStatement --> DocStatement == DocStatement
--   DocStatement --> DocValue

data DocStatement
  = OrStatement DocStatement DocStatement
  | ContextPeek
  | EqStatement DocStatement DocStatement
  | DocVal DocValue
  deriving (Show)


-- | DocValue --> True
--   DocValue --> False

data DocValue
  = DocTrue
  | DocFalse
  | NamedContext String
  deriving (Show)
