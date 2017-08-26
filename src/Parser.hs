
-- module Parser ( parseDoc ) where
module Parser where

import Control.Monad
import ParseTree
import Text.ParserCombinators.Parsec


white :: Parser ()
white =
  void $ many space <|> many tab

specialCharacters :: String
specialCharacters =
  "~"

addWrapper :: Parser a -> Parser a
addWrapper x = white >> between (string "~") (string "~") x <* white

parseDoc :: String -> Either ParseError DocTree
parseDoc = parse (white >> DocTrees <$> many parseDocTree <* white <* eof) ""


parseDocTree :: Parser DocTree
parseDocTree =
  let p = try parseContextBlock
          <|> try parseIfBlock
          <|> try parseDocTreeText
  in white >> p <* white

parseDocTreeText :: Parser DocTree
parseDocTreeText =
  DocText <$> many1 (noneOf specialCharacters)

parseContextBlock :: Parser DocTree
parseContextBlock = do
  cs <- parseContextBlockOpen
  middle <- DocTrees <$> many parseDocTree
  parseContextBlockClose
  return (ContextBlock cs middle)

parseContextBlockOpen :: Parser [String]
parseContextBlockOpen = addWrapper $ do
  white >> string "Contexts" >> white
  many1 alphaNum `sepEndBy1` white

parseContextBlockClose :: Parser ()
parseContextBlockClose =
  addWrapper (white >> string "EndContexts" >> white)

parseIfBlock :: Parser DocTree
parseIfBlock = do
  s <- parseIfBlockOpen
  middle <- DocTrees <$> many parseDocTree
  parseIfBlockClose
  return $ IfBlock s middle

parseIfBlockOpen :: Parser [String]
parseIfBlockOpen = addWrapper $ do
  white >> string "If" >> white
  many1 alphaNum `sepEndBy1` white

parseIfBlockClose :: Parser ()
parseIfBlockClose =
  addWrapper $ white >> string "EndIf" >> white

parsePeek :: Parser DocStatement
parsePeek =
  const ContextPeek <$> (white >> string "Context" <* white)
