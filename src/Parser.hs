module Parser (parseProject) where

import qualified Data.List as List
import Lexer (Token)
import Project

data ParserState = ParserState {input :: [Token], curTkn :: Token, nxtTkn :: Token, prevTkn :: Token}

createParserState :: [Token] -> Either String ParserState
createParserState tokens =
  case List.uncons tokens of
    Just (cur, tail) ->
      case List.uncons tail of
        Just (next, tail') -> Right $ ParserState {input = tail', prevTkn = cur, curTkn = cur, nxtTkn = next}
        Nothing -> Right $ ParserState {input = tail, prevTkn = cur, curTkn = cur, nxtTkn = cur}
    _ -> Left "Error parsing, no input."

-- | Moves to the next token in the input.
--   If the last token has been reached, this token will be moved into nxtTkn, curTKn and then prevTkn.
nextToken :: ParserState -> ParserState
nextToken state@ParserState {input, curTkn, nxtTkn} =
  case List.uncons input of
    Just (next', input') -> state {prevTkn = curTkn, curTkn = nxtTkn, nxtTkn = next', input = input'}
    Nothing -> state {prevTkn = curTkn, curTkn = nxtTkn}

parseProject :: [Token] -> Either String UncheckedProject
parseProject = undefined
