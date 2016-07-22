module Text.Pandoc.Readers.Creole
( readCreole
, onLeft
, onRight
)
where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Parsec
import Data.List (intersperse, intercalate)

onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x) = Left (f x)
onLeft _ (Right r) = Right r

onRight :: (b -> c) -> Either a b -> Either a c
onRight f (Right x) = Right (f x)
onRight _ (Left r) = Left r

readCreole :: ReaderOptions -> String -> Either PandocError Pandoc
readCreole options input =
    onLeft (ParsecError input) $
    runParser wikipage () "creole source" input

wikipage = Pandoc nullMeta <$> (spaces *> manyTill paragraph eof)

paragraph = nowikiBlock
          <|> horizontalLine
          <|> emptyParagraph
          -- <|> unorderedList
          -- <|> orderedList
          <|> textParagraph

nowikiBlock = do
    try (string "{{{" *> (eol <|> eof))
    lns <- many nowikiLine
    string "}}}" *> (eol <|> eof)
    return $ CodeBlock nullAttr (intercalate "\n" lns)

nowikiLine = nowikiEscapedEndMarker <|> nowikiRegularLine

nowikiEscapedEndMarker = try $ do
    oneOf " \t"
    many (noneOf "\r\n") <* (eol <|> eof)

nowikiRegularLine = do
    notFollowedBy . try $ string "}}}" *> (eol <|> eof)
    many (noneOf "\r\n") <* (eol <|> eof)


emptyParagraph = try (endOfParagraph *> return Null)

horizontalLine = try (string "----") *> (eol <|> eof) *> return HorizontalRule

textParagraph = Para . concat . intersperse [Space] <$>
    many1 textLine <* endOfParagraph

textLine = do
    notFollowedBy $ string "----"
    many1 (notFollowedBy eol *> textItem) <* (eol <|> eof)

textItem =
    nowikiTextItem <|>
    boldTextItem <|>
    italTextItem <|>
    rawTextItem

boldTextItem = Strong <$> (try (string "**") *> many1 boldItalTextItem <* string "**")

italTextItem = Emph <$> (try (string "//") *> many1 italBoldTextItem <* string "//")

boldItalTextItem =
    nowikiTextItem <|>
    italTextItem <|>
    rawTextItem

italBoldTextItem =
    nowikiTextItem <|>
    boldTextItem <|>
    rawTextItem

rawTextItem = Str <$> many1 textChar

nowikiTextItem =
    Code nullAttr <$>
    (inlineNowikiStart *> manyTill inlineNowikiItem inlineNowikiEnd)

inlineNowikiEnd = try (string "}}}" *> notFollowedBy (char '}'))

inlineNowikiStart = try (string "{{{")

inlineNowikiItem = anyChar

textChar = escapedChar <|> safeChar

escapedChar = char '~' *> anyChar

safeChar = noneOf " \n\r\t~*/[]"

eol = (try (string "\r\n") <|> string "\n") *> return ()

ignore :: Parsec s u a -> Parsec s u ()
ignore = (*> return ())

endOfParagraph = ignore eol
               <|> ignore eof
               <|> ignore (lookAhead . try $ string "----")
