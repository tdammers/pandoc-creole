module Text.Pandoc.Readers.Creole
( readCreole
, onLeft
, onRight
)
where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Parsec
import Data.List (intersperse, intercalate, lookup)
import Data.Maybe (fromMaybe)

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
          <|> division nullAttr
          <|> horizontalLine
          <|> heading nullAttr
          <|> annotatedParagraph
          <|> emptyParagraph
          <|> unorderedList 1
          -- <|> orderedList
          <|> textParagraph

unorderedList level = BulletList <$> many1 (unorderedListItem level)

unorderedListItem level = do
    unorderedListBullet level
    unorderedListItemContent level

unorderedListBullet level = do
    try (lookAhead $ char '*')
    try (count level (char '*') *> whitespace1)

unorderedListItemContent :: Int -> Parsec String () [Block]
unorderedListItemContent level = (:[]) . Para . concat <$> many1 (unorderedListLine level)

unorderedListLine level = do
    notFollowedBy $
        (ignore . try $ unorderedListBullet level) <|>
        (ignore . try $ whitespace *> string "----") <|>
        (ignore . try $ whitespace *> char '=') <|>
        (ignore . try $ whitespace *> string "]]]" *> notFollowedBy (char ']')) <|>
        (ignore eol) <|>
        (ignore eof)
    many1 (notFollowedBy eol *> textItem) <* (eol <|> eof)

annotatedParagraph = do
    attr <- try preAnnotation
    division attr <|> heading attr

annotation = do
    string "@("
    whitespace
    kvp <- sepBy attribute (char ',')
    let idVal = fromMaybe "" . lookup "id" $ kvp
        classes = words . fromMaybe "" . lookup "class" $ kvp
        kvp' = [ (k,v) | (k,v) <- kvp, k /= "id" && k /= "class" ]
    char ')'
    return (idVal, classes, kvp')
    where
        attribute = do
            k <- many (noneOf ")= \t\r\n")
            whitespace
            char '='
            whitespace
            v <- many (noneOf "),\r\n")
            whitespace
            return (k,v)

preAnnotation = annotation <* char ':' <* (eol <|> eof)

postAnnotation = optional eol *> annotation <* notFollowedBy (char ':')

heading attr = do
    leader <- many1 (char '=')
    whitespace
    inner <- manyTill textItem endOfHeading
    attr' <- option attr (try postAnnotation)
    return $ Header (length leader) attr' inner

endOfHeading = eof
             <|> try
                 (do
                    whitespace
                    many (char '=')
                    whitespace
                    ignore eol <|> eof
                 )

division attr = do
    try (string "[[[" *> (eol <|> eof))
    inner <- manyTill paragraph endOfDiv
    return $ Div attr inner

endOfDiv = eof
         <|> try
             (do
                whitespace
                string "]]]"
                notFollowedBy (char ']')
                whitespace
                ignore eol <|> eof
             )

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
    notFollowedBy $
        (ignore . try $ unorderedListBullet 1) <|>
        (ignore . try $ whitespace *> string "----") <|>
        (ignore . try $ whitespace *> char '=') <|>
        (ignore . try $ whitespace *> string "]]]" *> notFollowedBy (char ']')) <|>
        (ignore eol) <|>
        (ignore eof)
    many1 (notFollowedBy eol *> textItem) <* (eol <|> eof)

textItem =
    nowikiTextItem <|>
    newlineTextItem <|>
    link <|>
    image <|>
    boldTextItem <|>
    italTextItem <|>
    whitespaceTextItem <|>
    rawTextItem

link = do
    try $ string "[[" *> notFollowedBy (char '[')
    url <- many $ noneOf "|]"
    label <- option url $ do
        char '|'
        many $ noneOf "]"
    string "]]"
    attr <- option nullAttr (try postAnnotation)
    return $ Link attr [ Str label ] (url, label)

image = do
    try $ string "{{" *> notFollowedBy (char '{')
    url <- many $ noneOf "|}"
    label <- option url $ do
        char '|'
        many $ noneOf "}"
    string "}}"
    attr <- option nullAttr (try postAnnotation)
    return $ Image attr [ Str label ] (url, label)

newlineTextItem = do
    try $ whitespace *> string "\\\\"
    whitespace
    optional eol
    whitespace
    return LineBreak

boldTextItem = Strong <$>
    (
        try (string "**" *> notFollowedBy whitespace1) *>
        many1 boldItalTextItem <*
        (
            eof <|>
            (lookAhead . try $ eol *> endOfParagraph) <|>
            (ignore . try . string $ "**")
        )
    )

italTextItem = Emph <$>
    (
        try (string "//") *>
        many1 italBoldTextItem <*
        (
            eof <|>
            (lookAhead . try $ eol *> endOfParagraph) <|>
            (ignore . try . string $ "//")
        )
    )


boldItalTextItem =
    nowikiTextItem <|>
    newlineTextItem <|>
    link <|>
    image <|>
    italTextItem <|>
    whitespaceTextItem <|>
    rawTextItem

italBoldTextItem =
    nowikiTextItem <|>
    newlineTextItem <|>
    link <|>
    image <|>
    boldTextItem <|>
    whitespaceTextItem <|>
    rawTextItem

rawTextItem = Str <$> many1 textChar

whitespaceTextItem = Space <$ many1 irrelevantWhitespace

irrelevantWhitespace =
    (ignore . many1) (oneOf " \t") <|> try (eol *> notFollowedBy eol)

nowikiTextItem =
    Code nullAttr <$>
    (inlineNowikiStart *> manyTill inlineNowikiItem inlineNowikiEnd)

inlineNowikiEnd = try (string "}}}" *> notFollowedBy (char '}'))

inlineNowikiStart = try (string "{{{")

inlineNowikiItem = anyChar

textChar = escapedChar <|> safeChar <|> allowedSpecialChar

escapedChar = char '~' *> anyChar

safeChar = noneOf " \n\r\t~*/[]\\{}@"

allowedSpecialChar =
    try (char '/' <* notFollowedBy (char '/')) <|>
    try (char '*' <* notFollowedBy (char '*')) <|>
    try (char '[' <* notFollowedBy (char '[')) <|>
    try (char '{' <* notFollowedBy (char '{')) <|>
    try (char '\\' <* notFollowedBy (char '\\')) <|>
    try (char ']' <* notFollowedBy (char ']')) <|>
    try (char '}' <* notFollowedBy (char '}')) <|>
    try (char '@' <* notFollowedBy (char '('))

eol = (try (string "\r\n") <|> string "\n") *> return ()

whitespaceChar = oneOf " \t"

whitespace1 = many1 whitespaceChar

whitespace = many whitespaceChar

ignore :: Parsec s u a -> Parsec s u ()
ignore = (*> return ())

endOfParagraph = (ignore . lookAhead . try $ whitespace *> string "----")
               <|> (ignore . lookAhead . try $ whitespace *> char '=')
               <|> (ignore . lookAhead . try $ whitespace *> string "]]]" *> notFollowedBy (char ']'))
               <|> ignore eof
               <|> ignore eol
