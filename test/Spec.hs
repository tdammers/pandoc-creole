import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc
import Text.Pandoc.Readers.Creole

main :: IO ()
main = defaultMain $
    testGroup "Tests"
        [ testGroup "empty document"
            [ testCase "empty document" $
                assertValid "" []
            , testCase "empty document (with newline)" $
                assertValid "\n\n" []
            ]
        , testGroup "paragraphs"
            [ testCase "one paragraph" $
                assertValid
                    "asdf\n"
                    [ Para
                        [ Str "asdf"
                        ]
                    ]
            , testCase "one paragraph, two lines" $
                assertValid
                    "asdf\nasdf"
                    [ Para
                        [ Str "asdf"
                        , Space
                        , Str "asdf"
                        ]
                    ]
            ]
        , testGroup "Nowiki Block"
            [ testCase "simple" $
                assertValid
                    "{{{\n!@#$%\n}}}"
                    [ CodeBlock nullAttr "!@#$%"
                    ]
            , testCase "with }}} inside" $
                assertValid
                    "{{{\n!@#$%\n }}}\n}}}"
                    [ CodeBlock nullAttr "!@#$%\n}}}"
                    ]
            ]
        , testGroup "Headings"
            [ testCase "simple" $
                assertValid
                    "= Level 1"
                    [ Header 1 nullAttr
                        [ Str "Level"
                        , Space
                        , Str "1" ]
                    ]
            , testCase "followed by newline" $
                assertValid
                    "= Level 1\n"
                    [ Header 1 nullAttr
                        [ Str "Level"
                        , Space
                        , Str "1" ]
                    ]
            , testCase "explicit terminator" $
                assertValid
                    "= Level 1 =="
                    [ Header 1 nullAttr
                        [ Str "Level"
                        , Space
                        , Str "1" ]
                    ]
            , testCase "explicit terminator, followed by newline" $
                assertValid
                    "= Level 1 ==\n"
                    [ Header 1 nullAttr
                        [ Str "Level"
                        , Space
                        , Str "1" ]
                    ]
            ]
        , testGroup "Nowiki inline"
            [ testCase "simple" $
                assertValid
                    "{{{!@#$%}}}"
                    [ Para [ Code nullAttr "!@#$%" ]
                    ]
            , testCase "more than 3 }'s" $
                assertValid
                    "{{{!@#$%}}}}"
                    [ Para [ Code nullAttr "!@#$%}" ]
                    ]
            ]
        , testGroup "Horizontal Rule"
            [ testCase "Horizontal line" $
                assertValid
                    "----\n"
                    [ HorizontalRule
                    ]
            , testCase "Horizontal line + Para" $
                assertValid
                    "----\nasdf"
                    [ HorizontalRule
                    , Para [Str "asdf"]
                    ]
            , testCase "Para + Horizontal line" $
                assertValid
                    "asdf\n----\nasdf"
                    [ Para
                        [ Str "asdf" ]
                    , HorizontalRule
                    , Para
                        [ Str "asdf"
                        ]
                    ]
            ]
        , testGroup "Inline markup: bold & italic"
            [ testCase "Bold" $
                assertValid
                    "**bold**\n"
                    [ Para
                        [ Strong [ Str "bold" ] ]
                    ]
            , testCase "Italic" $
                assertValid
                    "//italic//\n"
                    [ Para
                        [ Emph [ Str "italic" ] ]
                    ]
            , testCase "Bold Italic" $
                assertValid
                    "**//asdf//**\n"
                    [ Para
                        [ Strong [ Emph [ Str "asdf" ] ] ]
                    ]
            , testCase "Bold spanning two lines" $
                assertValid
                    "**asdf\nasdf**"
                    [ Para
                        [ Strong
                            [ Str "asdf"
                            , Space
                            , Str "asdf"
                            ]
                        ]
                    ]
            , testCase "Bold auto-closing at end of paragraph" $
                assertValid
                    "**asdf\n\nasdf"
                    [ Para
                        [ Strong [ Str "asdf" ] ]
                    , Para
                        [ Str "asdf" ]
                    ]
            ]
        ]

assertValid :: String -> [Block] -> IO ()
assertValid input output = do
    let actual = onLeft show $ readCreole def input
        expected = Right $ Pandoc nullMeta output
    assertEqual "" expected actual 
