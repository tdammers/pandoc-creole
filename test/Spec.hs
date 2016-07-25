{-#LANGUAGE TemplateHaskell #-}
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc
import Text.Pandoc.Readers.Creole
import Data.FileEmbed

main :: IO ()
main = defaultMain $
    testGroup "Tests"
        [ testGroup "Syntax"
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
            , testGroup "Div"
                [ testCase "simple" $
                    assertValid
                        "[[[\nHi!\n]]]"
                        [ Div nullAttr
                            [ Para
                                [ Str "Hi!" ]
                            ]
                        ]
                ]
            , testGroup "Unordered List"
                [ testCase "simple" $
                    assertValid
                        "* Item1\n* Item2\n"
                        [ BulletList
                            [ [Para [Str "Item1"]]
                            , [Para [Str "Item2"]]
                            ]
                        ]
                , testCase "nested" $
                    assertValid
                        "* Item1\n** Item2\n"
                        [ BulletList
                            [ [ Para [Str "Item1"]
                              , BulletList
                                [ [Para [Str "Item2"]]
                                ]
                              ]
                            ]
                        ]
                , testCase "nested, popping in and out" $
                    assertValid
                        "* Item1\n** Item2\n* Item3"
                        [ BulletList
                            [ [ Para [Str "Item1"]
                              , BulletList
                                [ [Para [Str "Item2"]]
                                ]
                              ]
                            , [ Para [Str "Item3"]
                              ]
                            ]
                        ]
                , testCase "containing link" $
                    assertValid
                        "asdf\n\n* [[/example|test (one)]]\\\\blah\n* Item2\n"
                        [ Para [ Str "asdf" ]
                        , BulletList
                            [ [ Para
                                [ Link 
                                    nullAttr
                                    [ Str "test (one)" ]
                                    ("/example", "test (one)")
                                , LineBreak
                                , Str "blah"
                                ]
                              ]
                            , [Para [Str "Item2"]]
                            ]
                        ]
                , testCase "after para" $
                    assertValid
                        "asdf\n\n* Item1\n* Item2\n"
                        [ Para [Str "asdf"]
                        , BulletList
                            [ [Para [Str "Item1"]]
                            , [Para [Str "Item2"]]
                            ]
                        ]
                ]
            , testGroup "Annotated Paragraphs"
                [ testCase "div with ID" $
                    assertValid
                        "@(id=foobar):\n[[[\nHi!\n]]]"
                        [ Div ("foobar", [], [])
                            [ Para
                                [ Str "Hi!" ]
                            ]
                        ]
                , testCase "div with classes" $
                    assertValid
                        "@(class=foo bar):\n[[[\nHi!\n]]]"
                        [ Div ("", ["foo", "bar"], [])
                            [ Para
                                [ Str "Hi!" ]
                            ]
                        ]
                , testCase "div with other attributes" $
                    assertValid
                        "@(data-thing=hello):\n[[[\nHi!\n]]]"
                        [ Div ("", [], [("data-thing", "hello")])
                            [ Para
                                [ Str "Hi!" ]
                            ]
                        ]
                , testCase "heading with ID" $
                    assertValid
                        "@(id=foobar):\n= Hi!"
                        [ Header 1 ("foobar", [], [])
                            [ Str "Hi!" ]
                        ]
                ]
            , testGroup "Headings"
                [ testCase "simple (level 1)" $
                    assertValid
                        "= Level 1"
                        [ Header 1 nullAttr
                            [ Str "Level"
                            , Space
                            , Str "1" ]
                        ]
                , testCase "simple (level 2)" $
                    assertValid
                        "== Level 2"
                        [ Header 2 nullAttr
                            [ Str "Level"
                            , Space
                            , Str "2" ]
                        ]
                , testCase "followed by newline" $
                    assertValid
                        "= Level 1\n"
                        [ Header 1 nullAttr
                            [ Str "Level"
                            , Space
                            , Str "1" ]
                        ]
                , testCase "after Para" $
                    assertValid
                        "asdf\n= Level 1\n"
                        [ Para
                            [ Str "asdf" ]
                        , Header 1 nullAttr
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
                , testCase "Horizontal line between Paras" $
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
            , testGroup "Explicit newlines"
                [ testCase "explicit newline" $
                    assertValid
                        "asdf \\\\\nasdf"
                        [ Para
                            [ Str "asdf"
                            , LineBreak
                            , Str "asdf"
                            ]
                        ]
                ]
            , testGroup "Inline escaping and special chars"
                [ testCase "escaped characters" $
                    assertValid
                        "~a~@~[~]"
                        [ Para
                            [ Str "a@[]" ]
                        ]
                , testCase "special characters requiring no escaping" $
                    assertValid
                        "/*[]{}"
                        [ Para
                            [ Str "/*[]{}" ]
                        ]
                ]
            , testGroup "Links"
                [ testCase "explicit label" $
                    assertValid
                        "[[http://example.org/|Example]]"
                        [ Para
                            [ Link
                                nullAttr
                                [ Str "Example" ]
                                ("http://example.org/", "Example")
                            ]
                        ]
                , testCase "explicit label" $
                    assertValid
                        "[[http://example.org/]]"
                        [ Para
                            [ Link
                                nullAttr
                                [ Str "http://example.org/" ]
                                ("http://example.org/", "http://example.org/")
                            ]
                        ]
                ]
            , testGroup "Images"
                [ testCase "explicit label" $
                    assertValid
                        "{{http://example.org/|Example}}"
                        [ Para
                            [ Image
                                nullAttr
                                [ Str "Example" ]
                                ("http://example.org/", "Example")
                            ]
                        ]
                , testCase "explicit label" $
                    assertValid
                        "{{http://example.org/}}"
                        [ Para
                            [ Image
                                nullAttr
                                [ Str "http://example.org/" ]
                                ("http://example.org/", "http://example.org/")
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
        , testGroup "Simulation"
            [ testCase "test001" $
                assertParses
                    $(embedStringFile "test/fixtures/test001.creole")
            ]
        ]

assertValid :: String -> [Block] -> IO ()
assertValid input output = do
    let actual = onLeft show $ readCreole def input
        expected = Right $ Pandoc nullMeta output
    assertEqual "" expected actual 

assertParses :: String -> IO ()
assertParses input = do
    let actual = onRight (const True) . onLeft show $ readCreole def input
        expected = Right True
    assertEqual "" expected actual 
