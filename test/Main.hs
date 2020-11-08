{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Boring.XML.Schema as Schema
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import qualified Text.XML as XML

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Boring.XML.Schema"
      [ contentAsTests
      ]

contentAsTests :: Tasty.TestTree
contentAsTests =
  Tasty.testGroup
    "contentAs"
    [ elementNotContentTest,
      instructionNotContentTest,
      noChildrenNoContentTest,
      childrenAllCommentsNoContentTest,
      contentParsingErrorTest,
      concatenateInputTest,
      stripInputTest,
      parseBoolTest
    ]
  where
    elementNotContentTest =
      HUnit.testCase "Returns an ElementNotContent error if one of the children is an Element" $
        let childElement =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = mempty
                }
            parentElement =
              XML.Element
                { elementName = "parent",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "content",
                      XML.NodeElement childElement,
                      XML.NodeComment "comment"
                    ]
                }
         in Schema.contentAs Right parentElement @?= Left (Schema.ElementNotContent childElement)

    instructionNotContentTest =
      HUnit.testCase "Returns an InstructionNotContent error if one of the children is an Instruction" $
        let instruction =
              XML.Instruction
                { instructionTarget = "target",
                  instructionData = "data"
                }
            element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "content",
                      XML.NodeInstruction instruction,
                      XML.NodeComment "comment"
                    ]
                }
         in Schema.contentAs Right element @?= Left (Schema.InstructionNotContent instruction)

    noChildrenNoContentTest =
      HUnit.testCase "Returns a NoContent error if the element has no children" $
        let element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes = []
                }
         in Schema.contentAs Right element @?= Left Schema.NoContent

    childrenAllCommentsNoContentTest =
      HUnit.testCase "Returns a NoContent error if all of the children are comments" $
        let element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeComment "not",
                      XML.NodeComment "that",
                      XML.NodeComment "important"
                    ]
                }
         in Schema.contentAs Right element @?= Left Schema.NoContent

    contentParsingErrorTest =
      HUnit.testCase "Returns a ContentParsingError error if the parsing function fails" $
        let input = "input"
            errorMessage = "nope"
            parser = const (Left errorMessage)
            element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent input
                    ]
                }
            contentParsingError = Schema.ContentParsingError (Schema.ParsingError input (Schema.ParsingErrorMessage errorMessage))
         in Schema.contentAs @Int parser element @?= Left contentParsingError

    concatenateInputTest =
      HUnit.testCase "Concatenates Content children" $
        let element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "pine",
                      XML.NodeComment "comment",
                      XML.NodeContent "apple"
                    ]
                }
         in Schema.contentAs Right element @?= Right "pineapple"

    stripInputTest =
      HUnit.testCase "Strips outer whitespaces from Content children" $
        let element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "\n  ",
                      XML.NodeContent "alpha",
                      XML.NodeComment "comment",
                      XML.NodeContent " ",
                      XML.NodeContent "bravo",
                      XML.NodeContent "\t \n\n"
                    ]
                }
         in Schema.contentAs Right element @?= Right "alpha bravo"

    parseBoolTest =
      HUnit.testCase "Successfully parses a boolean" $
        let parser = \case
              "True" -> Right True
              "False" -> Right False
              _ -> Left "Invalid boolean"
            element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "\n  ",
                      XML.NodeContent "True",
                      XML.NodeComment "comment",
                      XML.NodeContent "\n"
                    ]
                }
         in Schema.contentAs parser element @?= Right True
