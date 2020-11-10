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
      [ contentAsTests,
        rootTests
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
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent "content",
                      XML.NodeElement childElement,
                      XML.NodeComment "comment"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema parentElement @?= Left (Schema.Path [], Schema.ElementNotContent childElement)

    instructionNotContentTest =
      HUnit.testCase "Returns an InstructionNotContent error if one of the children is an Instruction" $
        let instruction =
              XML.Instruction
                { instructionTarget = "target",
                  instructionData = "data"
                }
            element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent "content",
                      XML.NodeInstruction instruction,
                      XML.NodeComment "comment"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Left (Schema.Path [], Schema.InstructionNotContent instruction)

    noChildrenNoContentTest =
      HUnit.testCase "Returns a NoContent error if the element has no children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Left (Schema.Path [], Schema.NoContent)

    childrenAllCommentsNoContentTest =
      HUnit.testCase "Returns a NoContent error if all of the children are comments" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeComment "not",
                      XML.NodeComment "that",
                      XML.NodeComment "important"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Left (Schema.Path [], Schema.NoContent)

    contentParsingErrorTest =
      HUnit.testCase "Returns a ContentParsingError error if the parsing function fails" $
        let input = "input"
            errorMessage = "nope"
            parser = const (Left errorMessage)
            element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent input
                    ]
                }
            schema = Schema.contentAs @Int parser
            contentParsingError = Schema.ContentParsingError (Schema.ParsingError input (Schema.ParsingErrorMessage errorMessage))
         in Schema.applySchema schema element @?= Left (Schema.Path [], contentParsingError)

    concatenateInputTest =
      HUnit.testCase "Concatenates Content children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent "pine",
                      XML.NodeComment "comment",
                      XML.NodeContent "apple"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Right "pineapple"

    stripInputTest =
      HUnit.testCase "Strips outer whitespaces from Content children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent "\n  ",
                      XML.NodeContent "alpha",
                      XML.NodeComment "comment",
                      XML.NodeContent " ",
                      XML.NodeContent "bravo",
                      XML.NodeContent "\t \n\n"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Right "alpha bravo"

    parseBoolTest =
      HUnit.testCase "Successfully parses a boolean" $
        let parser = \case
              "True" -> Right True
              "False" -> Right False
              _ -> Left "Invalid boolean"
            element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes =
                    [ XML.NodeContent "\n  ",
                      XML.NodeContent "True",
                      XML.NodeComment "comment",
                      XML.NodeContent "\n"
                    ]
                }
            schema = Schema.contentAs parser
         in Schema.applySchema schema element @?= Right True

rootTests :: Tasty.TestTree
rootTests =
  Tasty.testGroup
    "root"
    [ invalidRootElementTest,
      schemaErrorTest,
      schemaValueTest
    ]
  where
    invalidRootElementTest =
      HUnit.testCase "Returns an InvalidRootElement error if the root element name doesn't match" $
        let element =
              XML.Element
                { elementName = "welp",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            schema = Schema.contentAs Right
            invalidRootElementError = Schema.InvalidRootElement element
         in Schema.root "name" schema element @?= Left (Schema.Path ["name"], invalidRootElementError)

    schemaErrorTest =
      HUnit.testCase "Surfaces the error returned by the given schema if the root element name does match" $
        let element =
              XML.Element
                { elementName = "name",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            schema = Schema.contentAs Right
         in Schema.root "name" schema element @?= Left (Schema.Path ["name"], Schema.NoContent)

    schemaValueTest =
      HUnit.testCase "Surfaces the value returned by the given schema if the root element name does match" $
        let element =
              XML.Element
                { elementName = "name",
                  elementAttributes = mempty,
                  elementNodes =
                    [ XML.NodeContent "content"
                    ]
                }
            schema = Schema.contentAs Right
         in Schema.root "name" schema element @?= Right "content"
