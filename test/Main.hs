{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Boring.XML.Schema as Schema
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import qualified Text.XML as XML

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Boring.XML.Schema"
      [ showPathTests,
        contentAsTests,
        rootTests,
        requiredElementTests,
        elementTests,
        elementsTests,
        elements1Tests,
        elementContentWithNameTests,
        attributeTests
      ]

showPathTests :: Tasty.TestTree
showPathTests =
  Tasty.testGroup
    "showPath"
    [ emptyPathTest,
      segmentsTest
    ]
  where
    emptyPathTest =
      HUnit.testCase "Returns `/` for the empty path" $
        Schema.showPath Schema.Empty @?= "/"

    segmentsTest =
      HUnit.testCase "Returns all segments preceded by `/` and with indices in square brackets" $
        let path =
              Schema.Root
                "a"
                [ Schema.SingleSegment "b",
                  Schema.IndexedSegment 42 "c",
                  Schema.SingleSegment "{d}e",
                  Schema.IndexedSegment 10 "{f}g"
                ]
         in Schema.showPath path @?= "/a/b/c[42]/{d}e/{f}g[10]"

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
         in Schema.applySchema schema parentElement @?= Left ([], Schema.ElementNotContent "child")

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
         in Schema.applySchema schema element @?= Left ([], Schema.InstructionNotContent instruction)

    noChildrenNoContentTest =
      HUnit.testCase "Returns a NoContent error if the element has no children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.contentAs Right
         in Schema.applySchema schema element @?= Left ([], Schema.NoContent)

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
         in Schema.applySchema schema element @?= Left ([], Schema.NoContent)

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
         in Schema.applySchema schema element @?= Left ([], contentParsingError)

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
    [ rootElementNotFoundTest,
      schemaErrorTest,
      schemaValueTest
    ]
  where
    rootElementNotFoundTest =
      HUnit.testCase "Returns an RootElementNotFound error if the name of the root element doesn't match" $
        let element =
              XML.Element
                { elementName = "welp",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            schema = Schema.contentAs Right
            rootElementNotFoundError = Schema.RootElementNotFound "name"
         in Schema.root "name" schema element @?= Left (Schema.Empty, rootElementNotFoundError)

    schemaErrorTest =
      HUnit.testCase "Surfaces the error returned by the given schema if the root element name does match" $
        let element =
              XML.Element
                { elementName = "name",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            schema = Schema.contentAs Right
         in Schema.root "name" schema element @?= Left (Schema.Root "name" [], Schema.NoContent)

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

requiredElementTests :: Tasty.TestTree
requiredElementTests =
  Tasty.testGroup
    "requiredElement"
    [ elementNotFoundTest,
      moreThanOneElementTest,
      childErrorTest,
      childValueTest
    ]
  where
    elementNotFoundTest =
      HUnit.testCase "Returns an ElementNotFound error if the element isn't found" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.requiredElement "element" (Schema.contentAs Right)
         in Schema.applySchema schema element @?= Left ([], Schema.ElementNotFound "element")

    moreThanOneElementTest =
      HUnit.testCase "Returns a MoreThanOneElement error if there is more than one element for the given name, even if the schema for the child was to return an error too" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [] -- Note how `contentAs Right` would fail here if `requiredElement "child"` was to succeed
                }
            childNode = XML.NodeElement child
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [childNode, childNode]
                }
            schema = Schema.requiredElement "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([], Schema.MoreThanOneElement "child")

    childErrorTest =
      HUnit.testCase "Surfaces the error returned by the child with the element name in the path" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.requiredElement "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.SingleSegment "child"], Schema.NoContent)

    childValueTest =
      HUnit.testCase "Surfaces the value returned by the child with the element name in the path" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.requiredElement "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Right "content"

elementTests :: Tasty.TestTree
elementTests =
  Tasty.testGroup
    "element"
    [ moreThanOneElementTest,
      childErrorTest,
      childValueTest,
      noChildTest
    ]
  where
    moreThanOneElementTest =
      HUnit.testCase "Returns a MoreThanOneElement error if there is more than one element for the given name, even if the schema for the child was to return an error too" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [] -- Note how `contentAs Right` would fail here if `requiredElement "child"` was to succeed
                }
            childNode = XML.NodeElement child
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [childNode, childNode]
                }
            schema = Schema.element "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([], Schema.MoreThanOneElement "child")

    childErrorTest =
      HUnit.testCase "Surfaces the error returned by the child with the element name in the path" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.element "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.SingleSegment "child"], Schema.NoContent)

    childValueTest =
      HUnit.testCase "Surfaces the value returned by the child with the element name in the path" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.element "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Right (Just "content")

    noChildTest =
      HUnit.testCase "Returns Nothing if the child element doesn't exist" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.element "element" (Schema.contentAs Right)
         in Schema.applySchema schema element @?= Right Nothing

elementsTests :: Tasty.TestTree
elementsTests =
  Tasty.testGroup
    "elements"
    [ childErrorTest,
      secondChildErrorTest,
      noChildTest,
      noMatchingChildTest,
      childrenContentTest
    ]
  where
    childErrorTest =
      HUnit.testCase "Surfaces the error returned by the first child with the element index and name in the path" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child, XML.NodeElement child]
                }
            schema = Schema.elements "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.IndexedSegment 1 "child"], Schema.NoContent)

    secondChildErrorTest =
      HUnit.testCase "Surfaces the error returned by the second child with the element index and name in the path" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child1 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            child2 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child1, XML.NodeElement child2]
                }
            schema = Schema.elements "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.IndexedSegment 2 "child"], Schema.NoContent)

    noChildTest =
      HUnit.testCase "Returns the empty list if the element doesn't have any children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.elements "child" (Schema.contentAs Right)
         in Schema.applySchema schema element @?= Right []

    noMatchingChildTest =
      HUnit.testCase "Returns the empty list if the element doesn't have any matching children" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.elements "other" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Right []

    childrenContentTest =
      HUnit.testCase "Surfaces content of matching children" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child1 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            child2 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "more content"]
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child1, XML.NodeElement child2]
                }
            schema = Schema.elements "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Right ["content", "more content"]

elements1Tests :: Tasty.TestTree
elements1Tests =
  Tasty.testGroup
    "elements1"
    [ childErrorTest,
      secondChildErrorTest,
      noChildTest,
      noMatchingChildTest,
      childrenContentTest
    ]
  where
    childErrorTest =
      HUnit.testCase "Surfaces the error returned by the first child with the element index and name in the path" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child, XML.NodeElement child]
                }
            schema = Schema.elements1 "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.IndexedSegment 1 "child"], Schema.NoContent)

    secondChildErrorTest =
      HUnit.testCase "Surfaces the error returned by the second child with the element index and name in the path" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child1 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            child2 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child1, XML.NodeElement child2]
                }
            schema = Schema.elements1 "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([Schema.IndexedSegment 2 "child"], Schema.NoContent)

    noChildTest =
      HUnit.testCase "Returns an ElementNotFound error if the element doesn't have any children" $
        let element =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = []
                }
            schema = Schema.elements1 "child" (Schema.contentAs Right)
         in Schema.applySchema schema element @?= Left ([], Schema.ElementNotFound "child")

    noMatchingChildTest =
      HUnit.testCase "Returns an ElementNotFound error if the element doesn't have any matching children" $
        let child =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = []
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement child]
                }
            schema = Schema.elements1 "other" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Left ([], Schema.ElementNotFound "other")

    childrenContentTest =
      HUnit.testCase "Surfaces content of matching children" $
        let other =
              XML.Element
                { elementName = "other",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "ok"]
                }
            child1 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "content"]
                }
            child2 =
              XML.Element
                { elementName = "child",
                  elementAttributes = mempty,
                  elementNodes = [XML.NodeContent "more content"]
                }
            parent =
              Schema.ElementContent
                { ecAttributes = mempty,
                  ecNodes = [XML.NodeElement other, XML.NodeElement child1, XML.NodeElement child2]
                }
            schema = Schema.elements1 "child" (Schema.contentAs Right)
         in Schema.applySchema schema parent @?= Right ("content" :| ["more content"])

elementContentWithNameTests :: Tasty.TestTree
elementContentWithNameTests =
  Tasty.testGroup
    "elementContentWithName"
    [ differentLocalNameTest,
      localNameNoNamespaceTest,
      localNameMatchTest,
      differentNamespaceTest,
      namespaceMatchTest
    ]
  where
    differentLocalNameTest =
      HUnit.testCase "Returns nothing if local name doesn't match" $
        let element =
              XML.Element
                { elementName = "element",
                  elementAttributes = mempty,
                  elementNodes = []
                }
         in Schema.elementContentWithName "element'" element @?= Nothing

    localNameNoNamespaceTest =
      HUnit.testCase "Returns nothing if local name does match but element has a namespace" $
        let element =
              XML.Element
                { elementName = XML.Name "element" (Just "namespace") Nothing,
                  elementAttributes = mempty,
                  elementNodes = []
                }
         in Schema.elementContentWithName "element" element @?= Nothing

    localNameMatchTest =
      HUnit.testCase "Returns element content if local name does match ignoring prefix" $
        let element =
              XML.Element
                { elementName = XML.Name "element" Nothing (Just "prefix"),
                  elementAttributes = mempty,
                  elementNodes = []
                }
            elementContent = Schema.ElementContent mempty []
         in Schema.elementContentWithName "element" element @?= Just elementContent

    differentNamespaceTest =
      HUnit.testCase "Returns nothing if namespace does not match" $
        let element =
              XML.Element
                { elementName = XML.Name "element" (Just "namespace") (Just "prefix"),
                  elementAttributes = mempty,
                  elementNodes = []
                }
         in Schema.elementContentWithName "{namespace'}element" element @?= Nothing

    namespaceMatchTest =
      HUnit.testCase "Returns element content if namespace and local name do match ignoring prefix" $
        let element =
              XML.Element
                { elementName = XML.Name "element" (Just "namespace") (Just "prefix"),
                  elementAttributes = mempty,
                  elementNodes = []
                }
            elementContent = Schema.ElementContent mempty []
         in Schema.elementContentWithName "{namespace}element" element @?= Just elementContent

attributeTests :: Tasty.TestTree
attributeTests =
  Tasty.testGroup
    "attribute"
    [ attributeNotFoundTest,
      attributeErrorTest,
      attributeValueTest
    ]
  where
    attributeNotFoundTest =
      HUnit.testCase "Returns nothing if the attribute doesn't exist" $
        let element =
              Schema.ElementContent
                { ecAttributes = Map.singleton (XML.Name "attribute" (Just "namespace") Nothing) "value",
                  ecNodes = []
                }
            schema = Schema.attribute "attribute" Right
         in Schema.applySchema schema element @?= Right Nothing

    attributeErrorTest =
      HUnit.testCase "Surfaces parsing error in the attribute" $
        let name = XML.Name "attribute" (Just "namespace") Nothing
            value = "value"
            element =
              Schema.ElementContent
                { ecAttributes = Map.singleton name value,
                  ecNodes = []
                }
            parsingError = "nope"
            schema = Schema.attribute @Int name (const (Left parsingError))
            attributeParsingError = Schema.AttributeParsingError name (Schema.ParsingError value (Schema.ParsingErrorMessage parsingError))
         in Schema.applySchema schema element @?= Left ([], attributeParsingError)

    attributeValueTest =
      HUnit.testCase "Surfaces parsed value in the attribute" $
        let name = XML.Name "attribute" (Just "namespace") Nothing
            value = "value"
            element =
              Schema.ElementContent
                { ecAttributes = Map.singleton name value,
                  ecNodes = []
                }
            schema = Schema.attribute name Right
         in Schema.applySchema schema element @?= Right (Just value)
