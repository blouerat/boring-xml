module Main where

import qualified Boring.XML.Schema as Schema
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.XML as XML

main :: IO ()
main = do
  XML.Document {..} <- XML.readFile XML.def "example.xml"
  putStrLn case Schema.root "example" exampleSchema documentRoot of
    Left (path, err) -> "Invalid XML: " <> show err <> " @ " <> show path
    Right Example {..} -> Text.unpack $ displayMessage message

newtype Example = Example
  { message :: Message
  }

exampleSchema :: Schema.Schema Example
exampleSchema =
  Example
    <$> Schema.requiredElement "message" messageSchema

data Message = Message
  { messageType :: Text,
    description :: Maybe Text,
    value :: Text,
    published :: Maybe Bool,
    tags :: [Text],
    comments :: [Comment]
  }

data Comment = Comment
  { content :: Text,
    useful :: Maybe Bool
  }

messageSchema :: Schema.Schema Message
messageSchema =
  Message
    <$> Schema.requiredAttribute "type" Right
    <*> Schema.element "description" contentAsText
    <*> Schema.requiredElement (withExampleNamespace "value") contentAsText
    <*> Schema.element (withExampleNamespace "published") contentAsBool
    <*> Schema.elements "tag" contentAsText
    <*> commentsSchema
  where
    contentAsText =
      Schema.contentAs Right

    contentAsBool =
      Schema.contentAs parseBool

    parseBool = \case
      "True" -> Right True
      "False" -> Right False
      _ -> Left "Invalid boolean value"

    commentsSchema =
      foldMap NonEmpty.toList <$> Schema.element "comments" (Schema.elements1 "comment" commentSchema)

    commentSchema =
      Comment
        <$> contentAsText
        <*> Schema.attribute (withExampleNamespace "useful") parseBool

    withExampleNamespace localName =
      XML.Name localName (Just "http://example.org/schema") Nothing

displayMessage :: Message -> Text
displayMessage Message {..} =
  messageType
    <> ": "
    <> value
    <> foldMap displayDescription description
    <> foldMap displayPublished published
    <> displayTags
    <> displayComments
  where
    displayDescription d =
      " (" <> d <> ")"

    displayPublished p =
      if p then "" else " -- UNPUBLISHED"

    displayTags =
      case tags of
        [] -> ""
        _ -> "\n  tags: " <> Text.intercalate ", " tags

    displayComments =
      case comments of
        [] -> ""
        _ -> "\n  comments: " <> Text.intercalate ", " (displayComment <$> comments)

    displayComment Comment {..} =
      content <> if Foldable.or useful then " (+)" else ""
