{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Boring.XML.Schema where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.XML as XML

newtype Schema a = Schema
  { applySchema :: ElementContent -> Either (Segments, Error) a
  }
  deriving (Functor, Applicative) via (Reader.ReaderT ElementContent (Either (Segments, Error)))

data ElementContent = ElementContent
  { ecAttributes :: Map XML.Name Text,
    ecNodes :: [XML.Node]
  }

data Segment
  = SingleSegment Text
  | IndexedSegment Int Text
  deriving (Eq, Show)

type Segments = [Segment]

prependSingleSegment :: Text -> Segments -> Segments
prependSingleSegment segment segments =
  SingleSegment segment : segments

prependIndexedSegment :: Int -> Text -> Segments -> Segments
prependIndexedSegment ix segment segments =
  IndexedSegment ix segment : segments

data Path
  = Empty
  | Root Text Segments
  deriving Eq

instance Show Path where
  show = Text.unpack . showPath

showPath :: Path -> Text
showPath path =
  "/" <> Text.intercalate "/" (showSegments path)
  where
    showSegments =
      \case
        Empty -> []
        Root rootSegment childrenSegments -> escape rootSegment : fmap showSegment childrenSegments

    showSegment =
      \case
        SingleSegment segment -> escape segment
        IndexedSegment ix segment -> escape segment <> "[" <> Text.pack (show ix) <> "]"

    escape =
      Text.replace "/" "\\/" . Text.replace "[" "\\[" . Text.replace "]" "\\]"

data Error
  = ElementNotContent XML.Element
  | InstructionNotContent XML.Instruction
  | NoContent
  | ContentParsingError ParsingError
  | RootElementNotFound Text
  | ElementNotFound Text
  | MoreThanOneElement Text
  deriving stock (Eq, Show)

data ParsingError = ParsingError
  { eInput :: Text,
    eMessage :: ParsingErrorMessage
  }
  deriving stock (Eq, Show)

newtype ParsingErrorMessage
  = ParsingErrorMessage Text
  deriving newtype (Eq, Show)

-- | Extracts content from the children of an element.
--
-- It fails if it encounters an 'XML.Eement' or an 'XML.Instruction'.
--
-- 'XML.Comment's are discarded.
--
-- It concatenates the values if there's more than one 'XML.Content' child, strips outer whitespaces, and calls the 'parser' function.
contentAs :: (Text -> Either Text a) -> Schema a
contentAs parser =
  Schema \ElementContent {..} ->
    Bifunctor.first ([],) do
      bitsOfContent <- traverse extractContent ecNodes
      case Foldable.fold bitsOfContent of
        Nothing -> Left NoContent
        Just input -> runParser (Text.strip input)
  where
    extractContent =
      \case
        XML.NodeElement e -> Left (ElementNotContent e)
        XML.NodeInstruction i -> Left (InstructionNotContent i)
        XML.NodeContent c -> Right (Just c)
        XML.NodeComment _ -> Right Nothing

    runParser input =
      Bifunctor.first
        (ContentParsingError . ParsingError input . ParsingErrorMessage)
        (parser input)

-- | Checks that the given root 'XML.Element' has the right name, and if so,
-- feeds its attributes and children nodes to the given 'Schema'.
root :: Text -> Schema a -> XML.Element -> Either (Path, Error) a
root name schema XML.Element {..} =
  if XML.nameLocalName elementName /= name
    then Left (Empty, RootElementNotFound name)
    else Bifunctor.first toPath (applySchema schema (ElementContent elementAttributes elementNodes))
  where
    toPath (segments, err) =
      (Root name segments, err)

-- | Ensures that there's one and only one element for the given name,
-- applies the schema to its attributes and children
requiredElement :: Text -> Schema a -> Schema a
requiredElement name schema =
  Schema \elementContent ->
    case childrenElements name schema elementContent of
      [] -> Left ([], ElementNotFound name)
      [(_, result)] -> Bifunctor.first (Bifunctor.first (prependSingleSegment name)) result
      _ -> Left ([], MoreThanOneElement name)

-- | Ensures that there's at most one element for the given name.
-- If one exists, applies the schema to its attributes and children
element :: Text -> Schema a -> Schema (Maybe a)
element name schema =
  Schema \elementContent ->
    case childrenElements name schema elementContent of
      [] -> Right Nothing
      [(_, result)] -> Bifunctor.bimap (Bifunctor.first (prependSingleSegment name)) Just result
      _ -> Left ([], MoreThanOneElement name)

-- | Extract all children with the given name
elements :: Text -> Schema a -> Schema [a]
elements name schema =
  Schema (traverse toResult . childrenElements name schema)
  where
    toResult (ix, result) =
      Bifunctor.first (Bifunctor.first (prependIndexedSegment ix name)) result

childrenElements :: Text -> Schema a -> ElementContent -> [(Int, Either (Segments, Error) a)]
childrenElements name schema =
  zipWith applySchema' [1 ..] . Maybe.mapMaybe extractElements . ecNodes
  where
    extractElements =
      \case
        XML.NodeElement XML.Element {..}
          | XML.nameLocalName elementName == name -> Just (ElementContent elementAttributes elementNodes)
          | otherwise -> Nothing
        XML.NodeInstruction _ -> Nothing
        XML.NodeContent _ -> Nothing
        XML.NodeComment _ -> Nothing

    applySchema' ix elementContent =
      (ix, applySchema schema elementContent)
