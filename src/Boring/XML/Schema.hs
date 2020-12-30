{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Boring.XML.Schema where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
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
  deriving (Eq, Show)

data Segment
  = SingleSegment XML.Name
  | IndexedSegment Int XML.Name
  deriving (Eq, Show)

type Segments = [Segment]

prependSegment :: Segment -> Either (Segments, Error) a -> Either (Segments, Error) a
prependSegment segment =
  Bifunctor.first (Bifunctor.first (segment :))

prependSingleSegment :: XML.Name -> Either (Segments, Error) a -> Either (Segments, Error) a
prependSingleSegment segment =
  prependSegment (SingleSegment segment)

prependIndexedSegment :: Int -> XML.Name -> Either (Segments, Error) a -> Either (Segments, Error) a
prependIndexedSegment ix segment =
  prependSegment (IndexedSegment ix segment)

data Path
  = Empty
  | Root XML.Name Segments
  deriving (Eq)

instance Show Path where
  show = Text.unpack . showPath

showPath :: Path -> Text
showPath path =
  "/" <> Text.intercalate "/" (showSegments path)
  where
    showSegments =
      \case
        Empty -> []
        Root rootSegment childrenSegments -> showName rootSegment : fmap showSegment childrenSegments

    showSegment =
      \case
        SingleSegment segment -> showName segment
        IndexedSegment ix segment -> showName segment <> "[" <> Text.pack (show ix) <> "]"

    showName XML.Name {..} =
      foldMap showNamespace nameNamespace <> nameLocalName

    showNamespace namespace =
      "{" <> namespace <> "}"

data Error
  = ElementNotContent XML.Name
  | InstructionNotContent InstructionTarget
  | NoContent
  | ContentParsingError ParsingError
  | RootElementNotFound XML.Name
  | ElementNotFound XML.Name
  | MoreThanOneElement XML.Name
  | AttributeParsingError XML.Name ParsingError
  | AttributeNotFound XML.Name
  deriving stock (Eq, Show)

newtype InstructionTarget
  = InstructionTarget Text
  deriving newtype (Eq, Show)

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
        XML.NodeElement XML.Element {..} -> Left (ElementNotContent elementName)
        XML.NodeInstruction XML.Instruction {..} -> Left (InstructionNotContent (InstructionTarget instructionTarget))
        XML.NodeContent c -> Right (Just c)
        XML.NodeComment _ -> Right Nothing

    runParser input =
      Bifunctor.first
        (ContentParsingError . ParsingError input . ParsingErrorMessage)
        (parser input)

-- | Checks that the given root 'XML.Element' has the right name, and if so,
-- feeds its attributes and children nodes to the given 'Schema'.
root :: XML.Name -> Schema a -> XML.Element -> Either (Path, Error) a
root name schema xmlElement =
  case elementContentWithName name xmlElement of
    Nothing -> Left (Empty, RootElementNotFound name)
    Just elementContent -> Bifunctor.first toPath (applySchema schema elementContent)
  where
    toPath (segments, err) =
      (Root name segments, err)

-- | Ensures that there's one and only one element for the given name,
-- applies the schema to its attributes and children
requiredElement :: XML.Name -> Schema a -> Schema a
requiredElement name schema =
  Schema \elementContent ->
    case childrenElements name schema elementContent of
      [] -> Left ([], ElementNotFound name)
      [(_, result)] -> prependSingleSegment name result
      _ -> Left ([], MoreThanOneElement name)

-- | Ensures that there's at most one element for the given name.
-- If one exists, applies the schema to its attributes and children
element :: XML.Name -> Schema a -> Schema (Maybe a)
element name schema =
  Schema \elementContent ->
    case childrenElements name schema elementContent of
      [] -> Right Nothing
      [(_, result)] -> Just <$> prependSingleSegment name result
      _ -> Left ([], MoreThanOneElement name)

-- | Extract all children with the given name
elements :: XML.Name -> Schema a -> Schema [a]
elements name schema =
  Schema (traverse toResult . childrenElements name schema)
  where
    toResult (ix, result) =
      prependIndexedSegment ix name result

-- | Extract all children with the given name, fail if none are found
elements1 :: XML.Name -> Schema a -> Schema (NonEmpty a)
elements1 name schema =
  Schema \elementContent ->
    traverse toResult (childrenElements name schema elementContent) >>= \case
      [] -> Left ([], ElementNotFound name)
      hd : tl -> Right (hd :| tl)
  where
    toResult (ix, result) =
      prependIndexedSegment ix name result

childrenElements :: XML.Name -> Schema a -> ElementContent -> [(Int, Either (Segments, Error) a)]
childrenElements name schema =
  zipWith applySchema' [1 ..] . Maybe.mapMaybe extractElements . ecNodes
  where
    extractElements =
      \case
        XML.NodeElement xmlElement -> elementContentWithName name xmlElement
        XML.NodeInstruction _ -> Nothing
        XML.NodeContent _ -> Nothing
        XML.NodeComment _ -> Nothing

    applySchema' ix elementContent =
      (ix, applySchema schema elementContent)

elementContentWithName :: XML.Name -> XML.Element -> Maybe ElementContent
elementContentWithName name XML.Element {..} =
  if elementName == name
    then Just (ElementContent elementAttributes elementNodes)
    else Nothing

-- | Optionally parse an attribute
attribute :: XML.Name -> (Text -> Either Text a) -> Schema (Maybe a)
attribute name parser =
  Schema \ElementContent {..} ->
    traverse runParser (Map.lookup name ecAttributes)
  where
    runParser input =
      Bifunctor.first
        (([],) . AttributeParsingError name . ParsingError input . ParsingErrorMessage)
        (parser input)

-- | Parse a required attribute
requiredAttribute :: XML.Name -> (Text -> Either Text a) -> Schema a
requiredAttribute name parser =
  Schema \elementContent ->
    applySchema (attribute name parser) elementContent >>= \case
      Nothing -> Left ([], AttributeNotFound name)
      Just value -> Right value
