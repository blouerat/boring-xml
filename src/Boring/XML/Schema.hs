{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Boring.XML.Schema where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.String (IsString, fromString)
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
  = SingleSegment NamePredicate
  | IndexedSegment Int NamePredicate
  deriving (Eq, Show)

type Segments = [Segment]

prependSegment :: Segment -> Either (Segments, Error) a -> Either (Segments, Error) a
prependSegment segment =
  Bifunctor.first (Bifunctor.first (segment :))

prependSingleSegment :: NamePredicate -> Either (Segments, Error) a -> Either (Segments, Error) a
prependSingleSegment segment =
  prependSegment (SingleSegment segment)

prependIndexedSegment :: Int -> NamePredicate -> Either (Segments, Error) a -> Either (Segments, Error) a
prependIndexedSegment ix segment =
  prependSegment (IndexedSegment ix segment)

data Path
  = Empty
  | Root NamePredicate Segments
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
        Root rootSegment childrenSegments -> showNamePredicate rootSegment : fmap showSegment childrenSegments

    showSegment =
      \case
        SingleSegment segment -> showNamePredicate segment
        IndexedSegment ix segment -> showNamePredicate segment <> "[" <> Text.pack (show ix) <> "]"

data Error
  = ElementNotContent XML.Element
  | InstructionNotContent XML.Instruction
  | NoContent
  | ContentParsingError ParsingError
  | RootElementNotFound NamePredicate
  | ElementNotFound NamePredicate
  | MoreThanOneElement NamePredicate
  deriving stock (Eq, Show)

data ParsingError = ParsingError
  { eInput :: Text,
    eMessage :: ParsingErrorMessage
  }
  deriving stock (Eq, Show)

newtype ParsingErrorMessage
  = ParsingErrorMessage Text
  deriving newtype (Eq, Show)

newtype LocalName
  = LocalName Text
  deriving (Eq)

data NamePredicate
  = NameOnly LocalName
  | WithNamespace Text LocalName
  | WithPrefix Text LocalName
  deriving (Eq)

instance IsString NamePredicate where
  fromString = NameOnly . LocalName . Text.pack

instance Show NamePredicate where
  show = Text.unpack . showNamePredicate

showNamePredicate :: NamePredicate -> Text
showNamePredicate =
  \case
    NameOnly (LocalName localName) -> localName
    WithNamespace namespace (LocalName localName) -> "{" <> namespace <> "}" <> localName
    WithPrefix prefix (LocalName localName) -> prefix <> ":" <> localName

elementWithName :: NamePredicate -> XML.Element -> Maybe ElementContent
elementWithName namePredicate XML.Element {..} =
  case namePredicate of
    NameOnly (LocalName localName)
      | XML.nameLocalName elementName == localName && Maybe.isNothing (XML.nameNamespace elementName) -> Just elementContent
    WithNamespace namespace (LocalName localName)
      | XML.nameLocalName elementName == localName && XML.nameNamespace elementName == Just namespace -> Just elementContent
    WithPrefix prefix (LocalName localName)
      | XML.nameLocalName elementName == localName && XML.namePrefix elementName == Just prefix -> Just elementContent
    _ -> Nothing
  where
    elementContent =
      ElementContent elementAttributes elementNodes

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
root :: NamePredicate -> Schema a -> XML.Element -> Either (Path, Error) a
root namePredicate schema xmlElement =
  case elementWithName namePredicate xmlElement of
    Nothing -> Left (Empty, RootElementNotFound namePredicate)
    Just elementContent -> Bifunctor.first toPath (applySchema schema elementContent)
  where
    toPath (segments, err) =
      (Root namePredicate segments, err)

-- | Ensures that there's one and only one element for the given name,
-- applies the schema to its attributes and children
requiredElement :: NamePredicate -> Schema a -> Schema a
requiredElement namePredicate schema =
  Schema \elementContent ->
    case childrenElements namePredicate schema elementContent of
      [] -> Left ([], ElementNotFound namePredicate)
      [(_, result)] -> prependSingleSegment namePredicate result
      _ -> Left ([], MoreThanOneElement namePredicate)

-- | Ensures that there's at most one element for the given name.
-- If one exists, applies the schema to its attributes and children
element :: NamePredicate -> Schema a -> Schema (Maybe a)
element namePredicate schema =
  Schema \elementContent ->
    case childrenElements namePredicate schema elementContent of
      [] -> Right Nothing
      [(_, result)] -> Just <$> prependSingleSegment namePredicate result
      _ -> Left ([], MoreThanOneElement namePredicate)

-- | Extract all children with the given name
elements :: NamePredicate -> Schema a -> Schema [a]
elements namePredicate schema =
  Schema (traverse toResult . childrenElements namePredicate schema)
  where
    toResult (ix, result) =
      prependIndexedSegment ix namePredicate result

-- | Extract all children with the given name, fail if none are found
elements1 :: NamePredicate -> Schema a -> Schema (NonEmpty a)
elements1 namePredicate schema =
  Schema \elementContent ->
    traverse toResult (childrenElements namePredicate schema elementContent) >>= \case
      [] -> Left ([], ElementNotFound namePredicate)
      hd : tl -> Right (hd :| tl)
  where
    toResult (ix, result) =
      prependIndexedSegment ix namePredicate result

childrenElements :: NamePredicate -> Schema a -> ElementContent -> [(Int, Either (Segments, Error) a)]
childrenElements namePredicate schema =
  zipWith applySchema' [1 ..] . Maybe.mapMaybe extractElements . ecNodes
  where
    extractElements =
      \case
        XML.NodeElement xmlElement -> elementWithName namePredicate xmlElement
        XML.NodeInstruction _ -> Nothing
        XML.NodeContent _ -> Nothing
        XML.NodeComment _ -> Nothing

    applySchema' ix elementContent =
      (ix, applySchema schema elementContent)
