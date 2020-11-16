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
  { applySchema :: ElementContent -> Either (Path, Error) a
  }
  deriving (Functor, Applicative) via (Reader.ReaderT ElementContent (Either (Path, Error)))

failure :: Error -> Either (Path, Error) a
failure =
  Left . (Path [],)

applySchema' :: Schema a -> XML.Element -> Either (Path, Error) a
applySchema' schema XML.Element {..} =
  prependElementNameToPath . applySchema schema $ elementContent
  where
    prependElementNameToPath =
      Bifunctor.first prependElementNameSegment

    prependElementNameSegment (Path segments, err) =
      (Path (elementLocalName : segments), err)

    elementLocalName =
      XML.nameLocalName elementName

    elementContent =
      ElementContent elementAttributes elementNodes

data ElementContent = ElementContent
  { ecAttributes :: Map XML.Name Text,
    ecNodes :: [XML.Node]
  }

newtype Path = Path
  { pathSegments :: [Text]
  }
  deriving newtype (Eq)

instance Show Path where
  show = Text.unpack . showPath

showPath :: Path -> Text
showPath =
  ("/" <>) . Text.intercalate "/" . pathSegments

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
    Bifunctor.first (Path [],) do
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
root name schema xmlElement@XML.Element {..} =
  if XML.nameLocalName elementName /= name
    then failure (RootElementNotFound name)
    else applySchema' schema xmlElement

-- | Ensures that there's one and only one element for the given name,
-- applies the schema to its attributes and children
requiredElement :: Text -> Schema a -> Schema a
requiredElement name schema =
  Schema \elementContent ->
    case childrenElements name elementContent of
      [] -> failure (ElementNotFound name)
      [oneElement] -> applySchema' schema oneElement
      _ -> failure (MoreThanOneElement name)

-- | Ensures that there's at most one element for the given name.
-- If one exists, applies the schema to its attributes and children
element :: Text -> Schema a -> Schema (Maybe a)
element name schema =
  Schema \elementContent ->
    case childrenElements name elementContent of
      [] -> Right Nothing
      [oneElement] -> Just <$> applySchema' schema oneElement
      _ -> failure (MoreThanOneElement name)

childrenElements :: Text -> ElementContent -> [XML.Element]
childrenElements name ElementContent {..} =
  Maybe.mapMaybe extractElements ecNodes
  where
    extractElements =
      \case
        XML.NodeElement xmlElement@XML.Element {..}
          | XML.nameLocalName elementName == name -> Just xmlElement
          | otherwise -> Nothing
        XML.NodeInstruction _ -> Nothing
        XML.NodeContent _ -> Nothing
        XML.NodeComment _ -> Nothing
