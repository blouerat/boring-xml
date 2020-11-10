{-# LANGUAGE TupleSections #-}

module Boring.XML.Schema where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.XML as XML

data Schema a = Schema
  { applySchema :: ElementContent -> Either (Path, Error) a
  }

data ElementContent = ElementContent
  { ecAttributes :: Map XML.Name Text,
    ecNodes :: [XML.Node]
  }

newtype Path = Path [Text]
  deriving newtype (Eq)

instance Show Path where
  show = Text.unpack . showPath

appendSegment :: Text -> Path -> Path
appendSegment segment (Path segments) =
  Path (segment : segments)

pathSegments :: Path -> [Text]
pathSegments (Path segments) =
  reverse segments

showPath :: Path -> Text
showPath =
  foldMap (Text.cons '/') . pathSegments

data Error
  = ElementNotContent XML.Element
  | InstructionNotContent XML.Instruction
  | NoContent
  | ContentParsingError ParsingError
  | InvalidRootElement XML.Element
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
root name schema rootElement@XML.Element {..} =
  Bifunctor.first (Bifunctor.first (appendSegment name)) $
    if XML.nameLocalName elementName /= name
      then Left ((Path []), InvalidRootElement rootElement)
      else applySchema schema (ElementContent elementAttributes elementNodes)
