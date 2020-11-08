module Boring.XML.Schema where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.XML as XML

data Error
  = ElementNotContent XML.Element
  | InstructionNotContent XML.Instruction
  | NoContent
  | ContentParsingError ParsingError
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
contentAs :: (Text -> Either Text a) -> XML.Element -> Either Error a
contentAs parser XML.Element {..} =
  do
    bitsOfContent <- traverse extractContent elementNodes
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
