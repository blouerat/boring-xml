module Main where

import qualified Boring.XML.Schema as Schema
import qualified Data.Text as Text
import qualified Text.XML as XML

main :: IO ()
main = do
  XML.Document {..} <- XML.readFile XML.def "example.xml"
  let schema =
        Schema.requiredElement "message"
          . Schema.requiredElement "value"
          $ Schema.contentAs Right
  putStrLn case Schema.root "example" schema documentRoot of
    Left (path, err) -> "Invalid XML: " <> show err <> " @ " <> show path
    Right value -> Text.unpack value
