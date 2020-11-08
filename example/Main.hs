module Main where

import qualified Boring.XML.Schema as Schema
import qualified Data.Text as Text
import qualified Text.XML as XML

main :: IO ()
main = do
  XML.Document {..} <- XML.readFile XML.def "example.xml"
  putStrLn case Schema.contentAs Right documentRoot of
    Left err -> "Invalid XML: " <> show err
    Right value -> Text.unpack value
