# boring-xml

`boring-xml` is a **set of combinators** carefully crafted for **extracting data from XML documents**.

It is **not a parsing library**, it is instead built on top of [`xml-conduit`](https://www.stackage.org/lts/package/xml-conduit), specifically around [`Text.XML.Element`](https://www.stackage.org/haddock/lts/xml-conduit/Text-XML.html#t:Element).

It is an alternative to [`Text.XML.Cursor`](https://www.stackage.org/haddock/lts/xml-conduit/Text-XML-Cursor.html) with a focus on **simplicity, return types, and error handling**.
