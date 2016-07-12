{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import BluePencil
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.DeriveTH
import qualified Data.Map as M
import Data.Monoid
import Data.Proxy
import Data.Text.Arbitrary ()
import Data.Typeable
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck           (prop)
import Test.QuickCheck
import Text.RawString.QQ

main :: IO ()
main = hspec $ do
  describe "plain text rendering" $ do
    it "should render empty content" $ do
      renderPlainText emptyContent `shouldBe` "\n"
    it "should render example content" $ do
      renderPlainText exampleContent `shouldBe` "bold italic bolditalic\nstrikethrough\ncode example\nunordered list item\nordered list item\nblockquote\nhttp://bitemyapp.com\nHeading large\nHeading medium\nHeading small\nCode block\n\n"
  describe "Valid JSON inputs should parse correctly" $ do
    it "Empty JSON should produce empty content" $ do
      decode emptyJSON `shouldBe` Just emptyContent
    it "Example JSON should produce the example content" $ do
      decode exampleJSON `shouldBe` Just exampleContent
  describe "Rendering raw content value should produce the correct HTML" $ do
    it "contentHtmlTest should be the correct HTML" $ do
      renderHtml contentHtmlTest `shouldBe` expectedHtml
  describe "JSON instances" $ do
    it "should have isomorphism for small, simple examples" $ do
      decode (encode emptyContent) `shouldBe` Just emptyContent
  describe "JSON instances should have isomorphism" $ do
      propJSON (Proxy :: Proxy BlockType)
      propJSON (Proxy :: Proxy EntityRange)
      propJSON (Proxy :: Proxy EntityData)
      propJSON (Proxy :: Proxy Style)
      propJSON (Proxy :: Proxy EntityType)
      propJSON (Proxy :: Proxy StyleRange)
      propJSON (Proxy :: Proxy Mutability)
      propJSON (Proxy :: Proxy Block)
      propJSON (Proxy :: Proxy Entity)
      propJSON (Proxy :: Proxy ContentRaw)

emptyHTML :: String
emptyHTML = ""

emptyJSON :: BL.ByteString
emptyJSON = [r|
{
  "entityMap":{
  },
  "blocks":[
    {
      "key":"6s9ko",
      "text":"",
      "type":"unstyled",
      "depth":0,
      "inlineStyleRanges":[],
      "entityRanges":[]
    }
  ]
}
|]

emptyContent :: ContentRaw
emptyContent =
  makeEmptyContent "6s9ko"

exampleHTML :: String
exampleHTML = [r|
<p><strong>bold</strong> <em>italic</em> <em><strong>bolditalic</strong></em></p>
<p><del>strikethrough</del></p>
<p><code>code example</code></p>
<ul>
  <li>unordered list item</li>
</ul>
<ol>
  <li>ordered list item</li>
</ol>
<blockquote>blockquote</blockquote>
<p><a href="http://bitemyapp.com">http://bitemyapp.com</a></p>
<h1>Heading large</h1>
<h2>Heading medium</h2>
<h3>Heading small</h3>
<pre><code>Code block</code></pre>
<p><br></p>
|]

exampleJSON :: BL.ByteString
exampleJSON = [r|
{
    "entityMap": {
        "0": {
            "type": "LINK",
            "mutability": "MUTABLE",
            "data": {
                "url": "http://bitemyapp.com"
            }
        }
    },
    "blocks": [
        {
            "key": "3vpca",
            "text": "bold italic bolditalic",
            "type": "unstyled",
            "depth": 0,
            "inlineStyleRanges": [
                {
                    "offset": 0,
                    "length": 4,
                    "style": "BOLD"
                },
                {
                    "offset": 12,
                    "length": 10,
                    "style": "BOLD"
                },
                {
                    "offset": 5,
                    "length": 6,
                    "style": "ITALIC"
                },
                {
                    "offset": 12,
                    "length": 10,
                    "style": "ITALIC"
                }
            ],
            "entityRanges": []
        },
        {
            "key": "3916r",
            "text": "strikethrough",
            "type": "unstyled",
            "depth": 0,
            "inlineStyleRanges": [
                {
                    "offset": 0,
                    "length": 13,
                    "style": "STRIKETHROUGH"
                }
            ],
            "entityRanges": []
        },
        {
            "key": "4o5l0",
            "text": "code example",
            "type": "unstyled",
            "depth": 0,
            "inlineStyleRanges": [
                {
                    "offset": 0,
                    "length": 12,
                    "style": "CODE"
                }
            ],
            "entityRanges": []
        },
        {
            "key": "bqhv1",
            "text": "unordered list item",
            "type": "unordered-list-item",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "70a5r",
            "text": "ordered list item",
            "type": "ordered-list-item",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "e93jn",
            "text": "blockquote",
            "type": "blockquote",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "elsqh",
            "text": "http://bitemyapp.com",
            "type": "unstyled",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": [
                {
                    "offset": 0,
                    "length": 20,
                    "key": 0
                }
            ]
        },
        {
            "key": "7te3b",
            "text": "Heading large",
            "type": "header-one",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "1jm2o",
            "text": "Heading medium",
            "type": "header-two",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "5r31e",
            "text": "Heading small",
            "type": "header-three",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "5nagf",
            "text": "Code block",
            "type": "code-block",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        },
        {
            "key": "cuil",
            "text": "",
            "type": "unstyled",
            "depth": 0,
            "inlineStyleRanges": [],
            "entityRanges": []
        }
    ]
}
|]

exampleContent :: ContentRaw
exampleContent = ContentRaw {entityMap = M.fromList [("0",Entity {entityType = Link, entityMutability = Mutable, entityData = EntityData {entityDataUrl = "http://bitemyapp.com"}})], blocks = [Block {blockKey = "3vpca", blockText = "bold italic bolditalic", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = [StyleRange {styleOffset = 0, styleLength = 4, style = Bold},StyleRange {styleOffset = 12, styleLength = 10, style = Bold},StyleRange {styleOffset = 5, styleLength = 6, style = Italic},StyleRange {styleOffset = 12, styleLength = 10, style = Italic}], entityRanges = []},Block {blockKey = "3916r", blockText = "strikethrough", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = [StyleRange {styleOffset = 0, styleLength = 13, style = Strikethrough}], entityRanges = []},Block {blockKey = "4o5l0", blockText = "code example", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = [StyleRange {styleOffset = 0, styleLength = 12, style = CodeStyle}], entityRanges = []},Block {blockKey = "bqhv1", blockText = "unordered list item", blockType = UnorderedListItem, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "70a5r", blockText = "ordered list item", blockType = OrderedListItem, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "e93jn", blockText = "blockquote", blockType = Blockquote, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "elsqh", blockText = "http://bitemyapp.com", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = [], entityRanges = [EntityRange {entityOffset = 0, entityLength = 20, entityKey = 0}]},Block {blockKey = "7te3b", blockText = "Heading large", blockType = HeaderOne, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "1jm2o", blockText = "Heading medium", blockType = HeaderTwo, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "5r31e", blockText = "Heading small", blockType = HeaderThree, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "5nagf", blockText = "Code block", blockType = CodeBlock, blockDepth = 0, inlineStyleRanges = [], entityRanges = []},Block {blockKey = "cuil", blockText = "", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = [], entityRanges = []}]}

contentHtmlTest :: ContentRaw
contentHtmlTest = ContentRaw {entityMap = M.fromList [("0",Entity {entityType = Link, entityMutability = Mutable, entityData = EntityData {entityDataUrl = "http://google.com/"}})], blocks = V.fromList [Block {blockKey = "djceq", blockText = "bold bolditalic GoogleURI italic boldAgain plain", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = V.fromList [StyleRange {styleOffset = 0, styleLength = 15, style = Bold}, StyleRange {styleOffset = 33, styleLength = 9, style = Bold},StyleRange {styleOffset = 5, styleLength = 27, style = Italic}], entityRanges = V.fromList [EntityRange {entityOffset = 16, entityLength = 9, entityKey = 0}]}, Block {blockKey = "1tuas", blockText = "Second block", blockType = Unstyled, blockDepth = 0, inlineStyleRanges = V.fromList [], entityRanges = V.fromList []}]}

expectedHtml :: BS.ByteString
expectedHtml = [r|<p><b>bold <em>bolditalic</em></b><em> <a href="http://google.com/">GoogleURI</a> italic</em> <b>boldAgain</b> plain</p>
<p>Second block</p>
|]

-- ApproxEq a,
-- ==~
propJSON :: forall a . (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a) => Proxy a -> Spec
propJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
  in counterexample jsonStr (parseEither parseJSON (toJSON a) == Right a)
  where testName = show ty <> " FromJSON/ToJSON roundtrips"
        ty = typeOf (undefined :: a)

-- this is the saddest arbitrary instance I have ever written
instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary

$(derive makeArbitrary ''BlockType)
$(derive makeArbitrary ''EntityRange)
$(derive makeArbitrary ''EntityData)
$(derive makeArbitrary ''Style)
$(derive makeArbitrary ''EntityType)
$(derive makeArbitrary ''StyleRange)
$(derive makeArbitrary ''Mutability)
$(derive makeArbitrary ''Block)
$(derive makeArbitrary ''Entity)
$(derive makeArbitrary ''ContentRaw)
