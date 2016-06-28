{-# LANGUAGE RecordWildCards #-}

-- | This module is intended to be used as a qualified import.

module BluePencil where

import Control.Monad (foldM)
import Data.Aeson
import qualified Data.BufferBuilder as BB
import qualified Data.ByteString as BS
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Natural

-- import qualified Network.HTTP.Types as HTTP

-- ""
-- {"entityMap":{},"blocks":[{"key":"6s9ko","text":"","type":"unstyled","depth":0,"inlineStyleRanges":[],"entityRanges":[]}]}
-- <p><br></p>

-- emptyJSON = [r|
-- {
--   "entityMap":{
--   },
--   "blocks":[
--     {
--       "key":"6s9ko",
--       "text":"",
--       "type":"unstyled",
--       "depth":0,
--       "inlineStyleRanges":[],
--       "entityRanges":[]
--     }
--   ]
-- }
-- |]

-- exampleJSON = [r|
-- {
--     "entityMap": {
--         "0": {
--             "type": "LINK",
--             "mutability": "MUTABLE",
--             "data": {
--                 "url": "http://bitemyapp.com"
--             }
--         }
--     },
--     "blocks": [
--         {
--             "key": "3vpca",
--             "text": "bold italic bolditalic",
--             "type": "unstyled",
--             "depth": 0,
--             "inlineStyleRanges": [
--                 {
--                     "offset": 0,
--                     "length": 4,
--                     "style": "BOLD"
--                 },
--                 {
--                     "offset": 12,
--                     "length": 10,
--                     "style": "BOLD"
--                 },
--                 {
--                     "offset": 5,
--                     "length": 6,
--                     "style": "ITALIC"
--                 },
--                 {
--                     "offset": 12,
--                     "length": 10,
--                     "style": "ITALIC"
--                 }
--             ],
--             "entityRanges": []
--         },
--         {
--             "key": "3916r",
--             "text": "strikethrough",
--             "type": "unstyled",
--             "depth": 0,
--             "inlineStyleRanges": [
--                 {
--                     "offset": 0,
--                     "length": 13,
--                     "style": "STRIKETHROUGH"
--                 }
--             ],
--             "entityRanges": []
--         },
--         {
--             "key": "4o5l0",
--             "text": "code example",
--             "type": "unstyled",
--             "depth": 0,
--             "inlineStyleRanges": [
--                 {
--                     "offset": 0,
--                     "length": 12,
--                     "style": "CODE"
--                 }
--             ],
--             "entityRanges": []
--         },
--         {
--             "key": "bqhv1",
--             "text": "unordered list item",
--             "type": "unordered-list-item",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "70a5r",
--             "text": "ordered list item",
--             "type": "ordered-list-item",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "e93jn",
--             "text": "blockquote",
--             "type": "blockquote",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "elsqh",
--             "text": "http://bitemyapp.com",
--             "type": "unstyled",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": [
--                 {
--                     "offset": 0,
--                     "length": 20,
--                     "key": 0
--                 }
--             ]
--         },
--         {
--             "key": "7te3b",
--             "text": "Heading large",
--             "type": "header-one",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "1jm2o",
--             "text": "Heading medium",
--             "type": "header-two",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "5r31e",
--             "text": "Heading small",
--             "type": "header-three",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "5nagf",
--             "text": "Code block",
--             "type": "code-block",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         },
--         {
--             "key": "cuil",
--             "text": "",
--             "type": "unstyled",
--             "depth": 0,
--             "inlineStyleRanges": [],
--             "entityRanges": []
--         }
--     ]
-- }
-- |]

-- <ol>
--   <li>First List Item</li>
--   <li><a href="http://www.google.com">Google</a></li>
-- </ol>

-- {"entityMap":{"0":{"type":"LINK","mutability":"MUTABLE","data":{"url":"http://www.google.com"}}},"blocks":[{"key":"296mn","text":"First List Item","type":"ordered-list-item","depth":0,"inlineStyleRanges":[],"entityRanges":[]},{"key":"4kk8e","text":"Google","type":"ordered-list-item","depth":0,"inlineStyleRanges":[],"entityRanges":[{"offset":0,"length":6,"key":0}]}]}


data BlockType =
    Unstyled
  | UnorderedListItem
  | OrderedListItem
  | Blockquote
  | HeaderOne
  | HeaderTwo
  | HeaderThree
  | CodeBlock
  deriving (Eq, Show)

instance FromJSON BlockType where
  parseJSON (String "unstyled") = return Unstyled
  parseJSON (String "unordered-list-item") = return UnorderedListItem
  parseJSON (String "ordered-list-item") = return OrderedListItem
  parseJSON (String "blockquote") = return Blockquote
  parseJSON (String "header-one") = return HeaderOne
  parseJSON (String "header-two") = return HeaderTwo
  parseJSON (String "header-three") = return HeaderThree
  parseJSON (String "code-block") = return CodeBlock
  parseJSON _ = fail "Expected one of several possible strings for BluePencil.BlockType"


data Style =
    Bold
  | Italic
  | Strikethrough
  -- A Header can be styled like code, for example
  | CodeStyle
  deriving (Eq, Ord, Show)

instance FromJSON Style where
  parseJSON (String "BOLD") = return Bold
  parseJSON (String "ITALIC") = return Italic
  parseJSON (String "STRIKETHROUGH") = return Strikethrough
  parseJSON (String "CODE") = return CodeStyle
  parseJSON _ = fail "Expected one of several possible strings for BluePencil.Style"


data StyleRange =
  StyleRange {
    styleOffset :: Natural
  , styleLength :: Natural
  , style :: Style
  } deriving (Eq, Show)

instance FromJSON StyleRange where
  parseJSON = withObject "StyleRange" parse
    where parse o = StyleRange <$> o .: "offset"
                               <*> o .: "length"
                               <*> o .: "style"

data EntityRange =
  EntityRange {
    entityOffset :: Natural
  , entityLength :: Natural
  , entityKey :: Natural
  } deriving (Eq, Show)

instance FromJSON EntityRange where
  parseJSON = withObject "EntityRange" parse
    where parse o = EntityRange <$> o .: "offset"
                                <*> o .: "length"
                                <*> o .: "key"


data Block =
  Block {
    blockKey :: Text
  , blockText :: Text
  , blockType :: BlockType
  , blockDepth :: Natural
  , inlineStyleRanges :: Vector StyleRange
  , entityRanges :: Vector EntityRange
  } deriving (Eq, Show)

instance FromJSON Block where
  parseJSON = withObject "Block" parse
    where parse o = Block <$> o .: "key"
                          <*> o .: "text"
                          <*> o .: "type"
                          <*> o .: "depth"
                          <*> o .: "inlineStyleRanges"
                          <*> o .: "entityRanges"


data EntityType =
  Link
  deriving (Eq, Show)

instance FromJSON EntityType where
  parseJSON = withText "EntityType" parse
    where parse "LINK" = return Link
          parse _ = fail "Expected a String \"LINK\" for BluePencil.EntityType"


data EntityData =
  EntityData { entityDataUrl :: Text }
  deriving (Eq, Show)

instance FromJSON EntityData where
  parseJSON = withObject "EntityData" parse
    where parse o = EntityData <$> o .: "url"


data Mutability =
    Mutable
  | Immutable
  deriving (Eq, Show)

instance FromJSON Mutability where
  parseJSON = withText "Mutability" parse
    where parse "MUTABLE" = return Mutable
          parse "IMMUTABLE" = return Immutable
          parse _ = fail "Expected MUTABLE or IMMUTABLE for BluePencil.Mutability"


data Entity =
  Entity {
    entityType :: EntityType
  , entityMutability :: Mutability
  , entityData :: EntityData
  } deriving (Eq, Show)

instance FromJSON Entity where
  parseJSON = withObject "Entity" parse
    where parse o = Entity <$> o .: "type"
                           <*> o .: "mutability"
                           <*> o .: "data"

data ContentRaw =
  ContentRaw {
    entityMap :: Map Text Entity
  , blocks :: Vector Block
  } deriving (Eq, Show)

makeEmptyContent :: Text -> ContentRaw
makeEmptyContent initialBlockKey =
  ContentRaw M.empty (V.fromList [Block initialBlockKey "" Unstyled 0 (V.fromList []) (V.fromList [])])

instance FromJSON ContentRaw where
  parseJSON = withObject "ContentRaw" parse
    where parse o = ContentRaw <$> o .: "entityMap"
                               <*> o .: "blocks"


-- ContentRaw / Block to HTML conversion

exampleBlock =
  Block {blockKey = "3vpca",
         blockText = "bold italic bolditalic",
         blockType = Unstyled,
         blockDepth = 0, -- almost exclusively for lists at the moment
         inlineStyleRanges = V.fromList
                              [StyleRange {styleOffset = 0, styleLength = 4, style = Bold},
                               StyleRange {styleOffset = 12, styleLength = 10, style = Bold},
                               StyleRange {styleOffset = 5, styleLength = 6, style = Italic},
                               StyleRange {styleOffset = 12, styleLength = 10, style = Italic}],
         entityRanges = V.fromList []}

data TagAction =
    Open
  | Close
  deriving (Eq, Show)

data BufferAction =
  BufferAction { actionStyle :: Style
               , openOrClose :: TagAction }
  deriving (Eq, Show)

updateMap :: (Ord k) => k -> BufferAction -> Map k (NE.NonEmpty BufferAction) -> Map k (NE.NonEmpty BufferAction)
updateMap k v m = if M.member k m
                  then M.adjust (<> (singletonNE v)) k m
                  else M.insert k (singletonNE v) m

singletonNE v = v NE.:| []

type ActionsMap = M.Map Natural (NE.NonEmpty BufferAction)

injectStyleRange :: StyleRange -> ActionsMap -> ActionsMap
injectStyleRange StyleRange{..} m = withClose
  where start = styleOffset
        terminate = styleOffset + styleLength
        withOpen = updateMap start (BufferAction style Open) m
        withClose = updateMap terminate (BufferAction style Open) withOpen

buildIndices :: Block -> ActionsMap
buildIndices Block{..} =
  foldr injectStyleRange mempty inlineStyleRanges

baToTag :: BufferAction -> BS.ByteString
baToTag (BufferAction s ta) = styleToTag ta s

styleToTag :: TagAction -> Style -> BS.ByteString
styleToTag Open Bold = "<b>"
styleToTag Close Bold = "</b>"
styleToTag Open Italic = "<em>"
styleToTag Close Italic = "</em>"
styleToTag Open Strikethrough = "<del>"
styleToTag Close Strikethrough = "</del>"
styleToTag Open CodeStyle = "<code>"
styleToTag Close CodeStyle = "</code>"

-- accumulateFromBlock :: ActionsMap -> Text -> BB.BufferBuilder ()
-- accumulateFromBlock = undefined

-- blockToHtml :: Block -> BS.ByteString
blockToHtml m Block{..} = undefined

contentToHtml :: ContentRaw -> BS.ByteString
contentToHtml = undefined

testBB = BB.runBufferBuilder $ do
  BB.appendBS "http"
  BB.appendChar8 ':'
  BB.appendBS "//"

-- indexMatchesStyleRange n StyleRange{..} =
--   (n' == styleOffset, n' == (styleOffset + styleLength))
--   where n' = fromInteger (toInteger n)

-- eitherTrue :: (Bool, Bool) -> Bool
-- eitherTrue (True, _) = True
-- eitherTrue (_, True) = True
-- eitherTrue _ = False

-- styleRangeToByteString :: Int -> StyleRange -> BS.ByteString
-- styleRangeToByteString n sr@StyleRange{..} =
--   case indexMatchesStyleRange n sr of
--     (True, _) -> styleToTag Open style
--     (_, True) -> styleToTag Close style
--     (False, False) -> ""
  

-- addIfIndexMatches :: Block -> Int -> StyleRange -> [StyleRange]
-- addIfIndexMatches Block{..} n StyleRange{..} xs =

-- stylesMatchingIndex :: Block -> Int -> [StyleRange]
-- stylesMatchingIndex = undefined

-- blockText = "bold italic bolditalic",
-- addAtIndex :: Block -> Int -> BB.BufferBuilder ()
-- addAtIndex Block{..} n = undefined
--   where curChar = T.index blockText n
--         sortByStyle s s' = compare (style s) (style s')
--         styleRangeList = sortBy sortByStyle $ V.toList inlineStyleRanges
        -- matchingIndex sr = (eitherTrue . indexMatchesStyleRange n sr)
