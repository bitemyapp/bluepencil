{-# LANGUAGE RecordWildCards #-}

-- | This module is intended to be used as a qualified import.

module BluePencil where

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.BufferBuilder as BB
import qualified Data.ByteString as BS
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.MonoTraversable (omapM_, ofoldlM)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Natural
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

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
  | Image
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

styleRangeToHtml :: Text -> StyleRange -> (Natural, Html)
styleRangeToHtml t StyleRange{..} = undefined
  -- where
  --   sortByStyle s s' = compare (style s) (style s')
  --   sortedList = sortBy sortByStyle $ V.toList inlineStyleRanges

-- {"entityMap":{},"blocks":[{"key":"8n4dj","text":"bold bolditalic italic","type":"unstyled","depth":0,
-- "inlineStyleRanges":[{"offset":0,"length":16,"style":"BOLD"},
--                      {"offset":5,"length":17,"style":"ITALIC"}],"entityRanges":[]}]}
-- <p><strong>bold </strong><em><strong>bolditalic </strong></em><em>italic</em></p>


-- ofoldlM (\n c -> BB.appendChar8 c >> return (n+1)) 0 ("Blah" :: Text)

-- Prelude> renderHtml $ H.b "bold" >> H.em "italic" >> H.b (H.em "bolditalic")
-- "<b>bold</b><em>italic</em><b><em>bolditalic</em></b>"
-- Prelude> renderHtml $  (H.b $ H.text "bold " >> H.i "bolditalic") >> H.i " italic"
-- "<b>bold <i>bolditalic</i></b><i> italic</i>"

-- {"entityMap":{},"blocks":[{"key":"8n4dj","text":"All Bold but this is Italic","type":"unstyled","depth":0,"inlineStyleRanges":[{"offset":0,"length":27,"style":"BOLD"},{"offset":13,"length":14,"style":"ITALIC"}],"entityRanges":[]}]}
-- <p><strong>All Bold but </strong><em><strong>this is Italic</strong></em></p>

codeBlock =
  Block {blockKey = "3vpca",
         blockText = "bold italic bolditalic",
         blockType = CodeBlock,
         blockDepth = 0, -- almost exclusively for lists at the moment
         inlineStyleRanges = V.fromList
                              [StyleRange {styleOffset = 0, styleLength = 4, style = Bold},
                               StyleRange {styleOffset = 12, styleLength = 10, style = Bold},
                               StyleRange {styleOffset = 5, styleLength = 6, style = Italic},
                               StyleRange {styleOffset = 12, styleLength = 10, style = Italic}],
         entityRanges = V.fromList []}

htmlBlock :: Block -> Html
htmlBlock Block{..} = wrapBlock blockType (H.text blockText)

wrapBlock :: BlockType -> Html -> Html
wrapBlock Unstyled content = H.p content
wrapBlock UnorderedListItem content = H.li content
wrapBlock OrderedListItem content = H.li content
wrapBlock Blockquote content = H.blockquote content
wrapBlock HeaderOne content = H.h1 content
wrapBlock HeaderTwo content = H.h2 content
wrapBlock HeaderThree content = H.h3 content
wrapBlock CodeBlock content = H.pre (H.code content)

-- data TagAction =
--     Open
--   | Close
--   deriving (Eq, Show)

-- data BufferAction =
--   BufferAction { actionStyle :: Style
--                , openOrClose :: TagAction }
--   deriving (Eq, Show)

-- updateMap :: (Ord k) => k -> BufferAction -> Map k (NE.NonEmpty BufferAction) -> Map k (NE.NonEmpty BufferAction)
-- updateMap k v m = if M.member k m
--                   then M.adjust (<> (singletonNE v)) k m
--                   else M.insert k (singletonNE v) m

-- singletonNE v = v NE.:| []

-- type ActionsMap = M.Map Natural (NE.NonEmpty BufferAction)

-- injectStyleRange :: StyleRange -> ActionsMap -> ActionsMap
-- injectStyleRange StyleRange{..} m = withClose
--   where start = styleOffset
--         terminate = styleOffset + styleLength
--         withOpen = updateMap start (BufferAction style Open) m
--         withClose = updateMap terminate (BufferAction style Open) withOpen

-- buildIndices :: Block -> ActionsMap
-- buildIndices Block{..} =
--   foldr injectStyleRange mempty inlineStyleRanges

-- baToTag :: BufferAction -> BS.ByteString
-- baToTag (BufferAction s ta) = styleToTag ta s

-- styleToTag :: TagAction -> Style -> BS.ByteString
-- styleToTag Open Bold = "<b>"
-- styleToTag Close Bold = "</b>"
-- styleToTag Open Italic = "<em>"
-- styleToTag Close Italic = "</em>"
-- styleToTag Open Strikethrough = "<del>"
-- styleToTag Close Strikethrough = "</del>"
-- styleToTag Open CodeStyle = "<pre><code>"
-- styleToTag Close CodeStyle = "</code></pre>"

-- accumulateFromBlock :: ActionsMap -> Text -> BB.BufferBuilder ()
-- accumulateFromBlock = undefined

-- blockTypeTags :: BlockType -> [(BS.ByteString, BS.ByteString)]
-- blockTypeTags Unstyled = ["<p>"]
-- blockTypeTags UnorderedListItem = ["<li>"]
-- blockTypeTags OrderedListItem = ["<li>"]
-- blockTypeTags Blockquote = ["<blockquote>"]
-- blockTypeTags HeaderOne = ["<h1>"]
-- blockTypeTags HeaderTwo = ["<h2>"]
-- blockTypeTags HeaderThree = ["<h3>"]
-- blockTypeTags CodeBlock = ["<code>", "<pre>"]
-- blockTypeTags Unstyled = [("<p>", "</p>")]

-- closeTag :: BS.ByteString -> BS.ByteString
-- closeTag "<p>" = "</p>"

-- wrapContent :: BS.ByteString -> [BS.ByteString] -> BB.BufferBuilder ()
-- wrapContent wrapped xs = do
--   BB.appendBS begin
--   BB.appendBS wrapped
--   BB.appendBS end

-- blockToHtml :: ActionsMap -> Block -> BB.BufferBuilder ()
-- blockToHtml m Block{..} = do
--   let wrapperTags = blockTypeTags blockType
--   _ <- traverse (wrapContent (TE.encodeUtf8 blockText)) wrapperTags
--   return ()

-- contentToHtml :: ContentRaw -> BS.ByteString
-- contentToHtml = undefined

-- testBB = BB.runBufferBuilder $ do
--   BB.appendBS "http"
--   BB.appendChar8 ':'
--   BB.appendBS "//"

-- {"entityMap":{"0":{"type":"LINK","mutability":"MUTABLE","data":{"url":"http://www.google.com"}}},"blocks":[{"key":"8n4dj","text":"Italic BoldItalic Google","type":"blockquote","depth":0,"inlineStyleRanges":[{"offset":0,"length":18,"style":"ITALIC"},{"offset":7,"length":11,"style":"BOLD"}],"entityRanges":[{"offset":18,"length":6,"key":0}]}]}
-- <blockquote><em>Italic </em><em><strong>BoldItalic </strong></em><a href="http://www.google.com">Google</a></blockquote>

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
--         matchingIndex sr = (eitherTrue . indexMatchesStyleRange n sr)
