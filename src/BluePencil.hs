module BluePencil where

import Data.Map (Map)
import Data.Text (Text)
import GHC.Natural
import Data.Vector (Vector)
-- import qualified Network.HTTP.Types as HTTP

-- ""
-- {"entityMap":{},"blocks":[{"key":"6s9ko","text":"","type":"unstyled","depth":0,"inlineStyleRanges":[],"entityRanges":[]}]}
-- <p><br></p>

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

data Style =
    Bold
  | Italic
  | Strikethrough
  -- A Header can be styled like code, for example
  | CodeStyle
  deriving (Eq, Show)

data StyleRange =
  StyleRange {
    styleOffset :: Natural
  , styleLength :: Natural
  , style :: Style
  } deriving (Eq, Show)

data EntityRange =
  EntityRange {
    entityOffset :: Natural
  , entityLength :: Natural
  , entityKey :: Natural
  } deriving (Eq, Show)

data Block =
  Block {
    blockKey :: Text
  , blockText :: Text
  , blockType :: BlockType
  , blockDepth :: Natural
  , entityRanges :: Vector EntityRange
  , inlineStyleRanges :: Vector StyleRange
  } deriving (Eq, Show)

data EntityType =
    Link
  | Blah
  deriving (Eq, Show)

data EntityData =
    URL Text -- HTTP.URL
  | Woot
  deriving (Eq, Show)

data Mutability =
    Mutable
  | Immutable
  deriving (Eq, Show)

data Entity =
  Entity {
    entityType :: EntityType
  , entityMutability :: Mutability
  , entityData :: EntityData
  } deriving (Eq, Show)

data ContentRaw =
  ContentRaw {
    entityMap :: Map Text Entity
  , blocks :: Vector Block
  } deriving (Eq, Show)
