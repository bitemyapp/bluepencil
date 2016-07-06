{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | This module is intended to be used as a qualified import.

module BluePencil
  ( ContentRaw(..)
  , Block(..)
  , BlockType(..)

  , Style(..)

  , Entity(..)
  , EntityType(..)
  , EntityData(..)
  , Mutability(..)

  , StyleRange(..)
  , EntityRange(..)
  , StyleRangeAbsolute(..)
  , EntityRangeAbsolute(..)

  , buildContent
  , makeEmptyContent
  , renderContent )
  where

import           Data.Aeson
import qualified Data.BufferBuilder            as BB
import qualified Data.ByteString               as BS
import           Data.Foldable                 (traverse_)
import           Data.List                     (partition, sortBy)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import           Data.MonoTraversable          (ofoldlM)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           GHC.Natural

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
  | Plain
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
  , style       :: Style
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
  , entityKey    :: Natural
  } deriving (Eq, Show)

instance FromJSON EntityRange where
  parseJSON = withObject "EntityRange" parse
    where parse o = EntityRange <$> o .: "offset"
                                <*> o .: "length"
                                <*> o .: "key"


data Block =
  Block {
    blockKey          :: Text
  , blockText         :: Text
  , blockType         :: BlockType
  , blockDepth        :: Natural
  , inlineStyleRanges :: Vector StyleRange
  , entityRanges      :: Vector EntityRange
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
          parse _ = fail "Expected a String \"LINK\" or \"IMAGE\" for BluePencil.EntityType"


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
    entityType       :: EntityType
  , entityMutability :: Mutability
  , entityData       :: EntityData
  } deriving (Eq, Show)

instance FromJSON Entity where
  parseJSON = withObject "Entity" parse
    where parse o = Entity <$> o .: "type"
                           <*> o .: "mutability"
                           <*> o .: "data"

hrefToOpenAnchor :: Text -> BS.ByteString
hrefToOpenAnchor uri =
  BS.concat ["<a href=\"",
             TE.encodeUtf8 uri,
             "\">"]

srcToOpenImg :: Text -> BS.ByteString
srcToOpenImg uri =
  BS.concat ["<img src=",
             "\"", TE.encodeUtf8 uri, "\"",
             ">"]

entityToTags :: Entity -> (BS.ByteString, BS.ByteString)
entityToTags (Entity Link _ (EntityData uri)) = (hrefToOpenAnchor uri, "</a>")
entityToTags (Entity Image _ (EntityData uri)) = (srcToOpenImg uri, "")

data ContentRaw =
  ContentRaw {
    entityMap :: Map Text Entity
  , blocks    :: Vector Block
  } deriving (Eq, Show)

makeEmptyContent :: Text -> ContentRaw
makeEmptyContent initialBlockKey =
  ContentRaw M.empty
  (V.fromList [Block initialBlockKey "" Unstyled 0 (V.fromList []) (V.fromList [])])

instance FromJSON ContentRaw where
  parseJSON = withObject "ContentRaw" parse
    where parse o = ContentRaw <$> o .: "entityMap"
                               <*> o .: "blocks"

sraCompare :: StyleRangeAbsolute -> StyleRangeAbsolute -> Ordering
sraCompare (StyleRangeAbsolute i j _) (StyleRangeAbsolute i' j' _) =
  case compare i i' of
    EQ -> compare j j'
    x -> x

eraCompare :: EntityRangeAbsolute -> EntityRangeAbsolute -> Ordering
eraCompare (EntityRangeAbsolute i j _) (EntityRangeAbsolute i' j' _) =
  case compare i i' of
    EQ -> compare j j'
    x -> x

data TagRange =
    StyleTR  StyleRangeAbsolute
  | EntityTR EntityRangeAbsolute
  deriving (Eq, Show)

trCompare :: TagRange -> TagRange -> Ordering
trCompare (StyleTR sr) (StyleTR sr') = sraCompare sr sr'
trCompare (EntityTR er) (EntityTR er') = eraCompare er er'
trCompare (EntityTR _) (StyleTR _) = GT
trCompare (StyleTR _) (EntityTR _) = LT

data EntityRangeAbsolute =
  EntityRangeAbsolute {
    eraStart  :: Natural
  , eraStop   :: Natural
  , eraEntity :: Entity
  } deriving (Eq, Show)

data StyleRangeAbsolute =
  StyleRangeAbsolute {
    sraStart :: Natural
  , sraStop  :: Natural
  , sraStyle :: Style
  } deriving (Eq, Show)

srToSra :: StyleRange -> StyleRangeAbsolute
srToSra StyleRange{..} =
  StyleRangeAbsolute styleOffset (styleLength + styleOffset) style

erToEra :: Map Text Entity -> EntityRange -> Maybe EntityRangeAbsolute
erToEra entityMap EntityRange{..} =
      EntityRangeAbsolute entityOffset (entityOffset + entityLength)
  <$> entityFromKey
  where entityFromKey = M.lookup (T.pack (show entityKey)) entityMap

splitOverlaps' :: [TagRange] -> [TagRange]
splitOverlaps' [] = []
splitOverlaps' (x : xs) =
  x : (splitOverlaps' merged)
  where overlapT :: ([TagRange], [TagRange])
        overlapT = partition (checkOverlapTR x) xs
        overlaps = fst overlapT
        nonOverlaps = snd overlapT
        -- splitPoint = trStop x
        splits = concatMap (splitOverlap' x) overlaps
        merged = sortBy trCompare $ splits ++ nonOverlaps

splitOverlap' :: TagRange -> TagRange -> [TagRange]
splitOverlap' (EntityTR (EntityRangeAbsolute _ j _)) (EntityTR (EntityRangeAbsolute i' j' s')) =
  [split1, split2]
  where split1 = EntityTR $ EntityRangeAbsolute i' j s'
        split2 = EntityTR $ EntityRangeAbsolute j j' s'
splitOverlap' (StyleTR (StyleRangeAbsolute _ j _)) (StyleTR (StyleRangeAbsolute i' j' s')) =
  [split1, split2]
  where split1 = StyleTR $ StyleRangeAbsolute i' j s'
        split2 = StyleTR $ StyleRangeAbsolute j j' s'
splitOverlap' (EntityTR (EntityRangeAbsolute i j e)) (StyleTR (StyleRangeAbsolute _ j' s')) =
  [split1, split2, split3]
  where split1 = EntityTR $ EntityRangeAbsolute i j e
        split2 = StyleTR $ StyleRangeAbsolute j j' s'
        split3 = StyleTR $ StyleRangeAbsolute i j s'
splitOverlap' (StyleTR (StyleRangeAbsolute i j s)) (EntityTR (EntityRangeAbsolute _ j' e')) =
  [split1, split2, split3]
  where split1 = StyleTR $ StyleRangeAbsolute i j s
        split2 = EntityTR $ EntityRangeAbsolute j j' e'
        split3 = EntityTR $ EntityRangeAbsolute i j e'

trStart :: TagRange -> Natural
trStart (EntityTR (EntityRangeAbsolute start _ _)) = start
trStart (StyleTR (StyleRangeAbsolute start _ _)) = start

trStop :: TagRange -> Natural
trStop (EntityTR (EntityRangeAbsolute _ stop _)) = stop
trStop (StyleTR (StyleRangeAbsolute _ stop _)) = stop

trMatches :: Natural -> TagRange -> Maybe TagAction
trMatches n tr =
  case ((trStart tr) == n, (trStop tr) == n) of
    (True, _) -> Just Open
    (_, True) -> Just Close
    (_, _)    -> Nothing

accumIfMatchTR :: Natural
               -> TagRange
               -> ([TagRange], [TagRange])
               -> ([TagRange], [TagRange])
accumIfMatchTR idx tr (xs, ys) =
  case trMatches idx tr of
    Just Open  -> (tr : xs, ys)
    Just Close -> (xs, tr : ys)
    Nothing    -> (xs, ys)

-- requires sorted xs, cf. splitXs
getOpenAndCloseTags' :: [TagRange]
                     -> Natural
                     -> ([TagRange], [TagRange])
getOpenAndCloseTags' xs idx =
  foldr (accumIfMatchTR idx) ([], []) xs

data TagAction = Open | Close

styleToTag :: TagAction -> Style -> BS.ByteString
styleToTag Open Bold = "<b>"
styleToTag Close Bold = "</b>"
styleToTag Open Italic = "<em>"
styleToTag Close Italic = "</em>"
styleToTag Open Strikethrough = "<del>"
styleToTag Close Strikethrough = "</del>"
styleToTag Open CodeStyle = "<pre><code>"
styleToTag Close CodeStyle = "</code></pre>"
styleToTag _ Plain         = ""

taToTupleGetter :: TagAction -> (a, a) -> a
taToTupleGetter Open (a, _) = a
taToTupleGetter Close (_, a) = a

entityToTag :: TagAction -> Entity -> BS.ByteString
entityToTag ta e = taToTupleGetter ta (entityToTags e)

tagRangeToTag :: TagAction -> TagRange -> BS.ByteString
tagRangeToTag ta (StyleTR (StyleRangeAbsolute _ _ style)) =
  styleToTag ta style
tagRangeToTag ta (EntityTR (EntityRangeAbsolute _ _ entity)) =
  entityToTag ta entity

addTag' :: TagAction -> TagRange -> BB.BufferBuilder ()
addTag' ta tr = BB.appendBS (tagRangeToTag ta tr)

drainTags' :: [TagRange]
          -> Natural
          -> Char
          -> BB.BufferBuilder Natural
drainTags' xs n c = do
  _ <- traverse (addTag' Close) sortedClose
  _ <- traverse (addTag' Open) sortedOpen
  BB.appendChar8 c
  return (n + 1)
  where (open, close) = getOpenAndCloseTags' xs n
        sortedOpen = reverse open
        sortedClose = reverse close

appendBlockContent' :: [TagRange] -> Text -> BB.BufferBuilder ()
appendBlockContent' xs t = do
  _ <- ofoldlM (\n c -> drainTags' xs n c) 0 t
  return ()

buildBlock' :: Map Text Entity -> Block -> BB.BufferBuilder ()
buildBlock' entityMap Block{..} = do
  BB.appendBS open
  appendBlockContent' absoluteRanges blockText
  BB.appendBS close
  BB.appendBS "\n" -- for humans.
  where absoluteStyles = fmap srToSra (V.toList inlineStyleRanges)
        absoluteEntities = catMaybes (fmap (erToEra entityMap) (V.toList entityRanges))
        absoluteRanges :: [TagRange]
        absoluteRanges = sortBy trCompare $ splitOverlaps' $ (fmap StyleTR absoluteStyles ++ fmap EntityTR absoluteEntities)
        (open, close) = blockTypeTags blockType

blockTypeTags :: BlockType -> (BS.ByteString, BS.ByteString)
blockTypeTags Unstyled          = ("<p>", "</p>")
blockTypeTags UnorderedListItem = ("<ul>", "</ul>")
blockTypeTags OrderedListItem   = ("<ol>", "</ol>")
blockTypeTags Blockquote        = ("<blockquote>", "</blockquote>")
blockTypeTags HeaderOne         = ("<h1>", "</h1>")
blockTypeTags HeaderTwo         = ("<h2>", "</h2>")
blockTypeTags HeaderThree       = ("<h3>", "</h3>")
blockTypeTags CodeBlock         = ("<pre><code>", "</code></pre>")


buildContent :: ContentRaw -> BB.BufferBuilder ()
buildContent ContentRaw{..} =
  traverse_ (buildBlock' entityMap) blocks

renderContent :: ContentRaw -> BS.ByteString
renderContent cr =
  BB.runBufferBuilder (buildContent cr)

overlapping :: (Natural, Natural) -> (Natural, Natural) -> Bool
overlapping (l, h) (l', h') =
  l < h' && l' < h

-- (0, 16) (5, 15) <--- first dominates second, this is false
-- (0, 16) (5, 17) <--- second extends outside first, this is true
overlapNotDominated :: (Natural, Natural) -> (Natural, Natural) -> Bool
overlapNotDominated i@(_, h) i'@(_, h') =
  (overlapping i i') && h < h'

rangeTupleTR :: TagRange -> (Natural, Natural)
rangeTupleTR (StyleTR (StyleRangeAbsolute i j _)) = (i, j)
rangeTupleTR (EntityTR (EntityRangeAbsolute i j _)) = (i, j)

checkOverlapTR :: TagRange -> TagRange -> Bool
checkOverlapTR tr tr' =
  overlapNotDominated (rangeTupleTR tr) (rangeTupleTR tr')
