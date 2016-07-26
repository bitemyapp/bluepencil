{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module is intended to be used as a qualified import.

module BluePencil where
  -- ( ContentRaw(..)
  -- , Block(..)
  -- , BlockType(..)

  -- , Style(..)

  -- , Entity(..)
  -- , EntityType(..)
  -- , EntityData(..)
  -- , Mutability(..)

  -- , StyleRange(..)
  -- , EntityRange(..)
  -- , StyleRangeAbsolute(..)
  -- , EntityRangeAbsolute(..)

  -- , buildContent
  -- , makeEmptyContent
  -- , renderHtml
  -- , renderPlainText
  -- )
  -- where

import           Control.Monad        (void)
import           Data.Aeson
import qualified Data.BufferBuilder   as BB
import qualified Data.ByteString      as BS
import           Data.Foldable        (traverse_)
import           Data.List            (partition, sortBy)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes)
import           Data.MonoTraversable (ofoldlM)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           GHC.Natural

data BlockType =
    Unstyled
  | UnorderedListItem
  | OrderedListItem
  | Blockquote
  | HeaderOne
  | HeaderTwo
  | HeaderThree
  | HeaderFour
  | HeaderFive
  | HeaderSix
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
  parseJSON (String "header-four") = return HeaderFour
  parseJSON (String "header-five") = return HeaderFive
  parseJSON (String "header-six") = return HeaderSix
  parseJSON (String "code-block") = return CodeBlock
  parseJSON _ = fail "Expected one of several possible strings for BluePencil.BlockType"

instance ToJSON BlockType where
  toJSON Unstyled = String "unstyled"
  toJSON UnorderedListItem = String "unordered-list-item"
  toJSON OrderedListItem = String "ordered-list-item"
  toJSON Blockquote = String "blockquote"
  toJSON HeaderOne = String "header-one"
  toJSON HeaderTwo = String "header-two"
  toJSON HeaderThree = String "header-three"
  toJSON HeaderFour = String "header-four"
  toJSON HeaderFive = String "header-five"
  toJSON HeaderSix = String "header-six"
  toJSON CodeBlock = String "code-block"

data Style =
    Bold
  | Italic
  | Strikethrough
  -- A Header can be styled like code, for example
  | CodeStyle
  | Underline
  deriving (Eq, Ord, Show)

instance FromJSON Style where
  parseJSON (String "BOLD") = return Bold
  parseJSON (String "ITALIC") = return Italic
  parseJSON (String "STRIKETHROUGH") = return Strikethrough
  parseJSON (String "CODE") = return CodeStyle
  parseJSON (String "UNDERLINE") = return Underline
  parseJSON _ = fail "Expected one of several possible strings for BluePencil.Style"

instance ToJSON Style where
  toJSON Bold = String "BOLD"
  toJSON Italic = String "ITALIC"
  toJSON Strikethrough = String "STRIKETHROUGH"
  toJSON CodeStyle = String "CODE"
  toJSON Underline = String "UNDERLINE"

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

instance ToJSON StyleRange where
  toJSON (StyleRange offset length' style) =
    object [ "offset" .= offset
           , "length" .= length'
           , "style" .= style ]

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

instance ToJSON EntityRange where
  toJSON (EntityRange offset length' key) =
    object [ "offset" .= offset
           , "length" .= length'
           , "key" .= key ]

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

instance ToJSON Block where
  toJSON (Block blockKey blockText blockType blockDepth inlineStyleRanges entityRanges) =
    object [ "key" .= blockKey
           , "text" .= blockText
           , "type" .= blockType
           , "depth" .= blockDepth
           , "inlineStyleRanges" .= inlineStyleRanges
           , "entityRanges" .= entityRanges
           ]


data EntityType =
    Link
  | Image
  deriving (Eq, Show)

instance FromJSON EntityType where
  parseJSON = withText "EntityType" parse
    where parse "LINK" = return Link
          parse "IMAGE" = return Image
          parse _ = fail "Expected a String \"LINK\" or \"IMAGE\" for BluePencil.EntityType"

instance ToJSON EntityType where
  toJSON Link = String "LINK"
  toJSON Image = String "IMAGE"

data EntityData =
  EntityData { entityDataUrl :: Text }
  deriving (Eq, Show)

instance FromJSON EntityData where
  parseJSON = withObject "EntityData" parse
    where parse o = EntityData <$> o .: "url"

instance ToJSON EntityData where
  toJSON (EntityData entityDataUrl) =
    object ["url" .= entityDataUrl]

data Mutability =
    Mutable
  | Immutable
  deriving (Eq, Show)

instance FromJSON Mutability where
  parseJSON = withText "Mutability" parse
    where parse "MUTABLE" = return Mutable
          parse "IMMUTABLE" = return Immutable
          parse _ = fail "Expected MUTABLE or IMMUTABLE for BluePencil.Mutability"

instance ToJSON Mutability where
  toJSON Mutable = String "MUTABLE"
  toJSON Immutable = String "IMMUTABLE"

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

instance ToJSON Entity where
  toJSON (Entity entityType mutability entityData) =
    object [ "type" .= entityType
           , "mutability" .= mutability
           , "data" .= entityData ]

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

instance ToJSON ContentRaw where
  toJSON (ContentRaw entityMap blocks) =
    object [ "entityMap" .= entityMap
           , "blocks"    .= blocks
           ]

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

-- original: <li><pre><code>asdas</code></pre>d</li>
-- -1: <li><pre><code>asdasd</li>

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
styleToTag Open Underline = "<u>"
styleToTag Close Underline = "</u>"

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

getFinalTagRanges :: [TagRange] -> Text -> [TagRange]
getFinalTagRanges trXs t =
  filter (\tr -> trStop tr == textLength) trXs
  where textLength = fromIntegral (T.length t)

foldBlockContent :: [TagRange] -> Text -> BB.BufferBuilder ()
foldBlockContent xs t = do
  void $ ofoldlM (\n c -> drainTags' xs n c) 0 t
  traverse_ (addTag' Close) (getFinalTagRanges xs t)

appendBlockContent' :: [TagRange] -> Text -> BB.BufferBuilder ()
appendBlockContent' xs t =
  if T.null t
  then BB.appendBS "<br/>"
  else foldBlockContent xs t

-- appendBlockContent' [StyleTR (StyleRangeAbsolute {sraStart = 0, sraStop = 6, sraStyle = CodeStyle})] "asdasd"
-- foldBlockContent [StyleTR (StyleRangeAbsolute {sraStart = 0, sraStop = 6, sraStyle = CodeStyle})] "asdasd"
-- BB.runBufferBuilder $ foldBlockContent [StyleTR (StyleRangeAbsolute {sraStart = 0, sraStop = 6, sraStyle = CodeStyle})] "asdasd"

buildBlock :: Map Text Entity -> Block -> BB.BufferBuilder ()
buildBlock entityMap Block{..} = do
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
blockTypeTags UnorderedListItem = ("<li>", "</li>")
blockTypeTags OrderedListItem   = ("<li>", "</li>")
blockTypeTags Blockquote        = ("<blockquote>", "</blockquote>")
blockTypeTags HeaderOne         = ("<h1>", "</h1>")
blockTypeTags HeaderTwo         = ("<h2>", "</h2>")
blockTypeTags HeaderThree       = ("<h3>", "</h3>")
blockTypeTags HeaderFour        = ("<h4>", "</h4>")
blockTypeTags HeaderFive        = ("<h5>", "</h5>")
blockTypeTags HeaderSix         = ("<h6>", "</h6>")
blockTypeTags CodeBlock         = ("<pre><code>", "</code></pre>")

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

-- data ActiveBlockType =
--     ActiveUnordered Natural
--   | ActiveOrdered Natural
--   deriving (Eq, Show)

-- getActiveDepth :: ActiveBlockType -> Natural
-- getActiveDepth (ActiveUnordered nat) = nat
-- getActiveDepth (ActiveOrdered nat) = nat

-- getActiveTags :: ActiveBlockType -> (BS.ByteString, BS.ByteString)
-- getActiveTags (ActiveUnordered _) = ("</ul>", "<ul>")
-- getActiveTags (ActiveOrdered _) = ("</ol>", "<ol>")

-- blockToActive :: ActiveBlockType -> [ActiveBlockType]
-- blockToActive Block{..} xs = case blockType of
--   UnorderedListItem -> ActiveUnordered blockDepth : xs
--   OrderedListItem -> ActiveOrdered blockDepth : xs
--   _ -> xs

-- pairBarrier :: ActiveBlockType -> ActiveBlockType -> (BS.ByteString, BS.ByteString)
-- pairBarrier prev next =
--   case compare prevDepth nextDepth of
--     EQ -> (fst (getActiveTags prev), snd (getActiveTags next))
--     LT -> ("", snd (getActiveTags next))
--     GT -> (fst (getActiveTags prev), "")
--   where prevDepth = getActiveDepth prev
--         nextDepth = getActiveDepth next

-- data BlockDepthTraversal =
--     Same
--   | Up
--   | Down
--   deriving (Eq, Show)

-- blockDepthChange :: Natural -> Block -> BlockDepthTraversal
-- blockDepthChange prev next =
--   case compare (blockDepth prev) (blockDepth next) of
--     EQ -> Same -- </ol><ul>
--     LT -> Down -- <ul>
--     GT -> Up -- </ol>

-- blockBarrier :: Block -> Block -> (BS.ByteString, BS.ByteString)
-- blockBarrier prev next = undefined

-- blockStartEnd :: [ActiveBlockType]
--               -> ActiveBlockType
--               -> (BS.ByteString, [ActiveBlockType])
-- blockStartEnd [] next = ( snd (getActiveTags next)
--                         , [next] )
-- blockStartEnd all@(prev:xs) next =
--   case compare prevDepth nextDepth of
--     -- </ol><ul>
--     EQ -> ( BS.concat [fst (getActiveTags prev), snd (getActiveTags next)]
--           , next : xs )
--     -- <ul>
--     LT -> ( BS.concat [snd (getActiveTags next)]
--           , next : all )
--     -- </ol>
--     GT -> ( BS.concat [fst (getActiveTags prev), snd (getActiveTags next)]
--           , next : xs )
--   where prevDepth = getActiveDepth prev
--         nextDepth = getActiveDepth next

-- blah :: [a] -> BS.ByteString
-- blah (x:xs) = ""
-- blah [] = ""
-- blah [] = ""

-- data BlockContext = { activeTypes :: S.Set ActiveBlockType
--                     , previousBlock ::

buildContent' :: ContentRaw -> BB.BufferBuilder ()
buildContent' ContentRaw{..} =
  void $ ofoldlM (\s block -> buildBlock entityMap block >> return s) S.empty blocks

renderHtml :: ContentRaw -> BS.ByteString
renderHtml cr =
  BB.runBufferBuilder (buildContent' cr)

-- buildContent :: ContentRaw -> BB.BufferBuilder ()
-- buildContent ContentRaw{..} =
--   ofoldlM (\_ block -> buildBlock entityMap block) () blocks

-- renderHtml :: ContentRaw -> BS.ByteString
-- renderHtml cr =
--   BB.runBufferBuilder (buildContent cr)

renderPlainText :: ContentRaw -> Text
renderPlainText cr = T.unlines fragments
  where fragments :: [Text]
        fragments = V.foldr (\Block{..} xs -> blockText : xs) [] (blocks cr)
