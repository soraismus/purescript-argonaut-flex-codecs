-- are all instances of DecodeCases/TypeEquals necessary
-- still too many copies of of json/object error message
-- maybe the heterogenous library pertains to this; is this redundant?
-- 4 cases: 0. gdecode, 1. gdecode leniently, 2. override, 3. override w/ leniency
-- the others can be based on the general case
-- try downgrading Monad to Bind
-- `unsafeCoerce` is used b/c at this point every Builder's src is {}.
-- Use of `unsafeCoerce` is unsightly, of course,
-- so try to generalize the type signature or introduce a bind-like
-- function for record mergers.
module Data.Argonaut.FlexDecode where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (delete, get, insert, merge, union)
import Record.Builder (Builder)
import Record.Builder (build, delete, insert, merge, union) as Builder
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(Proxy))
import Type.Row
  ( kind RowList
  , Cons
  , Nil
  , RProxy(RProxy)
  , class Cons
  , class Lacks
  , class Nub
  , class RowToList
  , class Union
  )
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

_decodeJson :: forall a. DecodeJson a => Proxy a -> Json -> Either String a
_decodeJson _ = decodeJson

reportBuilderJson
  :: forall f r
   . Status f
  => (Object Json -> f (Builder {} (Record r)))
  -> Json
  -> f (Builder {} (Record r))
reportBuilderJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"

getMissingFieldErrorMessage :: String -> String
getMissingFieldErrorMessage fieldName =
  "JSON was missing expected field: " <> fieldName

-- NOTE: I was working on this before I was interrupted,
-- so it might be helpful.
-- ------------------------
-- toStatus :: forall f g a. Alt f => Monad f => Status g => String -> f a -> g a
-- toStatus msg mx =
--   apply (pure identity) mx
--   case mx of
--     Just x -> report x
--     Nothing -> reportError msg

reportJson
  :: forall f r
   . Status f
  => (Object Json -> f (Record r))
  -> Json
  -> f (Record r)
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage

reportObject
  :: forall f l r
   . GDecodeJson r l
  => RowToList r l
  => Status f
  => Object Json
  -> RLProxy l
  -> f (Record r)
reportObject object rlProxy =
  case gDecodeJson object rlProxy of
    Left errorStr -> reportError errorStr
    Right record -> report record

-- | -------------------------------------------------------------------------

class BuilderFlexGDecodeJson
  f
  (row :: # Type)
  (list :: RowList)
  | list -> row where
  builderFlexGDecodeJson
    :: Object Json
    -> RLProxy list
    -> f (Builder {} (Record row))

instance builderFlexGDecodeJsonNil
  :: Status f
  => BuilderFlexGDecodeJson f () Nil where
  builderFlexGDecodeJson _ _ = report identity

instance builderFlexGDecodeJsonCons
  :: ( Alternative f
     , Cons field (f value) rowTail row
     , DecodeJson value
     , BuilderFlexGDecodeJson g rowTail tail
     , IsSymbol field
     , Lacks field rowTail
     , Monad g
     , Status g
     )
  => BuilderFlexGDecodeJson g row (Cons field (f value) tail) where
  builderFlexGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy field
    let fieldName = reflectSymbol sProxy
    rest <- builderFlexGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val -> report $ Builder.insert sProxy (pure val) <<< rest
      Nothing ->
        report $ Builder.insert sProxy empty <<< rest

class
  ( RowToList r0 l0
  , RowToList r1 l1
  ) <=
  BuilderFlexDecodeJson
    (f :: Type -> Type)
    (r0 :: # Type)
    (l0 :: RowList)
    (r1 :: # Type)
    (l1 :: RowList)
    | l0 -> r0, l1 -> r1 where
    builderFlexDecodeJson'
      :: RLProxy l0
      -> RLProxy l1
      -> Json
      -> f (Builder (Record r0) (Record r1))

instance builderFlexDecodeJsonBuilder
  :: ( BuilderFlexGDecodeJson f row list
     , Monad f
     , RowToList row list
     , Status f
     )
  => BuilderFlexDecodeJson f () Nil row list where
  builderFlexDecodeJson' _ _ =
    reportBuilderJson
      (flip builderFlexGDecodeJson (RLProxy :: RLProxy list))

builderFlexDecodeJson
  :: forall f list0 list1 list2 row0 row1 row2
   . BuilderFlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => RProxy row0
  -> Json
  -> f (Builder {} (Record row2))
builderFlexDecodeJson _ = reportBuilderJson go
  where
  go object = do
    builder0 <- builderFlexGDecodeJson object (RLProxy :: RLProxy list0)
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.merge record1) <<< builder0

builderFlexDecodeJson_
  :: forall f list0 list1 list2 row0 row1 row2
   . BuilderFlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => RProxy row0
  -> Json
  -> f (Builder {} (Record row2))
builderFlexDecodeJson_ _ = reportBuilderJson go
  where
  go object = do
    builder0 <- builderFlexGDecodeJson object (RLProxy :: RLProxy list0)
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.union record1) <<< builder0

class BuilderDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  __builderDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Builder {} (Record r0))

instance __builderDecodeJsonWithNil
  :: Status f
  => BuilderDecodeJsonWith_ f Nil () Nil () where
  __builderDecodeJsonWith _ _ _ _ = report identity

instance __builderDecodeJsonWithCons
  :: ( Cons field value row' row
     , Cons field decoderValue decoderRow' decoderRow
     , DecodeCases f decoderList row
     , DecodeCases f decoderList' row'
     , BuilderDecodeJsonWith_ f decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , Monad f
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status f
     , TypeEquals decoderValue (Json -> f value)
     )
  => BuilderDecodeJsonWith_
       f
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field value list')
       row
  where
  __builderDecodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f value
      decoder = to $ get sProxy decoderRecord

    rest <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        (delete sProxy decoderRecord)
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

class
  ( DecodeCases f l1 r0
  , RowToList r1 l1
  ) <=
  BuilderDecodeJsonWith
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    builderDecodeJsonWith'
      :: Record r1
      -> Json
      -> f (Builder {} (Record r0))

instance builderDecodeJsonWithDecodeJsonWith_
  :: ( DecodeCases f l1 r0
     , BuilderDecodeJsonWith_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => BuilderDecodeJsonWith f l1 r1 r0
  where
  builderDecodeJsonWith' decoderRecord =
    reportBuilderJson $
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord

builderDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . BuilderDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderDecodeJsonWith decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    builder0 <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.merge record1) <<< builder0

builderDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . BuilderDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderDecodeJsonWith_ decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    builder0 <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.union record1) <<< builder0

class BuilderFlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  __builderFlexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Builder {} (Record r0))

instance __builderFlexDecodeJsonWithNil
  :: Status f
  => BuilderFlexDecodeJsonWith_ f Nil () Nil () where
  __builderFlexDecodeJsonWith _ _ _ _ = report identity

instance __builderFlexDecodeJsonWithCons
  :: ( Alternative f
     , Cons field (f value) row' row
     , Cons field decoderValue decoderRow' decoderRow
     , FlexDecodeCases g decoderList row
     , FlexDecodeCases g decoderList' row'
     , BuilderFlexDecodeJsonWith_ g decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , Monad g
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status g
     , TypeEquals decoderValue (Json -> g (f value))
     )
  => BuilderFlexDecodeJsonWith_
       g
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field (f value) list')
       row
  where
  __builderFlexDecodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> g (f value)
      decoder = to $ get sProxy decoderRecord

    rest <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        (delete sProxy decoderRecord)
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        report $ Builder.insert sProxy empty <<< rest

class
  ( FlexDecodeCases f l1 r0
  , RowToList r1 l1
  ) <=
  BuilderFlexDecodeJsonWith
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    builderFlexDecodeJsonWith'
      :: Record r1
      -> Json
      -> f (Builder {} (Record r0))

instance builderFlexDecodeJsonWithDecodeJsonWith_
  :: ( FlexDecodeCases f l1 r0
     , BuilderFlexDecodeJsonWith_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => BuilderFlexDecodeJsonWith f l1 r1 r0
  where
  builderFlexDecodeJsonWith' decoderRecord =
    reportBuilderJson $
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord

builderFlexDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . BuilderFlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderFlexDecodeJsonWith decoderRecord = reportBuilderJson $ go decoderRecord
  where
  go :: Record decoderRow -> Object Json -> f (Builder {} (Record row2))
  go decoderRecord object = do
    builder0 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.merge record1) <<< builder0

builderFlexDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . BuilderFlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderFlexDecodeJsonWith_ decoderRecord = reportBuilderJson $ go decoderRecord
  where
  go :: Record decoderRow -> Object Json -> f (Builder {} (Record row2))
  go decoderRecord object = do
    builder0 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ (Builder.union record1) <<< builder0

builderFlexDecodeJsonWithBoth
  :: forall
       decoderList0
       decoderList1
       decoderRow0
       decoderRow1
       f
       intermediateRow
       list0
       list1
       list2
       row0
       row1
       row2
       row3
   . BuilderDecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => BuilderFlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
  => GDecodeJson row2 list2
  => Monad f
  => Nub intermediateRow intermediateRow
  => Nub row3 row3
  => RowToList decoderRow0 decoderList0
  => RowToList decoderRow1 decoderList1
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 intermediateRow
  => Union intermediateRow row2 row3
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Builder {} (Record row3))
builderFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row3))
  go object = do
    (builder0 :: Builder {} (Record row0)) <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
    builder1 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
    let
      builder1' :: Builder (Record row0) (Record intermediateRow)
      builder1' = unsafeCoerce builder1
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    report $ (Builder.merge record2) <<< builder1' <<< builder0

builderFlexDecodeJsonWithBoth_
  :: forall
       decoderList0
       decoderList1
       decoderRow0
       decoderRow1
       f
       intermediateRow
       list0
       list1
       list2
       row0
       row1
       row2
       row3
   . BuilderDecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => BuilderFlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
  => GDecodeJson row2 list2
  => Monad f
  => RowToList decoderRow0 decoderList0
  => RowToList decoderRow1 decoderList1
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 intermediateRow
  => Union intermediateRow row2 row3
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Builder {} (Record row3))
builderFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row3))
  go object = do
    (builder0 :: Builder {} (Record row0)) <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
    builder1 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
    let
      builder1' :: Builder (Record row0) (Record intermediateRow)
      builder1' = unsafeCoerce builder1
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    report $ (Builder.union record2) <<< builder1' <<< builder0

-- | -------------------------------------------------------------------------

class FlexGDecodeJson f (row :: # Type) (list :: RowList) | list -> row where
  flexGDecodeJson :: Object Json -> RLProxy list -> f (Record row)

instance flexGDecodeJsonCons
  :: ( Alternative f
     , Cons field (f value) rowTail row
     , DecodeJson value
     , FlexGDecodeJson g rowTail tail
     , IsSymbol field
     , Lacks field rowTail
     , Monad g
     , Status g
     )
  => FlexGDecodeJson g row (Cons field (f value) tail) where
  flexGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy field
    let fieldName = reflectSymbol sProxy
    rest <- flexGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy (pure val) rest
      Nothing ->
        report $ insert sProxy empty rest

instance flexGDecodeJsonNil
  :: Status f
  => FlexGDecodeJson f () Nil where
  flexGDecodeJson _ _ = report {}

class FlexDecodeJson f a where
  flexDecodeJson' :: Json -> f a

instance flexDecodeJsonRecord
  :: ( FlexGDecodeJson f row list
     , RowToList row list
     , Status f
     )
  => FlexDecodeJson f (Record row) where
  flexDecodeJson' = reportJson $ flip flexGDecodeJson (RLProxy :: RLProxy list)

flexDecodeJson
  :: forall f list0 list1 list2 row0 row1 row2
   . FlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => RProxy row0
  -> Json
  -> f (Record row2)
flexDecodeJson _ = reportJson go
  where
  go object = do
    record0 <- flexGDecodeJson object (RLProxy :: RLProxy list0)
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ merge record0 record1

flexDecodeJson_
  :: forall f list0 list1 list2 row0 row1 row2
   . FlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => RProxy row0
  -> Json
  -> f (Record row2)
flexDecodeJson_ _ = reportJson go
  where
  go object = do
    record0 <- flexGDecodeJson object (RLProxy :: RLProxy list0)
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ union record0 record1

class DecodeCases
  (f :: Type -> Type)
  (list :: RowList)
  (row :: # Type)
  | list -> row

instance decodeCasesCons
  :: ( Cons field value row' row
     , DecodeCases f list' row'
     , TypeEquals decoderValue (Json -> f value)
     )
  => DecodeCases f (Cons field decoderValue list') row

instance decodeCasesNil :: DecodeCases f Nil ()

class DecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  __decodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance __decodeJsonWithNil
  :: Status f
  => DecodeJsonWith_ f Nil () Nil () where
  __decodeJsonWith _ _ _ _ = report {}

instance __decodeJsonWithCons
  :: ( Cons field value row' row
     , Cons field decoderValue decoderRow' decoderRow
     , DecodeCases f decoderList row
     , DecodeCases f decoderList' row'
     , DecodeJsonWith_ f decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , Monad f
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status f
     , TypeEquals decoderValue (Json -> f value)
     )
  => DecodeJsonWith_
       f
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field value list')
       row
  where
  __decodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f value
      decoder = to $ get sProxy decoderRecord

    rest <-
      __decodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        (delete sProxy decoderRecord)
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

class
  ( DecodeCases f l1 r0
  , RowToList r1 l1
  ) <=
  DecodeJsonWith
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    decodeJsonWith'
      :: Record r1
      -> Json
      -> f (Record r0)

instance decodeJsonWithDecodeJsonWith_
  :: ( DecodeCases f l1 r0
     , DecodeJsonWith_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => DecodeJsonWith f l1 r1 r0
  where
  decodeJsonWith' decoderRecord =
    reportJson $
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord

decodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . DecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Record row2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record row2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ merge record0 record1

decodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . DecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Record row2)
decodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record row2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ union record0 record1

class FlexDecodeCases
  (f :: Type -> Type)
  (list :: RowList)
  (row :: # Type)
  | list -> row

instance flexDecodeCasesCons
  :: ( Cons field (f value) row' row
     , FlexDecodeCases g list' row'
     , TypeEquals decoderValue (Json -> g (f value))
     )
  => FlexDecodeCases g (Cons field decoderValue list') row

instance flexDecodeCasesNil :: FlexDecodeCases f Nil ()

class FlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  __flexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance __flexDecodeJsonWithNil
  :: Status f
  => FlexDecodeJsonWith_ f Nil () Nil () where
  __flexDecodeJsonWith _ _ _ _ = report {}

instance __flexDecodeJsonWithCons
  :: ( Alternative f
     , Cons field (f value) row' row
     , Cons field decoderValue decoderRow' decoderRow
     , FlexDecodeCases g decoderList row
     , FlexDecodeCases g decoderList' row'
     , FlexDecodeJsonWith_ g decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , Monad g
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status g
     , TypeEquals decoderValue (Json -> g (f value))
     )
  => FlexDecodeJsonWith_
       g
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field (f value) list')
       row
  where
  __flexDecodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> g (f value)
      decoder = to $ get sProxy decoderRecord

    rest <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        (delete sProxy decoderRecord)
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        report $ insert sProxy empty rest

class
  ( FlexDecodeCases f l1 r0
  , RowToList r1 l1
  ) <=
  FlexDecodeJsonWith
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    flexDecodeJsonWith'
      :: Record r1
      -> Json
      -> f (Record r0)

instance flexDecodeJsonWithDecodeJsonWith_
  :: ( FlexDecodeCases f l1 r0
     , FlexDecodeJsonWith_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => FlexDecodeJsonWith f l1 r1 r0
  where
  flexDecodeJsonWith' decoderRecord =
    reportJson $
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord

flexDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . FlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Record row2)
flexDecodeJsonWith decoderRecord = reportJson $ go decoderRecord
  where
  go :: Record decoderRow -> Object Json -> f (Record row2)
  go decoderRecord object = do
    record0 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ merge record0 record1

flexDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . FlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => Monad f
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Record row2)
flexDecodeJsonWith_ decoderRecord = reportJson $ go decoderRecord
  where
  go :: Record decoderRow -> Object Json -> f (Record row2)
  go decoderRecord object = do
    record0 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    report $ union record0 record1

flexDecodeJsonWithBoth
  :: forall
       decoderList0
       decoderList1
       decoderRow0
       decoderRow1
       f
       intermediateRow
       list0
       list1
       list2
       row0
       row1
       row2
       row3
   . DecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => FlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
  => GDecodeJson row2 list2
  => Monad f
  => Nub intermediateRow intermediateRow
  => Nub row3 row3
  => RowToList decoderRow0 decoderList0
  => RowToList decoderRow1 decoderList1
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 intermediateRow
  => Union intermediateRow row2 row3
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Record row3)
flexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record row3)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
    record1 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    report $ record0 `merge` record1 `merge` record2

flexDecodeJsonWithBoth_
  :: forall
       decoderList0
       decoderList1
       decoderRow0
       decoderRow1
       f
       intermediateRow
       list0
       list1
       list2
       row0
       row1
       row2
       row3
   . DecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => FlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
  => GDecodeJson row2 list2
  => Monad f
  => RowToList decoderRow0 decoderList0
  => RowToList decoderRow1 decoderList1
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 intermediateRow
  => Union intermediateRow row2 row3
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Record row3)
flexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record row3)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
    record1 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    report $ record0 `union` record1 `union` record2

-- | -------------------------------------------------------------------------

class XDecodeCases
  (f :: Type -> Type)
  (list :: RowList)
  (row :: # Type)
  a
  | list -> row

instance xDecodeCasesCons
  :: ( Cons field value row' row
     , XDecodeCases f list' row' a
     , TypeEquals decoderValue (Json -> a -> f value)
     )
  => XDecodeCases f (Cons field decoderValue list') row a

instance xDecodeCasesNil :: XDecodeCases f Nil () a

class XDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __xDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Record r0)

instance __xDecodeJsonWithNil
  :: Status f
  => XDecodeJsonWith_ f Nil () Nil () a where
  __xDecodeJsonWith _ _ _ _ _ = report {}

instance __xDecodeJsonWithCons
  :: ( Cons field value row' row
     , Cons field decoderValue decoderRow' decoderRow
     , XDecodeCases f decoderList row a
     , XDecodeCases f decoderList' row' a
     , XDecodeJsonWith_ f decoderList' decoderRow' list' row' a
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , Monad f
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status f
     , TypeEquals decoderValue (Json -> a -> f value)
     )
  => XDecodeJsonWith_
       f
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field value list')
       row
       a
  where
  __xDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> f value
      decoder = to $ get sProxy decoderRecord

    rest <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        (delete sProxy decoderRecord)
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

xDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . XDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
  => GDecodeJson row1 list1
  => Monad f
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Record row2)
xDecodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record row2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ merge record0 record1
