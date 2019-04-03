-- Should Lazy values be used in 'builderXFlexDecodeJsonWithBoth'?
module Data.Argonaut.Decode.XFlex.Builder where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases.XFlex
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Argonaut.Decode.X.Builder
import Data.Argonaut.Utils
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (Object, lookup)
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder (build, insert, merge, union) as Builder
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

class BuilderXFlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __builderXFlexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Builder {} (Record r0))

instance __builderXFlexDecodeJsonWithNil
  :: Status f
  => BuilderXFlexDecodeJsonWith_ f Nil () Nil () a where
  __builderXFlexDecodeJsonWith _ _ _ _ _ = report identity

instance __builderXFlexDecodeJsonWithCons
  :: ( Alternative f
     , Bind g
     , BuilderXFlexDecodeJsonWith_ g decoderList' decoderRow' list' row' a
     , Cons field (f value) row' row
     , Cons field decoderValue decoderRow' decoderRow
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status g
     , TypeEquals decoderValue (Json -> a -> g (f value))
     , XFlexDecodeCases g decoderList row a
     , XFlexDecodeCases g decoderList' row' a
     )
  => BuilderXFlexDecodeJsonWith_
       g
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field (f value) list')
       row
       a
  where
  __builderXFlexDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> g (f value)
      decoder = to $ Record.get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    (rest :: Builder {} (Record row')) <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        report $ Builder.insert sProxy empty <<< rest

builderXFlexDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => BuilderXFlexDecodeJsonWith_
       f
       decoderList
       decoderRow
       list0
       row0
       (Record row1)
  => GDecodeJson row1 list1
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderXFlexDecodeJsonWith decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    builder0 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ (Builder.merge record1) <<< builder0

builderXFlexDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => BuilderXFlexDecodeJsonWith_
       f
       decoderList
       decoderRow
       list0
       row0
       (Record row1)
  => GDecodeJson row1 list1
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderXFlexDecodeJsonWith_ decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    builder0 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ (Builder.union record1) <<< builder0

builderXFlexDecodeJsonWithBoth
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
   . Bind f
  => BuilderXDecodeJsonWith_
       f
       decoderList0
       decoderRow0
       list0
       row0
       (Record row2)
  => BuilderXFlexDecodeJsonWith_
       f
       decoderList1
       decoderRow1
       list1
       row1
       (Tuple (Record row2) (Record row0))
  => GDecodeJson row2 list2
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
builderXFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row3))
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
        record2
    builder1 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
        (Tuple record2 (Builder.build builder0 {}))
    let
      builder1' :: Builder (Record row0) (Record intermediateRow)
      builder1' = unsafeCoerce builder1
    report $ (Builder.merge record2) <<< builder1' <<< builder0

builderXFlexDecodeJsonWithBoth_
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
   . Bind f
  => BuilderXDecodeJsonWith_
       f
       decoderList0
       decoderRow0
       list0
       row0
       (Record row2)
  => BuilderXFlexDecodeJsonWith_
       f
       decoderList1
       decoderRow1
       list1
       row1
       (Tuple (Record row2) (Record row0))
  => GDecodeJson row2 list2
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
builderXFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row3))
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
        record2
    builder1 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
        (Tuple record2 (Builder.build builder0 {}))
    let
      builder1' :: Builder (Record row0) (Record intermediateRow)
      builder1' = unsafeCoerce builder1
    report $ (Builder.union record2) <<< builder1' <<< builder0
