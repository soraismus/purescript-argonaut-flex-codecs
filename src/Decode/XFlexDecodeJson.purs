module Data.Argonaut.Decode.XFlex where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases1
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Argonaut.Decode.X
import Data.Argonaut.Utils
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (Object, lookup)
import Record (delete, get, insert, merge, union)
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

class XFlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __xFlexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Record r0)

instance __xFlexDecodeJsonWithNil
  :: Status f
  => XFlexDecodeJsonWith_ f Nil () Nil () a where
  __xFlexDecodeJsonWith _ _ _ _ _ = report {}

instance __xFlexDecodeJsonWithCons
  :: ( Alternative f
     , Bind g
     , Cases1 g decoderList row a
     , Cases1 g decoderList' row' a
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
     , XFlexDecodeJsonWith_ g decoderList' decoderRow' list' row' a
     )
  => XFlexDecodeJsonWith_
       g
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field (f value) list')
       row
       a
  where
  __xFlexDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> g (f value)
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

xFlexDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => GDecodeJson row1 list1
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => XFlexDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
  => Record decoderRow
  -> Json
  -> f (Record row2)
xFlexDecodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record row2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    record0 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ merge record0 record1

xFlexDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => GDecodeJson row1 list1
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => XFlexDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
  => Record decoderRow
  -> Json
  -> f (Record row2)
xFlexDecodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record row2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    record0 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ union record0 record1

xFlexDecodeJsonWithBoth
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
  => XDecodeJsonWith_
       f
       decoderList0
       decoderRow0
       list0
       row0
       (Record row2)
  => XFlexDecodeJsonWith_
       f
       decoderList1
       decoderRow1
       list1
       row1
       (Tuple (Record row2) (Record row0))
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Record row3)
xFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record row3)
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
        record2
    record1 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
        (Tuple record2 record0)
    report $ record0 `merge` record1 `merge` record2

xFlexDecodeJsonWithBoth_
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
  => GDecodeJson row2 list2
  => RowToList decoderRow0 decoderList0
  => RowToList decoderRow1 decoderList1
  => RowToList row0 list0
  => RowToList row1 list1
  => RowToList row2 list2
  => Status f
  => Union row0 row1 intermediateRow
  => Union intermediateRow row2 row3
  => XDecodeJsonWith_
       f
       decoderList0
       decoderRow0
       list0
       row0
       (Record row2)
  => XFlexDecodeJsonWith_
       f
       decoderList1
       decoderRow1
       list1
       row1
       (Tuple (Record row2) (Record row0))
  => Record decoderRow0
  -> Record decoderRow1
  -> Json
  -> f (Record row3)
xFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record row3)
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy list2)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList0)
        decoderRecord0
        object
        record2
    record1 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy list1)
        (RLProxy :: RLProxy decoderList1)
        decoderRecord1
        object
        (Tuple record2 record0)
    report $ record0 `union` record1 `union` record2
