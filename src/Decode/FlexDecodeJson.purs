-- are all instances of DecodeCases/TypeEquals necessary
-- still too many copies of of json/object error message
-- maybe the heterogenous library pertains to this; is this redundant?
-- 4 cases: 0. gdecode, 1. gdecode leniently, 2. override, 3. override w/ leniency
-- the others can be based on the general case
-- `unsafeCoerce` is used b/c at this point every Builder's src is {}.
-- Use of `unsafeCoerce` is unsightly, of course,
-- so try to generalize the type signature or introduce a bind-like
-- function for record mergers.
-- Consider using the more-general Flex-scheme with `Identity` or (-> a)
module Data.Argonaut.Decode.Flex where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases.Flex
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Argonaut.Decode.Standard
  ( class DecodeJsonWith_
  , __decodeJsonWith
  )
import Data.Argonaut.Utils
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
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

class FlexGDecodeJson f (row :: # Type) (list :: RowList) | list -> row where
  flexGDecodeJson :: Object Json -> RLProxy list -> f (Record row)

instance flexGDecodeJsonNil
  :: Status f
  => FlexGDecodeJson f () Nil where
  flexGDecodeJson _ _ = report {}

instance flexGDecodeJsonCons
  :: ( Alternative f
     , Bind g
     , Cons field (f value) rowTail row
     , DecodeJson value
     , FlexGDecodeJson g rowTail tail
     , IsSymbol field
     , Lacks field rowTail
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
   . Bind f
  => FlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
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
   . Bind f
  => FlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
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
     , Bind g
     , Cons field (f value) row' row
     , Cons field decoderValue decoderRow' decoderRow
     , FlexDecodeCases g decoderList row
     , FlexDecodeCases g decoderList' row'
     , FlexDecodeJsonWith_ g decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
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

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        decoderRecord'
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
   . Bind f
  => FlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
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
   . Bind f
  => FlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
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
   . Bind f
  => DecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => FlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
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
   . Bind f
  => DecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => FlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
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
