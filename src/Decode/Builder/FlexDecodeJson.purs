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
module Data.Argonaut.Decode.Flex.Builder where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases.Flex
import Data.Argonaut.Decode.Standard.Builder
  ( class BuilderDecodeJsonWith_
  , __builderDecodeJsonWith
  )
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Argonaut.Utils
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
     , Bind g
     , BuilderFlexGDecodeJson g rowTail tail
     , Cons field (f value) rowTail row
     , DecodeJson value
     , IsSymbol field
     , Lacks field rowTail
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


-- Compare 'FlexDecodeJson':
-- ```
-- class FlexDecodeJson f a where
--  flexDecodeJson' :: Json -> f a
-- ```

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
     , RowToList row list
     , Status f
     )
  => BuilderFlexDecodeJson f () Nil row list where
  builderFlexDecodeJson' _ _ =
    reportBuilderJson $ flip builderFlexGDecodeJson (RLProxy :: RLProxy list)

builderFlexDecodeJson
  :: forall f list0 list1 list2 row0 row1 row2
   . Bind f
  => BuilderFlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
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
   . Bind f
  => BuilderFlexGDecodeJson f row0 list0
  => GDecodeJson row1 list1
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
     , Bind g
     , BuilderFlexDecodeJsonWith_ g decoderList' decoderRow' list' row'
     , Cons field (f value) row' row
     , Cons field decoderValue decoderRow' decoderRow
     , FlexDecodeCases g decoderList row
     , FlexDecodeCases g decoderList' row'
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

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        decoderRecord'
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
  :: ( BuilderFlexDecodeJsonWith_ f l1 r1 l0 r0
     , FlexDecodeCases f l1 r0
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
   . Bind f
  => BuilderFlexDecodeJsonWith_ f decoderList decoderRow list0 row0
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
builderFlexDecodeJsonWith decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
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
   . Bind f
  => BuilderFlexDecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
  => RowToList row1 list1
  => RowToList decoderRow decoderList
  => RowToList row2 list2
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderFlexDecodeJsonWith_ decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
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
   . Bind f
  => BuilderDecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => BuilderFlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
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
builderFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row3))
  go object = do
    builder0 <-
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
   . Bind f
  => BuilderDecodeJsonWith_ f decoderList0 decoderRow0 list0 row0
  => BuilderFlexDecodeJsonWith_ f decoderList1 decoderRow1 list1 row1
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
