module Data.Argonaut.Decode.X.Builder where

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

class BuilderXDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __builderXDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Builder {} (Record r0))

instance __builderXDecodeJsonWithNil
  :: Status f
  => BuilderXDecodeJsonWith_ f Nil () Nil () a where
  __builderXDecodeJsonWith _ _ _ _ _ = report identity

instance __builderXDecodeJsonWithCons
  :: ( Bind f
     , BuilderXDecodeJsonWith_ f decoderList' decoderRow' list' row' a
     , Cases1 f decoderList row a
     , Cases1 f decoderList' row' a
     , Cons field value row' row
     , Cons field decoderValue decoderRow' decoderRow
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
     , RowToList row list
     , RowToList row' list'
     , RowToList decoderRow decoderList
     , RowToList decoderRow' decoderList'
     , Status f
     , TypeEquals decoderValue (Json -> a -> f value)
     )
  => BuilderXDecodeJsonWith_
       f
       (Cons field decoderValue decoderList')
       decoderRow
       (Cons field value list')
       row
       a
  where
  __builderXDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> f value
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __builderXDecodeJsonWith
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
        reportError $ getMissingFieldErrorMessage fieldName

builderXDecodeJsonWith
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => BuilderXDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
  => GDecodeJson row1 list1
  => Nub row2 row2
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderXDecodeJsonWith decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ (Builder.merge record1) <<< builder0

builderXDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . Bind f
  => BuilderXDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
  => GDecodeJson row1 list1
  => RowToList row1 list1
  => RowToList row2 list2
  => RowToList decoderRow decoderList
  => Status f
  => Union row0 row1 row2
  => Record decoderRow
  -> Json
  -> f (Builder {} (Record row2))
builderXDecodeJsonWith_ decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record row2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy list1)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy list0)
        (RLProxy :: RLProxy decoderList)
        decoderRecord
        object
        record1
    report $ (Builder.union record1) <<< builder0
