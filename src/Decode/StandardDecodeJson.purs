-- still too many copies of of json/object error message
-- maybe the heterogenous library pertains to this; is this redundant?
-- 4 cases: 0. gdecode, 1. gdecode leniently, 2. override, 3. override w/ leniency
-- the others can be based on the general case
-- `unsafeCoerce` is used b/c at this point every Builder's src is {}.
-- Use of `unsafeCoerce` is unsightly, of course,
-- so try to generalize the type signature or introduce a bind-like
-- function for record mergers.
-- Consider using the more-general Flex-scheme with `Identity` or (-> a)
-- Put FlexGDecodeJson class in its own file.
-- Should Lazy values be used in 'builderXFlexDecodeJsonWithBoth'?
module Data.Argonaut.Decode.Standard where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases
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
  :: ( Bind f
     , Cases f decoderList row
     , Cases f decoderList' row'
     , Cons field value row' row
     , Cons field decoderValue decoderRow' decoderRow
     , DecodeJsonWith_ f decoderList' decoderRow' list' row'
     , IsSymbol field
     , Lacks field row'
     , Lacks field decoderRow'
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

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record decoderRow'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __decodeJsonWith
        (RLProxy :: RLProxy list')
        (RLProxy :: RLProxy decoderList')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

class
  ( Cases f l1 r0
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
  :: ( Cases f l1 r0
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
   . Bind f
  => DecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
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
   . Bind f
  => DecodeJsonWith_ f decoderList decoderRow list0 row0
  => GDecodeJson row1 list1
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
