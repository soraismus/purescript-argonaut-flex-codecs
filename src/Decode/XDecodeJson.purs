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
-- Consider using the more-general Flex-scheme with `Identity` or (-> a)
module Data.Argonaut.Decode.X where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases.X
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

xDecodeJsonWith_
  :: forall decoderRow decoderList f list0 list1 list2 row0 row1 row2
   . XDecodeJsonWith_ f decoderList decoderRow list0 row0 (Record row1)
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
xDecodeJsonWith_ decoderRecord = reportJson go
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
    report $ union record0 record1

-- | -------------------------------------------------------------------------
