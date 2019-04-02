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
module Data.Argonaut.Decode.Standard.Builder where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases.Standard
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
