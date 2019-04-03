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
-- Are Lacks instances necessary?
module Data.Argonaut.Decode.Standard where

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
  , class ListToRow
  , class Nub
  , class RowListAppend
  , class RowToList
  , class Union
  )
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith_
  (f :: Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)

  (l :: RowList)
  (r :: # Type)
  | l0 l1 -> l r l2 r0 r1 r2 where
  __decodeJsonWith
    :: RLProxy l
    -> RLProxy l0
    -> RLProxy l1
    -> Record r2
    -> Object Json
    -> f (Record r)

instance __decodeJsonWithNil
  :: Status f
  => DecodeJsonWith_ f Nil () r l r l            Nil () where
  __decodeJsonWith _ _ _ _ _ = report {}

instance __decodeJsonWithCons
  :: ( Bind f
     , Cons s v r' r
     , Cons s dv dr0' dr0
     , Cons s dv dr1 dr1'
     , Cons s dv dr2_ dr2
     , DecodeCases f dl0 r
     , DecodeCases f dl0' r'
     , DecodeJsonWith_ f dl0' dr0' dl1' dr1' dl2' dr2' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr0'
     , Lacks s dr1
     , Lacks s dr2_
     , ListToRow dl2' dr2'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr0 dl0
     , RowToList dr1 dl1
     , RowToList dr2 dl2
     , RowToList dr0' dl0'
     , RowToList dr1' dl1'
     , RowListAppend dl0' dl1' dIntermediateL
     , RowListAppend dIntermediateL (Cons s dv Nil) dl2'
     , Status f
     , TypeEquals dv (Json -> f v)
     , TypeEquals (Record dr2) (Record dr2')
     , Union dr0  dr1  dr2
     , Union dr0' dr1' dr2
     , Union dr0' dr1  dr2'
     )
  => DecodeJsonWith_
       f
       (Cons s dv dl0')
       dr0
       dl1
       dr1
       dl2
       dr2
       l
       r
  where
  __decodeJsonWith _ _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      sName :: String
      sName = reflectSymbol sProxy

      decoder :: Json -> f v
      -- get :: IsSymbol s => Cons s v r' r => SProxy s -> { | r } -> v
      -- decoderRecord :: Record dr2
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      --decoderRecord' :: Record dr0'
      --decoderRecord' = unsafeCoerce decoderRecord

      decoderRecord' :: Record dr2'
      decoderRecord' = to decoderRecord

    rest <-
      __decodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl0')
        (RLProxy :: RLProxy dl1')
        decoderRecord'
        object

    case lookup sName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage sName

class
  ( DecodeCases f dl r
  , RowToList dr dl
  ) <=
  DecodeJsonWith
    (f :: Type -> Type)
    (dl :: RowList)
    (dr :: # Type)
    (r :: # Type)
    | r -> dr dl where
    decodeJsonWith'
      :: Record dr
      -> Json
      -> f (Record r)

instance decodeJsonWithDecodeJsonWith_
  :: ( DecodeCases f l0 r
     , DecodeJsonWith_ f dl0 dr0 dl1 dr1 dl2 dr2 l r
     , RowToList r l
     , RowToList dr0 dl0
     , Status f
     )
  => DecodeJsonWith f dl2 dr2 r
  where
  decodeJsonWith' decoderRecord =
    reportJson $
      __decodeJsonWith
        (RLProxy :: RLProxy l)
        (RLProxy :: RLProxy dl0)
        (RLProxy :: RLProxy dl1)
        decoderRecord

decodeJsonWith
  :: forall dl0 dl1 dl2 dr0 dr1 dr2 f l0 l1 l2 r0 r1 r2
   . Bind f
  => DecodeJsonWith_ f dl0 dr0 dl1 dr1 dl1 dr1 l0 r0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr0 dl0
  => Status f
  => Union r0 r1 r2
  => Record dr2
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        (RLProxy :: RLProxy dl1)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ merge record0 record1

decodeJsonWith_
  :: forall dl0 dl1 dl2 dr0 dr1 dr2 f l0 l1 l2 r0 r1 r2
   . Bind f
  => DecodeJsonWith_ f dl0 dr0 dl1 dr1 dl1 dr1 l0 r0
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr0 dl0
  => Status f
  => Union r0 r1 r2
  => Record dr2
  -> Json
  -> f (Record r2)
decodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        (RLProxy :: RLProxy dl1)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ union record0 record1
