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
-- Should Lazy vs be used in 'builderXFlexDecodeJsonWithBoth'?
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
     , Cases f dl r
     , Cases f dl' r'
     , Cons s v r' r
     , Cons s dv dr' dr
     , DecodeJsonWith_ f dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> f v)
     )
  => DecodeJsonWith_ f (Cons s dv dl') dr (Cons s v l') r
  where
  __decodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f v
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __decodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
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
    decodeJsonWith' :: Record r1 -> Json -> f (Record r0)

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
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => DecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ merge record0 record1

decodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => DecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Record r2)
decodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ union record0 record1
