module Data.Argonaut.Decode.X where

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
  :: ( Bind f
     , Cases1 f dl r a
     , Cases1 f dl' r' a
     , Cons s v r' r
     , Cons s dv dr' dr
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> a -> f v)
     , XDecodeJsonWith_ f dl' dr' l' r' a
     )
  => XDecodeJsonWith_ f (Cons s dv dl') dr (Cons s v l') r a
  where
  __xDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> f v
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

xDecodeJsonWith
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => XDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => Record dr
  -> Json
  -> f (Record r2)
xDecodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ merge record0 record1

xDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => XDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => Record dr
  -> Json
  -> f (Record r2)
xDecodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ union record0 record1
