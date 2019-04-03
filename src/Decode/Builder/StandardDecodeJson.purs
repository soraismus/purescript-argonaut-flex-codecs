module Data.Argonaut.Decode.Standard.Builder where

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
  :: ( Bind f
     , BuilderDecodeJsonWith_ f dl' dr' l' r'
     , Cases f dl r
     , Cases f dl' r'
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
     , TypeEquals dv (Json -> f v)
     )
  => BuilderDecodeJsonWith_ f (Cons s dv dl') dr (Cons s v l') r
  where
  __builderDecodeJsonWith _ _ decoderRecord object = do
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
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

class
  ( Cases f l1 r0
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
  :: ( BuilderDecodeJsonWith_ f l1 r1 l0 r0
     , Cases f l1 r0
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
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderDecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Builder {} (Record r2))
builderDecodeJsonWith decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    builder0 <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ (Builder.merge record1) <<< builder0

builderDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderDecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Builder {} (Record r2))
builderDecodeJsonWith_ decoderRecord = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    builder0 <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ (Builder.union record1) <<< builder0
