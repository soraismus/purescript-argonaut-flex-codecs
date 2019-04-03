module Data.Argonaut.Decode.XFlex.Builder where

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
import Data.Argonaut.Decode.X.Builder
import Data.Argonaut.Utils
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (Object, lookup)
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder (build, insert, merge, union) as Builder
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

class BuilderXFlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __builderXFlexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Builder {} (Record r0))

instance __builderXFlexDecodeJsonWithNil
  :: Status f
  => BuilderXFlexDecodeJsonWith_ f Nil () Nil () a where
  __builderXFlexDecodeJsonWith _ _ _ _ _ = report identity

instance __builderXFlexDecodeJsonWithCons
  :: ( Alternative f
     , Bind g
     , BuilderXFlexDecodeJsonWith_ g dl' dr' l' r' a
     , Cases1 g dl r a
     , Cases1 g dl' r' a
     , Cons s (f v) r' r
     , Cons s dv dr' dr
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status g
     , TypeEquals dv (Json -> a -> g (f v))
     )
  => BuilderXFlexDecodeJsonWith_ g (Cons s dv dl') dr (Cons s (f v) l') r a
  where
  __builderXFlexDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> g (f v)
      decoder = to $ Record.get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        report $ Builder.insert sProxy empty <<< rest

builderXFlexDecodeJsonWith
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderXFlexDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList dr dl
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Builder {} (Record r2))
builderXFlexDecodeJsonWith decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    builder0 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ (Builder.merge record1) <<< builder0

builderXFlexDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderXFlexDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList dr dl
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Builder {} (Record r2))
builderXFlexDecodeJsonWith_ decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    builder0 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ (Builder.union record1) <<< builder0

builderXFlexDecodeJsonWithBoth
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => BuilderXDecodeJsonWith_ f dl0 dr0 l0 r0 (Record r2)
  => BuilderXFlexDecodeJsonWith_ f dl1 dr1 l1 r1 (Tuple (Record r2) (Record r0))
  => GDecodeJson r2 l2
  => Nub ir ir
  => Nub r3 r3
  => RowToList dr0 dl0
  => RowToList dr1 dl1
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 ir
  => Union ir r2 r3
  => Record dr0
  -> Record dr1
  -> Json
  -> f (Builder {} (Record r3))
builderXFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 =
  reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r3))
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
        record2
    builder1 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
        (Tuple record2 (Builder.build builder0 {}))
    let
      builder1' :: Builder (Record r0) (Record ir)
      builder1' = unsafeCoerce builder1
    report $ (Builder.merge record2) <<< builder1' <<< builder0

builderXFlexDecodeJsonWithBoth_
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => BuilderXDecodeJsonWith_ f dl0 dr0 l0 r0 (Record r2)
  => BuilderXFlexDecodeJsonWith_ f dl1 dr1 l1 r1 (Tuple (Record r2) (Record r0))
  => GDecodeJson r2 l2
  => RowToList dr0 dl0
  => RowToList dr1 dl1
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 ir
  => Union ir r2 r3
  => Record dr0
  -> Record dr1
  -> Json
  -> f (Builder {} (Record r3))
builderXFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 =
  reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r3))
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    builder0 <-
      __builderXDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
        record2
    builder1 <-
      __builderXFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
        (Tuple record2 (Builder.build builder0 {}))
    let
      builder1' :: Builder (Record r0) (Record ir)
      builder1' = unsafeCoerce builder1
    report $ (Builder.union record2) <<< builder1' <<< builder0
