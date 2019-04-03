module Data.Argonaut.Decode.Flex.Builder where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases
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
  (r :: # Type)
  (l :: RowList)
  | l -> r where
  builderFlexGDecodeJson
    :: Object Json
    -> RLProxy l
    -> f (Builder {} (Record r))

instance builderFlexGDecodeJsonNil
  :: Status f
  => BuilderFlexGDecodeJson f () Nil where
  builderFlexGDecodeJson _ _ = report identity

instance builderFlexGDecodeJsonCons
  :: ( Alternative f
     , Bind g
     , BuilderFlexGDecodeJson g r' l'
     , Cons s (f v) r' r
     , DecodeJson v
     , IsSymbol s
     , Lacks s r'
     , Status g
     )
  => BuilderFlexGDecodeJson g r (Cons s (f v) l') where
  builderFlexGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- builderFlexGDecodeJson object (RLProxy :: RLProxy l')
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
  :: ( BuilderFlexGDecodeJson f r l
     , RowToList r l
     , Status f
     )
  => BuilderFlexDecodeJson f () Nil r l where
  builderFlexDecodeJson' _ _ =
    reportBuilderJson $ flip builderFlexGDecodeJson (RLProxy :: RLProxy l)

builderFlexDecodeJson
  :: forall f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderFlexGDecodeJson f r0 l0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => RProxy r0
  -> Json
  -> f (Builder {} (Record r2))
builderFlexDecodeJson _ = reportBuilderJson go
  where
  go object = do
    builder0 <- builderFlexGDecodeJson object (RLProxy :: RLProxy l0)
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ (Builder.merge record1) <<< builder0

builderFlexDecodeJson_
  :: forall f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderFlexGDecodeJson f r0 l0
  => GDecodeJson r1 l1
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => RProxy r0
  -> Json
  -> f (Builder {} (Record r2))
builderFlexDecodeJson_ _ = reportBuilderJson go
  where
  go object = do
    builder0 <- builderFlexGDecodeJson object (RLProxy :: RLProxy l0)
    record1 <- reportObject object (RLProxy :: RLProxy l1)
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
     , BuilderFlexDecodeJsonWith_ g dl' dr' l' r'
     , Cases g dl r
     , Cases g dl' r'
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
     , TypeEquals dv (Json -> g (f v))
     )
  => BuilderFlexDecodeJsonWith_ g (Cons s dv dl') dr (Cons s (f v) l') r
  where
  __builderFlexDecodeJsonWith _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> g (f v)
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ Builder.insert sProxy val <<< rest
      Nothing ->
        report $ Builder.insert sProxy empty <<< rest

class
  ( Cases f l1 r0
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
     , Cases f l1 r0
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
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderFlexDecodeJsonWith_ f dl dr l0 r0
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
builderFlexDecodeJsonWith decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    builder0 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ (Builder.merge record1) <<< builder0

builderFlexDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => BuilderFlexDecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList dr dl
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Builder {} (Record r2))
builderFlexDecodeJsonWith_ decoderRecord = reportBuilderJson $ go
  where
  go :: Object Json -> f (Builder {} (Record r2))
  go object = do
    builder0 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ (Builder.union record1) <<< builder0

builderFlexDecodeJsonWithBoth
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => BuilderDecodeJsonWith_ f dl0 dr0 l0 r0
  => BuilderFlexDecodeJsonWith_ f dl1 dr1 l1 r1
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
builderFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r3))
  go object = do
    builder0 <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
    builder1 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
    let
      builder1' :: Builder (Record r0) (Record ir)
      builder1' = unsafeCoerce builder1
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    report $ (Builder.merge record2) <<< builder1' <<< builder0

builderFlexDecodeJsonWithBoth_
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => BuilderDecodeJsonWith_ f dl0 dr0 l0 r0
  => BuilderFlexDecodeJsonWith_ f dl1 dr1 l1 r1
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
builderFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportBuilderJson go
  where
  go :: Object Json -> f (Builder {} (Record r3))
  go object = do
    (builder0 :: Builder {} (Record r0)) <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
    builder1 <-
      __builderFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
    let
      builder1' :: Builder (Record r0) (Record ir)
      builder1' = unsafeCoerce builder1
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    report $ (Builder.union record2) <<< builder1' <<< builder0
