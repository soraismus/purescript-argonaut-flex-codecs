module Data.Argonaut.Decode.Flex where

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
import Data.Argonaut.Decode.Standard
  ( class DecodeJsonWith_
  , __decodeJsonWith
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

class FlexGDecodeJson f (r :: # Type) (l :: RowList) | l -> r where
  flexGDecodeJson :: Object Json -> RLProxy l -> f (Record r)

instance flexGDecodeJsonNil
  :: Status f
  => FlexGDecodeJson f () Nil where
  flexGDecodeJson _ _ = report {}

instance flexGDecodeJsonCons
  :: ( Alternative f
     , Bind g
     , Cons s (f v) rTail r
     , DecodeJson v
     , FlexGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Status g
     )
  => FlexGDecodeJson g r (Cons s (f v) tail) where
  flexGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- flexGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy (pure val) rest
      Nothing ->
        report $ insert sProxy empty rest

class FlexDecodeJson f a where
  flexDecodeJson' :: Json -> f a

instance flexDecodeJsonRecord
  :: ( FlexGDecodeJson f r l
     , RowToList r l
     , Status f
     )
  => FlexDecodeJson f (Record r) where
  flexDecodeJson' = reportJson $ flip flexGDecodeJson (RLProxy :: RLProxy l)

flexDecodeJson
  :: forall f l0 l1 l2 r0 r1 r2
   . Bind f
  => FlexGDecodeJson f r0 l0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => RProxy r0
  -> Json
  -> f (Record r2)
flexDecodeJson _ = reportJson go
  where
  go object = do
    record0 <- flexGDecodeJson object (RLProxy :: RLProxy l0)
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ merge record0 record1

flexDecodeJson_
  :: forall f l0 l1 l2 r0 r1 r2
   . Bind f
  => FlexGDecodeJson f r0 l0
  => GDecodeJson r1 l1
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => RProxy r0
  -> Json
  -> f (Record r2)
flexDecodeJson_ _ = reportJson go
  where
  go object = do
    record0 <- flexGDecodeJson object (RLProxy :: RLProxy l0)
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ union record0 record1

class FlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  __flexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance __flexDecodeJsonWithNil
  :: Status f
  => FlexDecodeJsonWith_ f Nil () Nil () where
  __flexDecodeJsonWith _ _ _ _ = report {}

instance __flexDecodeJsonWithCons
  :: ( Alternative f
     , Bind g
     , Cases g dl r
     , Cases g dl' r'
     , Cons s (f v) r' r
     , Cons s dv dr' dr
     , FlexDecodeJsonWith_ g dl' dr' l' r'
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
  => FlexDecodeJsonWith_ g (Cons s dv dl') dr (Cons s (f v) l') r
  where
  __flexDecodeJsonWith _ _ decoderRecord object = do
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
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        report $ insert sProxy empty rest

class
  ( Cases f l1 r0
  , RowToList r1 l1
  ) <=
  FlexDecodeJsonWith
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    flexDecodeJsonWith' :: Record r1 -> Json -> f (Record r0)

instance flexDecodeJsonWithDecodeJsonWith_
  :: ( Cases f l1 r0
     , FlexDecodeJsonWith_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => FlexDecodeJsonWith f l1 r1 r0
  where
  flexDecodeJsonWith' decoderRecord =
    reportJson $
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord

flexDecodeJsonWith
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => FlexDecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList dr dl
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Record r2)
flexDecodeJsonWith decoderRecord = reportJson $ go decoderRecord
  where
  go :: Record dr -> Object Json -> f (Record r2)
  go decoderRecord object = do
    record0 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ merge record0 record1

flexDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => FlexDecodeJsonWith_ f dl dr l0 r0
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList dr dl
  => RowToList r2 l2
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Record r2)
flexDecodeJsonWith_ decoderRecord = reportJson $ go decoderRecord
  where
  go :: Record dr -> Object Json -> f (Record r2)
  go decoderRecord object = do
    record0 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ union record0 record1

flexDecodeJsonWithBoth
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => DecodeJsonWith_ f dl0 dr0 l0 r0
  => FlexDecodeJsonWith_ f dl1 dr1 l1 r1
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
  -> f (Record r3)
flexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
    record1 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    report $ record0 `merge` record1 `merge` record2

flexDecodeJsonWithBoth_
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => DecodeJsonWith_ f dl0 dr0 l0 r0
  => FlexDecodeJsonWith_ f dl1 dr1 l1 r1
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
  -> f (Record r3)
flexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    record0 <-
      __decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
    record1 <-
      __flexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    report $ record0 `union` record1 `union` record2
