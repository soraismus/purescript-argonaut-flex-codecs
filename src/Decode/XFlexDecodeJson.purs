module Data.Argonaut.Decode.XFlex where

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
import Data.Argonaut.Decode.X
import Data.Argonaut.Utils
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
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

class XFlexDecodeJsonWith_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  __xFlexDecodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Record r0)

instance __xFlexDecodeJsonWithNil
  :: Status f
  => XFlexDecodeJsonWith_ f Nil () Nil () a where
  __xFlexDecodeJsonWith _ _ _ _ _ = report {}

instance __xFlexDecodeJsonWithCons
  :: ( Alternative f
     , Bind g
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
     , XFlexDecodeJsonWith_ g dl' dr' l' r' a
     )
  => XFlexDecodeJsonWith_ g (Cons s dv dl') dr (Cons s (f v) l') r a
  where
  __xFlexDecodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> g (f v)
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      __xFlexDecodeJsonWith
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

xFlexDecodeJsonWith
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => XFlexDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => Record dr
  -> Json
  -> f (Record r2)
xFlexDecodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    record0 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ merge record0 record1

xFlexDecodeJsonWith_
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => GDecodeJson r1 l1
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => XFlexDecodeJsonWith_ f dl dr l0 r0 (Record r1)
  => Record dr
  -> Json
  -> f (Record r2)
xFlexDecodeJsonWith_ decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    record0 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
        record1
    report $ union record0 record1

xFlexDecodeJsonWithBoth
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
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
  => XDecodeJsonWith_ f dl0 dr0 l0 r0 (Record r2)
  => XFlexDecodeJsonWith_ f dl1 dr1 l1 r1 (Tuple (Record r2) (Record r0))
  => Record dr0
  -> Record dr1
  -> Json
  -> f (Record r3)
xFlexDecodeJsonWithBoth decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
        record2
    record1 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
        (Tuple record2 record0)
    report $ record0 `merge` record1 `merge` record2

xFlexDecodeJsonWithBoth_
  :: forall dl0 dl1 dr0 dr1 f ir l0 l1 l2 r0 r1 r2 r3
   . Bind f
  => GDecodeJson r2 l2
  => RowToList dr0 dl0
  => RowToList dr1 dl1
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Union r0 r1 ir
  => Union ir r2 r3
  => XDecodeJsonWith_ f dl0 dr0 l0 r0 (Record r2)
  => XFlexDecodeJsonWith_ f dl1 dr1 l1 r1 (Tuple (Record r2) (Record r0))
  => Record dr0
  -> Record dr1
  -> Json
  -> f (Record r3)
xFlexDecodeJsonWithBoth_ decoderRecord0 decoderRecord1 = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    record2 <- reportObject object (RLProxy :: RLProxy l2)
    record0 <-
      __xDecodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy dl0)
        decoderRecord0
        object
        record2
    record1 <-
      __xFlexDecodeJsonWith
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy dl1)
        decoderRecord1
        object
        (Tuple record2 record0)
    report $ record0 `union` record1 `union` record2
