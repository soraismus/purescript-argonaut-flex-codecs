-- Should explicit kind-signatures be removed, if possible?
module Data.Argonaut.Decode.Cases.XFlex where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
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

class XFlexDecodeCases
  (f :: Type -> Type)
  (list :: RowList)
  (row :: # Type)
  a
  | list -> row

instance xFlexDecodeCasesCons
  :: ( Cons field (f value) row' row
     , TypeEquals decoderValue (Json -> a -> g (f value))
     , XFlexDecodeCases g list' row' a
     )
  => XFlexDecodeCases g (Cons field decoderValue list') row a

instance xFlexDecodeCasesNil :: XFlexDecodeCases f Nil () a
