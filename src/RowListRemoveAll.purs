module Type.Row.RowListRemoveAll.Class where

import Type.Data.Boolean (class If, class Or, kind Boolean, True, False)
import Type.Data.Ordering (class Equals)
import Prim.Ordering (LT, EQ)
import Prim.Symbol (class Compare)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Data.Symbol as Symbol

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  )
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object as FO
import Prim.Row as PrimRow
import Prim.RowList as PrimRL
import Record as Record
import Type.Data.Row (RProxy(RProxy))
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(Proxy))
import Type.Row as Row

class RowListRemoveAll_
  (continue :: Boolean)
  (lhs :: PrimRL.RowList)
  (rhs :: PrimRL.RowList)
  (out :: PrimRL.RowList) | lhs rhs -> out

instance rowListRemove_Nil
  :: TypeEquals (RLProxy rhs) (RLProxy out)
  => RowListRemoveAll_ True PrimRL.Nil rhs out

instance rowListRemove_stop :: RowListRemoveAll_ False lhs rhs out

instance rowListRemove_Cons
  :: ( TypeEquals (RLProxy (PrimRL.Cons llabel lhead lhs')) (RLProxy lhs)
     , TypeEquals (RLProxy (PrimRL.Cons rlabel rhead rhs')) (RLProxy rhs)
     , Symbol.Equals llabel rlabel eq
     , RowListRemoveAll_ eq lhs' rhs' out0
     , RowListRemoveAll_ eq lhs  rhs' out1
     , If eq
         (RLProxy out0)
         (RLProxy out1)
         (RLProxy out)
     )
  => RowListRemoveAll_ True (PrimRL.Cons llabel lhead lhs') rhs out

class RowListRemoveAll
  (lhs :: PrimRL.RowList)
  (rhs :: PrimRL.RowList)
  (out :: PrimRL.RowList) | lhs rhs -> out

instance rowListRemoveNil
  :: RowListRemoveAll_ True PrimRL.Nil rhs out
  => RowListRemoveAll       PrimRL.Nil rhs out

instance rowListRemoveCons
  :: ( TypeEquals (RLProxy (PrimRL.Cons llabel lhead lhs')) (RLProxy lhs)
     , TypeEquals (RLProxy (PrimRL.Cons rlabel rhead rhs')) (RLProxy rhs)
     , Symbol.Equals llabel rlabel eq
     , RowListRemoveAll_ eq lhs' rhs' out0
     , RowListRemoveAll_ eq lhs  rhs' out1
     , If eq
         (RLProxy out0)
         (RLProxy out1)
         (RLProxy out)
     )
  => RowListRemoveAll (PrimRL.Cons llabel lhead lhs') rhs out
