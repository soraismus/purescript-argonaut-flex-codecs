module Data.Argonaut.Utils where

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

_decodeJson :: forall a. DecodeJson a => Proxy a -> Json -> Either String a
_decodeJson _ = decodeJson

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"

getMissingFieldErrorMessage :: String -> String
getMissingFieldErrorMessage fieldName =
  "JSON was missing expected field: " <> fieldName

-- NOTE: I was working on this before I was interrupted,
-- so it might be helpful.
-- ------------------------
-- toStatus :: forall f g a. Alt f => Monad f => Status g => String -> f a -> g a
-- toStatus msg mx =
--   apply (pure identity) mx
--   case mx of
--     Just x -> report x
--     Nothing -> reportError msg

reportJson
  :: forall f r
   . Status f
  => (Object Json -> f (Record r))
  -> Json
  -> f (Record r)
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage

reportObject
  :: forall f l r
   . GDecodeJson r l
  => RowToList r l
  => Status f
  => Object Json
  -> RLProxy l
  -> f (Record r)
reportObject object rlProxy =
  case gDecodeJson object rlProxy of
    Left errorStr -> reportError errorStr
    Right record -> report record

reportBuilderJson
  :: forall f r
   . Status f
  => (Object Json -> f (Builder {} (Record r)))
  -> Json
  -> f (Builder {} (Record r))
reportBuilderJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage
