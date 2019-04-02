module Examples where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Either
import Data.Maybe
import Data.Status.Class (class Status, report, reportError)
import Type.Proxy
import Type.Row (type (+), RProxy(RProxy))
import Data.Argonaut.FlexDecode
import Prim.RowList as PrimRL

type TypeRep_0   = ( a0 :: Int, a1 :: Int )
type TypeRep_1   = ( a2 :: Maybe Int )
type TypeRep_0_1 = ( a0 :: Int, a1 :: Int, a2 :: Maybe Int )
type TypeRep_2   = ( a3 :: Maybe String, a4 :: Maybe Boolean )
type TypeRep_1_2 =
  ( a2 :: Maybe Int
  , a3 :: Maybe String
  , a4 :: Maybe Boolean
  )

r_ :: RProxy ()
r_ = RProxy
r_0 :: RProxy TypeRep_0
r_0 = RProxy
r_1 :: RProxy TypeRep_1
r_1 = RProxy
r_2 :: RProxy TypeRep_2
r_2 = RProxy

r_0_1 :: RProxy TypeRep_0_1
r_0_1 = RProxy
r_1_2 :: RProxy TypeRep_1_2
r_1_2 = RProxy

type Type_0 = { a0 :: Int, a1 :: Int }
type Type_1 = { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
type Type_2 =
  { a0 :: Int
  , a1 :: Int
  , a2 :: Maybe Int
  , a3 :: Maybe String
  , a4 :: Maybe Boolean
  }
type Type_1_2 =
  { a2 :: Maybe Int
  , a3 :: Maybe String
  , a4 :: Maybe Boolean
  }

val0 :: Type_0
val0 = { a0: 0, a1: 1 }

val1 :: Type_1
val1 = { a0: 0, a1: 1, a2: Just 2 }

val2 :: Type_2
val2 = { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true }

json0 :: Json
json0 = encodeJson val0

json1 :: Json
json1 = encodeJson val1

json2 :: Json
json2 = encodeJson val2

type Result a = Either String a

dec0 :: Result Type_0
dec0 = decodeJson json0

dec1 :: Result Type_1
dec1 = decodeJson json1

dec2 :: Result Type_2
dec2 = decodeJson json2

_dec00_ :: Result Type_0
_dec00_ = flexDecodeJson r_ json0

_dec10_ :: Result Type_0
_dec10_ = flexDecodeJson r_ json1

_dec20_ :: Result Type_0
_dec20_ = flexDecodeJson r_ json2

_dec01_ :: Result Type_1
_dec01_ = flexDecodeJson r_ json0

_dec11_ :: Result Type_1
_dec11_ = flexDecodeJson r_ json1

_dec21_ :: Result Type_1
_dec21_ = flexDecodeJson r_ json2

_dec01_1 :: Result Type_1
_dec01_1 = flexDecodeJson r_1 json0

_dec11_1 :: Result Type_1
_dec11_1 = flexDecodeJson r_1 json1

_dec21_1 :: Result Type_1
_dec21_1 = flexDecodeJson r_1 json2

_dec02_ :: Result Type_2
_dec02_ = flexDecodeJson r_ json0

_dec02_1 :: Result Type_2
_dec02_1 = flexDecodeJson r_1 json0

_dec02_1_2 :: Result Type_2
_dec02_1_2 = flexDecodeJson r_1_2 json0

_dec12_ :: Result Type_2
_dec12_ = flexDecodeJson r_ json1

_dec12_1 :: Result Type_2
_dec12_1 = flexDecodeJson r_1 json1

_dec12_1_2 :: Result Type_2
_dec12_1_2 = flexDecodeJson r_1_2 json1

_dec22_ :: Result Type_2
_dec22_ = flexDecodeJson r_ json2

_dec22_1:: Result Type_2
_dec22_1 = flexDecodeJson r_1 json2

_dec22_1_2 :: Result Type_2
_dec22_1_2 = flexDecodeJson r_1_2 json2

xdec :: Result Type_1_2
xdec = flexDecodeJson' json2


_adec1_0 :: Result (Record TypeRep_1)
_adec1_0 =
  decodeJsonWith'
    { a2: \json -> Right $ Just 10 }
    json1

_adec1_1 :: Result (Record TypeRep_1)
_adec1_1 =
  decodeJsonWith'
    { a2: \json -> Right $ Nothing }
    json1

_adec1_2 :: Result (Record TypeRep_1)
_adec1_2 =
  decodeJsonWith'
    { a2: \json -> Left "Capricious failure" }
    json1

x0 :: Result Type_1
x0 =
  decodeJsonWith'
    { a0: \json -> (Right 1000) :: Result Int
    , a1: \json -> (Right 1001) :: Result Int
    , a2: \json -> (Right $ Just 1002) :: Result (Maybe Int)
    }
    json1

x1 :: Result Type_1
x1 =
  decodeJsonWith'
    { a0: \json -> Right 1000
    , a1: \json -> Right 1001
    , a2: \json -> Right $ Just 1002
    }
    json1

x2 :: Result Type_1
x2 =
  decodeJsonWith'
    { a0: decodeJsonInt
    , a1: decodeJsonInt
    , a2: decodeJsonMaybeInt
    }
    json1
    where
    decodeJsonInt :: Json -> Result Int
    decodeJsonInt = decodeJson
    decodeJsonMaybeInt :: Json -> Result (Maybe Int)
    decodeJsonMaybeInt = decodeJson

x3 :: Result Type_1
x3 =
  decodeJsonWith'
    { a0: \json -> Right 1000
    , a1: \json -> Right 1001
    , a2: \json -> Right $ Nothing
    }
    json1

x4 :: Result Type_1
x4 =
  decodeJsonWith'
    { a0: \json -> Right 1000
    , a1: \json -> Left "Capricious failure"
    , a2: \json -> Right $ Just 1002
    }
    json1

y0 :: Result Type_1
y0 =
  decodeJsonWith
    { a2: \json -> Right $ Just 1002 }
    json1

y1_1 :: Result Type_2
y1_1 =
  decodeJsonWith
    { a2: \json -> Right $ Just 1002
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json1

y1_2 :: Result Type_2
y1_2 =
  decodeJsonWith
    { a2: \json -> Right $ Just 1002
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json2

y2 :: Result Type_2
y2 =
  decodeJsonWith
    { a1: \json -> Right $ 1002
    , a3: \json -> Right $ Just "bye"
    }
    json2

y3 :: Result Type_2
y3 =
  decodeJsonWith
    { a1: \json -> Right $ 1002
    , a3: \json -> Left "Capricious failure"
    }
    json2

y4 :: Result Type_2
y4 =
  decodeJsonWith
    { a1: \json -> Right $ 1002
    , a4: \json -> Right $ Just false
    }
    json2

y5 :: Result Type_2
y5 =
  decodeJsonWith
    {}
    json2

z0_0 :: Result Type_1
z0_0 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002 }
    json0

z0_1 :: Result Type_1
z0_1 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002 }
    json1

z0_2 :: Result Type_1
z0_2 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002 }
    json2

z0_2_ :: Result Type_1
z0_2_ =
  flexDecodeJsonWith
    { a2: decodeJson' }
    json2
  where
  decodeJson' :: Json -> Result (Maybe Int)
  decodeJson' = decodeJson

z1_0 :: Result Type_2
z1_0 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json0

z1_1 :: Result Type_2
z1_1 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json1

z1_2 :: Result Type_2
z1_2 =
  flexDecodeJsonWith
    { a2: \json -> Right $ Just 1002
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json2

z1_2_ :: Result Type_2
z1_2_ =
  flexDecodeJsonWith
    { a2: decodeJson2
    , a3: decodeJson3
    , a4: decodeJson4
    }
    json2
  where
  decodeJson2 :: Json -> Result (Maybe Int)
  decodeJson2 = decodeJson
  decodeJson3 :: Json -> Result (Maybe String)
  decodeJson3 = decodeJson
  decodeJson4 :: Json -> Result (Maybe Boolean)
  decodeJson4 = decodeJson

a0 :: Result Type_2
a0 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ Just 102
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json0

a1 :: Result Type_2
a1 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ Just 102
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json1

a2 :: Result Type_2
a2 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ Just 102
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json2

a3 :: Result Type_2
a3 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    , a3: \json -> Right $ Just "bye"
    }
    { a2: \json -> Right $ Just 102
    , a4: \json -> Right $ Just false
    }
    json2

a4 :: Result Type_2
a4 =
  flexDecodeJsonWithBoth
    {}
    { a2: \json -> Right $ Just 102
    , a4: \json -> Right $ Just false
    }
    json2

a5 :: Result Type_2
a5 =
  flexDecodeJsonWithBoth
    {}
    { a2: \json -> Right $ Just 102
    , a4: \json -> Right $ Just false
    }
    json1

a6 :: Result Type_2
a6 =
  flexDecodeJsonWithBoth
    {}
    { a2: \json -> Right $ Just 102
    , a4: \json -> Right $ Just false
    }
    json0

a7 :: Result Type_2
a7 =
  flexDecodeJsonWithBoth
    {}
    { a2: \json -> Right $ Just 102
    }
    json0

a8 :: Result Type_2
a8 =
  flexDecodeJsonWithBoth
    { a4: \json -> Right $ Just false
    }
    { a2: \json -> Right $ Just 102
    }
    json0

a9 :: Result Type_2
a9 =
  flexDecodeJsonWithBoth
    {}
    { a2: \json -> Right $ Just 102
    , a3: \json -> Right $ Just "bye"
    , a4: \json -> Right $ Just false
    }
    json1

type MhType_2 =
  { a0 :: Int
  , a1 :: Int
  , a2 :: Array Int
  , a3 :: Array String
  , a4 :: Array Boolean
  }

mh0 :: Result MhType_2
mh0 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    , a3: \json -> Right $ ["bye"]
    }
    { a2: \json -> Right $ [102]
    , a4: \json -> Right $ [false]
    }
    json0

mh0_ :: Result MhType_2
mh0_ =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ [102]
    , a3: \json -> Right $ ["bye"]
    , a4: \json -> Right $ [false]
    }
    json0

mh1 :: Result MhType_2
mh1 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    , a3: \json -> Right $ ["bye"]
    }
    { a2: \json -> Right $ [102]
    , a4: \json -> Right $ [false]
    }
    json1

mh1_ :: Result MhType_2
mh1_ =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ [102]
    , a3: \json -> Right $ ["bye"]
    , a4: \json -> Right $ [false]
    }
    json1

mh2 :: Result MhType_2
mh2 =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    , a3: \json -> Right $ ["bye"]
    }
    { a2: \json -> Right $ [102]
    , a4: \json -> Right $ [false]
    }
    json2

mh2_ :: Result MhType_2
mh2_ =
  flexDecodeJsonWithBoth
    { a0: \json -> Right 100
    , a1: \json -> Right 101
    }
    { a2: \json -> Right $ [102]
    , a3: \json -> Right $ ["bye"]
    , a4: \json -> Right $ [false]
    }
    json2

newtype Maybe' a = Maybe' (Maybe a)
instance showMaybe_ :: (Show a) => Show (Maybe' a) where
  show (Maybe' (Just x)) = "<Maybe' Just " <> show x <> " >"
  show (Maybe' Nothing) = "<Maybe' Nothing >"

instance statusMaybe_ :: Status Maybe' where
  report x = Maybe' $ Just x
  reportError _ = Maybe' Nothing

_mh1 :: Maybe' MhType_2
_mh1 =
  flexDecodeJsonWithBoth
    { a0: \json -> report 100
    , a1: \json -> report 101
    , a3: \json -> report $ ["bye"]
    }
    { a2: \json -> report $ [102]
    , a4: \json -> report $ [false]
    }
    json1

_mh1_ :: Maybe' MhType_2
_mh1_ =
  flexDecodeJsonWithBoth
    { a0: \json -> report 100
    , a1: \json -> report 101
    }
    { a2: \json -> report $ [102]
    , a3: \json -> report $ ["bye"]
    , a4: \json -> report $ [false]
    }
    json1

_mh2_ :: Maybe' MhType_2
_mh2_ =
  flexDecodeJsonWithBoth
    { a0: \json -> report 100
    , a1: \json -> report 101
    }
    { a2: \json -> report $ [102]
    , a3: \json -> report $ ["bye"]
    , a4: \json -> report $ [false]
    }
    json2

zz :: Result Type_2
zz =
  xDecodeJsonWith
    { a2: \json (rest :: Type_0) -> Right $ Just 1002
    , a3: \json (rest :: Type_0) -> Right $ Just "bye"
    , a4: \json (rest :: Type_0) -> Right $ Just false
    }
    json2

zz1_2 :: Result Type_2
zz1_2 =
  xDecodeJsonWith
    { a2: \json (rest :: Type_0) -> Right $ Just (rest.a0)
    , a3: \json (rest :: Type_0) -> Right $ Just "bye"
    , a4: \json (rest :: Type_0) -> Right $ Just false
    }
    json2

zz1_2_ :: Result Type_2
zz1_2_ =
  xDecodeJsonWith
    { a2: \json rest -> Right $ Just (rest.a0)
    , a3: \json rest -> Right $ Just $ show rest.a0
    , a4: \json rest -> Right $ Just $ (rest.a1 `mod` 2 == 0)
    }
    json2

zz1_2__ :: Result Type_2
zz1_2__ =
  xDecodeJsonWith
    { a4: \json rest -> Right $ isEven <$> rest.a2
    }
    json2
  where
  isEven :: Int -> Boolean
  isEven i = (i `mod` 2) == 0

instance functorMaybe_ :: Functor Maybe' where
  map fn (Maybe' (Just x)) = Maybe' $ Just (fn x)
  map _  _                 = Maybe' Nothing
instance applyMaybe_ :: Apply Maybe' where
  apply (Maybe' (Just fn)) x = fn <$> x
  apply (Maybe' Nothing)   _ = Maybe' Nothing
instance applicativeMaybe_ :: Applicative Maybe' where
  pure = Maybe' <<< Just
instance bindMaybe_ :: Bind Maybe' where
  bind (Maybe' (Just x)) k = k x
  bind (Maybe' Nothing)  _ = Maybe' Nothing
instance monadMaybe_ :: Monad Maybe'
