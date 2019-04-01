module Data.Status.Class
  ( class Status
  , report
  , reportError
  )
  where

import Control.Alternative (class Alternative, empty)
import Data.Either (Either(Left, Right))

-- instance applyMaybe :: Apply Maybe where
--   apply (Just fn) x = fn <$> x
--   apply Nothing   _ = Nothing
-- instance altMaybe :: Alt Maybe where
--   alt Nothing r = r
--   alt l       _ = l
-- | The `Ord` instance allows `Maybe` values to be compared with
-- | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
-- | the type the `Maybe` contains.
-- |
-- | `Nothing` is considered to be less than any `Just` value.
-- derive instance ordMaybe :: Ord a => Ord (Maybe a)
-- instance ord1Maybe :: Ord1 Maybe where compare1 = compare
-- instance applyEither :: Apply (Either e) where
--   apply (Left e) _ = Left e
--   apply (Right f) r = f <$> r
-- instance altEither :: Alt (Either e) where
--   alt (Left _) r = r
--   alt l        _ = l
-- | The `Ord` instance allows `Either` values to be compared with
-- | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
-- | both types the `Either` can contain.
-- |
-- | Any `Left` value is considered to be less than a `Right` value.
-- derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
-- derive instance ord1Either :: Ord a => Ord1 (Either a)


-- class Functor f <= Alt f where
--   alt :: forall a. f a -> f a -> f a

-- class Eq a <= Ord a where
--   compare :: a -> a -> Ordering
-- class Eq1 f <= Ord1 f where
--   compare1 :: forall a. Ord a => f a -> f a -> Ordering

-- data Ordering = LT | GT | EQ

--data PartialOrdering = LT | GT | EQ | NA

-- class (Functor f) <= NonIncreasingAlt f a where
--   alt :: f a -> f a -> f a

-- instance MeetSemilatticeF f => NonIncreasingAlt f a where
--   alt x0 x1 = meetF x0 x1

-- instance nonIncreasingAltEitherString :: NonIncreasingAlt (Either a) where
--   errorAlt (Left a)  _ = const a <$> bottomF
--   errorAlt (Right b) x = f <$> x

class Status f where
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a

instance statusEitherString :: Status (Either String) where
  report = Right
  reportError = Left

-- instance statusApplicativeBottomSemilattice
--   :: ( Applicative f
--      , BottomSemilatticeF f
--      )
--   => Status f where
--   report = pure
--   reportError msg = const msg <$> bottomF  -- can't insert `msg` into (Left ())
-- -- this reminds me of Heterogeneous mapping and lenses.

-- instance statusAlternative :: Alternative f => Status f where
--   report = pure
--   reportError = const empty
