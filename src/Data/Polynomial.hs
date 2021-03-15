{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Polynomial where

import Data.Functor ((<&>))
import Data.List.NonEmpty.Toolbox (NonEmpty (..), nonEmpty, (<|), (||>))
import Data.List.NonEmpty.Toolbox qualified as NE
import Data.List.Toolbox (dropWhileEnd, foldl', stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString (..))

-- | [a, b, c, ...] <==> a + bx + cx^2 + ...
newtype Polynomial a = Polynomial {unPolynomial :: NonEmpty a}
    deriving stock (Eq, Show)
    deriving newtype (Functor, Applicative, Monad, Foldable)

instance Traversable Polynomial where
    traverse f = fmap Polynomial . traverse f . unPolynomial

pattern ZeroPoly :: (Eq a, Num a) => Polynomial a
pattern ZeroPoly = Polynomial (0 :| [])

-- | Create a univariate 'Polynomial' from a list in ascending order of power.
mkPolynomial :: (Num a) => [a] -> Polynomial a
mkPolynomial = maybe (Polynomial $ pure 0) Polynomial . nonEmpty

-- | Create a univariate 'Polynomial' from a list in descending order of power.
mkPolynomialDesc :: (Num a) => [a] -> Polynomial a
mkPolynomialDesc = maybe (Polynomial $ pure 0) Polynomial . nonEmpty . reverse

-- | Get the list of coefficients in order of increasing power.
toList :: (Eq a, Num a) => Polynomial a -> [a]
toList = dropWhileEnd (== 0) . NE.toList . unPolynomial

-- | Get the coefficient of @x^n@.
(!?) :: (Eq a, Num a) => Polynomial a -> Int -> Maybe a
p !? n = if n < 0 || n > deg p then Nothing else Just $ toList p !! n

-- | Get the degree of a polynomial, where @'deg' 0 == -1@.
deg :: (Eq a, Num a) => Polynomial a -> Int
deg ZeroPoly = -1
deg (Polynomial as) = length as - 1

-- | Get the leading coefficient of a polynomial.
leading :: (Num a) => Polynomial a -> a
leading = NE.last . unPolynomial

{- |
    The reciprocal polynomial is the polynomial whose coefficients
    are in reverse order, __NOT__ @1/p(x)@.
    See [reciprocal polynomial](https://en.wikipedia.org/wiki/Reciprocal_polynomial)
    on Wikipedia
-}
reciprocal :: (Eq a, Num a) => Polynomial a -> Polynomial a
reciprocal = mkPolynomial . reverse . toList

polyZip ::
    (Eq a, Num a, Eq b, Num b) =>
    (a -> b -> c) ->
    Polynomial a ->
    Polynomial b ->
    Polynomial c
polyZip f a@(Polynomial as) b@(Polynomial bs) =
    let d = deg a - deg b
        a' = as ||> replicate (- d) 0
        b' = bs ||> replicate d 0
     in Polynomial $ NE.zipWith f a' b'

instance (Eq a, Num a) => Num (Polynomial a) where
    (+) = polyZip (+)
    Polynomial (a :| as) * Polynomial bs =
        fmap (a *) (Polynomial bs) + case nonEmpty as of
            Nothing -> 0
            Just as' -> Polynomial as' * Polynomial (0 <| bs)
    negate = fmap negate
    abs = error "polynomial abs :("
    signum = error "polynomial signum :("
    fromInteger = Polynomial . pure . fromInteger

-- | @'polyQuotRem' as bs == (qs, rs)@ such that @as == qs * bs + rs@ with @deg rs < deg bs@
polyQuotRem :: (Integral a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
_ `polyQuotRem` ZeroPoly = error "divide by 0"
a `polyQuotRem` b = go (toList a) (toList b) []
  where
    go :: (Integral a) => [a] -> [a] -> [a] -> (Polynomial a, Polynomial a)
    go as bs qs = if d < 0 then (mkPolynomial qs, mkPolynomial as) else go as' bs qs'
      where
        d = length as - length bs
        k = last as `div` last bs
        ks = map (* k) $ replicate d 0 ++ bs
        qs' = toList $ mkPolynomial qs + mkPolynomial (replicate d 0 ++ [k])
        as' = toList $ case last as `mod` last bs of
            0 -> mkPolynomial as - mkPolynomial ks
            _ -> mkPolynomial (map (* last bs) as) - mkPolynomial ks

-- | Calculate the GCD of two polynomials.
polyGCD :: (Integral a) => Polynomial a -> Polynomial a -> Polynomial a
ZeroPoly `polyGCD` _ = ZeroPoly
a `polyGCD` ZeroPoly = a
polyGCD a b =
    let (_, r) = polyQuotRem a b
     in if deg r < 1
            then
                let gcdb = foldl' gcd (leading b) (toList b)
                 in fmap (`div` (if signum (leading b) < 0 then negate else id) gcdb) b
            else polyGCD b r

-- | Show a polynomial in mathematical notation.
prettyPolynomial :: (Ord a, Num a, Show a, IsString t) => Polynomial a -> t
prettyPolynomial (zip [0 ..] . toList -> nxs) =
    let shape = dropWhile (`notElem` ('1' : "234567890-x")) . unwords $ mapMaybe showPower nxs
     in fromString $ case stripPrefix "- " shape of
            Nothing -> shape
            Just poly -> '-' : poly
  where
    showPower :: (Ord a, Num a, Show a) => (Int, a) -> Maybe String
    showPower (_, 0) = Nothing
    showPower (0, v) = Just (show v)
    showPower (n, v) =
        let up = if n == 1 then "" else "^" ++ show n
            sign = if signum v < 0 then "-" else "+"
         in Just $ sign ++ " " ++ (if abs v == 1 then "" else show (abs v)) ++ "x" ++ up

countIntegerPartitionsWith :: [Int] -> Int -> Int
countIntegerPartitionsWith [] _ = 0
countIntegerPartitionsWith xs n =
    fromMaybe 0 $
        product (xs <&> \x -> mkPolynomial . take (succ n) $ cycle (1 : replicate (pred x) 0)) !? n

-- | Count integer partitions. Very slow for values above 25!
integerPartitions :: Int -> Int
integerPartitions n = countIntegerPartitionsWith [1 .. n] n
