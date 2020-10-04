module Polynomials
    ( Polynomial(..)
    , poly, deg, leadingCoeff, getCoeff
    , polyQuotRem
    , polyGCD
    , numPartitionsUsing
    ) where

import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (toList))
import Data.List.Extra
    ( genericLength
    , genericReplicate
    , genericTake
    , stripPrefix
    , dropWhileEnd'
    )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple.Extra (both)

newtype Polynomial a = Poly [a] deriving (Eq, Functor, Foldable)

instance (Default a) => Default (Polynomial a) where
    def = Poly [def]

instance (Default a, Eq a, Num a) => Num (Polynomial a) where
    (+) = zipPoly (+)
    (*) x@(Poly xs) y@(Poly ys) | x == def  = def
                                | y == def  = def
                                | otherwise = let a:as = xs
                                              in  fmap (*a) y + (poly as * poly (def:ys))
    negate = fmap negate
    abs    = error "abs"
    signum (Poly xs) = poly . (:[]) . signum $ last xs
    fromInteger      = poly . (:[]) . fromInteger

instance (Eq a, Ord a, Num a, Show a) => Show (Polynomial a) where
    show = displayPoly

-- Little-endian
poly :: (Default a, Eq a) => [a] -> Polynomial a
poly [] = def
poly xs | xs == [def] = def
        | otherwise   = Poly $ dropWhileEnd' (==def) xs

deg :: (Default a, Eq a) => Polynomial a -> Integer
deg x@(Poly xs) | x == def  = -1
                | otherwise = genericLength xs - 1

leadingCoeff :: Polynomial a -> a
leadingCoeff (Poly as) = last as

getCoeff :: (Default a) => Integer -> Polynomial a -> a
getCoeff n (Poly as) = as !! fromIntegral n

zipPoly :: (Default a, Eq a) => (a -> a -> a) -> Polynomial a -> Polynomial a -> Polynomial a
zipPoly f x@(Poly xs) y@(Poly ys) = poly $ zipWith f xs' ys' where
    d   = deg x - deg y
    xs' = xs ++ replicate (fromInteger (-d)) def
    ys' = ys ++ replicate (fromInteger d) def

polyQuotRem :: (Eq a, Default a, Integral a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
polyQuotRem a@(Poly as) b@(Poly bs)
    | b == def  = error "divide by 0"
    | otherwise = both poly $ go as bs [] where
        go as bs qs
            | d < 0     = (qs, as)
            | otherwise = go as' bs qs'
            where
                d   = length as - length bs
                k   = last as `div` last bs
                ks  = map (*k) $ replicate d def ++ bs
                qs' = toList $ poly qs + poly (replicate d def ++ [k])
                as' = case last as `mod` last bs of
                            0 -> toList $ poly as - poly ks
                            _ -> toList $ (poly as * fromIntegral (last bs)) - poly ks

displayPoly :: (Eq a, Ord a, Num a, Show a) => Polynomial a -> String
displayPoly (Poly as) =
    let xpow (n, v) | v ==  0   = Nothing
                    | n ==  0   = Just $ show v
                    | v ==  1   = if n == 1 then Just " + x" else Just $ " + x^" ++ show n
                    | v == -1   = if n == 1 then Just " - x" else Just $ " - x^" ++ show n
                    | v  <  0   = if n == 1 then Just $ " - " ++ show (negate v) ++ "x" else Just $ " - " ++ show (negate v) ++ "x^" ++ show n
                    | otherwise = if n == 1 then Just $ " + " ++ show v ++ "x" else Just $ " + " ++ show v ++ "x^" ++ show n
        y = concat . mapMaybe xpow $ zip [0..] as
    in  "P(x) = " ++
        (case stripPrefix " - " y of
            Nothing -> y
            Just z  -> '-':z) `fromMaybe` stripPrefix " + " y

polyGCD :: (Eq a, Default a, Integral a) => Polynomial a -> Polynomial a -> Polynomial a
polyGCD a@(Poly as) b@(Poly bs)
    | a == def = b
    | b == def = a
    | deg (snd $ polyQuotRem a b) == 0 = fromInteger (fromIntegral (last as) `div` fromIntegral (last bs))
    | otherwise = polyGCD b $ snd (polyQuotRem a b)

numPartitionsUsing :: [Integer] -> Integer -> Integer
numPartitionsUsing [] _ = 0
numPartitionsUsing xs n = getCoeff n . product $ map (\x -> poly . genericTake (n+1) . cycle $ 1 : genericReplicate (x-1) 0) xs