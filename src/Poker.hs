module Poker (in054) where

import Data.Function (on)
import Data.List.Extra ((\\), elemIndex, groupOn, sort, sortOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Tuple.Extra (both)
import System.IO.Unsafe (unsafePerformIO)
import Test.FitSpec.Utils (subsets)

data CardValue
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Ord, Show, Enum, Bounded)
data Suit
    = Spade
    | Club
    | Heart
    | Diamond
    deriving (Eq, Show, Enum)

data Card = Card { val :: CardValue, suit :: Suit } deriving (Eq)

instance Ord Card where
    compare = compare `on` val

instance Show Card where
    show = showCard

showCardValue :: CardValue -> Char
showCardValue = \case
    One   -> 'A'
    Two   -> '2'
    Three -> '3'
    Four  -> '4'
    Five  -> '5'
    Six   -> '6'
    Seven -> '7'
    Eight -> '8'
    Nine  -> '9'
    Ten   -> 'T'
    Jack  -> 'J'
    Queen -> 'Q'
    King  -> 'K'
    Ace   -> 'A'

showSuit :: Suit -> String
showSuit = \case
    Spade   -> "♠︎"
    Club    -> "♣︎"
    Heart   -> "♥︎"
    Diamond -> "♦︎"

showCard :: Card -> String
showCard (Card v s) = showCardValue v : showSuit s

readCardValue :: Char -> CardValue
readCardValue = \case
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    '7' -> Seven
    '8' -> Eight
    '9' -> Nine
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
    _   -> error "invalid card value"

readSuit :: Char -> Suit
readSuit = \case
    'S' -> Spade
    'C' -> Club
    'H' -> Heart
    'D' -> Diamond
    _   -> error "invalid suit"

readCard :: String -> Card
readCard [v, s] = Card (readCardValue v) (readSuit s)
readCard _      = error "invalid card"

deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Spade .. Diamond]

type Poker = [Card]
data PokerHand
    = HighCard  [Card]
    | OnePair   [Card] [Card]
    | TwoPairs  [Card] [Card] Card
    | ThreeKind [Card] [Card]
    | Straight  [Card]
    | Flush     [Card]
    | FullHouse [Card] [Card]
    | FourKind  [Card] Card
    | StrFlush  Card
    deriving (Eq, Show, Ord)

sameSuit :: [Card] -> Bool
sameSuit h = all ((== suit (head h)) . suit) $ tail h

sameValue :: [Card] -> Bool
sameValue h = all ((== val (head h)) . val) $ tail h

straight :: Poker -> Bool
straight h = b == succ a
          && b == pred c
          && c == pred d
          && d == pred e
    where h'@[a, b, c, d, e] = map val (sort h)

wheel :: [CardValue] -> Bool
wheel = (==[Two, Three, Four, Five, Ace]) . sort

strFlush :: Poker -> Bool
strFlush = (&&) <$> sameSuit <*> straight

fourKind :: Poker -> Bool
fourKind = any sameValue . filter ((==4) . length) . subsets

threeKind :: Poker -> Bool
threeKind = any sameValue . filter ((==3) . length) . subsets

twoPairs :: Poker -> Bool
twoPairs h = any (pair . (h \\)) . filter sameValue . filter ((==2) . length) $ subsets h

pair :: Poker -> Bool
pair = any sameValue . filter ((==2) . length) . subsets

fullHouse :: Poker -> Bool
fullHouse h = (threeKind h &&) . pair . (h \\) . head . filter sameValue . filter ((==3) . length) $ subsets h

hand :: Poker -> PokerHand
hand h@[a, b, c, d, e]
    | strFlush  h' = StrFlush  $ head h'
    | fourKind  h' = FourKind  (head h'') (head $ last h'')
    | fullHouse h' = FullHouse (head h'') (h' \\ head h'')
    | sameSuit  h' = Flush     h'
    | wheel     g' = Straight  (tail h' ++ [Card One q'])
    | straight  h' = Straight  h'
    | threeKind h' = ThreeKind (head h'') (h' \\ head h'')
    | twoPairs  h' = TwoPairs  (head h'') (head $ tail h'') (head $ last h'')
    | pair      h' = OnePair   (head h'') (h' \\ head h'')
    | otherwise    = HighCard  h'
    where h'  = sortOn Down h
          h'' = sortOn (Down . length) $ groupOn val h'
          g'  = map val h'
          q'  = suit . (h !!) . fromJust $ elemIndex Ace g'

type PokerGame = (PokerHand, PokerHand)

in054 :: IO [PokerGame]
in054 = map (both hand . splitAt 5 . map readCard . words) . lines <$> readFile "./Inputs/054.txt"