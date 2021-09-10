-- url: %url

-- {{{ LANGUAGE pragmas
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- }}}
-- {{{ standard library imports
import           Control.Applicative         (liftA2)
import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Data.Array                  (Array)
import qualified Data.Array                  as A
import           Data.Array.IArray           hiding ((!))
import           Data.Array.Unboxed          (UArray)
import qualified Data.Array.Unboxed          as U
import           Data.Bits
import           Data.Bool
import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as C
import           Data.Char
import           Data.Function
import           Data.Functor.Identity
import qualified Data.Graph                  as G
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.Kind
import           Data.List
import           Data.List.Split
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.Sequence               (Seq (..), (<|), (|>))
import qualified Data.Sequence               as Seq
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Tree
import           Data.Tuple
import           Debug.Trace
import           GHC.Exts                    (Constraint)
import qualified Numeric                     as N
import           Text.Printf

import           Text.Parsec                 hiding (State)
import           Text.Parsec.ByteString.Lazy
import qualified Text.Parsec.Char            as P
import           Text.Parsec.Expr
import qualified Text.Parsec.Token           as T
-- }}}
-- {{{ Scanner
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets head

str :: Scanner C.ByteString
str = get >>= \case { s:ss -> put ss >> return s }

int :: Scanner Int
int = fst . fromJust . C.readInt <$> str

integer :: Scanner Integer
integer = read . C.unpack <$> str

double :: Scanner Double
double = read . C.unpack <$> str

decimal :: Int -> Scanner Int
decimal p = round . ((10^p)*) <$> double

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

rep :: Scanner a -> Scanner [a]
rep s = get >>= \case { [] -> return []; _ -> (:) <$> s <*> rep s }

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  if p t then return [] else (:) <$> s <*> till p s

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) = times

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map times [2..4]

pair :: Scanner a -> Scanner b -> Scanner (a,b)
pair = liftA2 (,)
-- }}}
-- {{{ utility
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fj :: Maybe a -> a
fj = fromJust

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

sortGroupOn :: Ord b => (a -> b) -> [a] -> [(b,[a])]
sortGroupOn f = sortOn f >>> groupOn f >>> map ((f.head) &&& id)

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y =
  case comparing f x y of
    GT -> x
    _  -> y

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y =
  case comparing f x y of
    LT -> x
    _  -> y

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 :: Int ..]

findJust :: (a -> Bool) -> [a] -> a
findJust p = find p >>> fj

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f = iterate f >>> take (n+1)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = iterate f >>> (!!n)

onHead :: (a -> a) -> [a] -> [a]
onHead _ []     = []
onHead f (x:xs) = f x : xs

showLower :: Show a => a -> String
showLower = onHead toLower . show

readLower :: Read a => String -> a
readLower = read . onHead toUpper

showB :: Show a => a -> C.ByteString
showB = show >>> C.pack

readB :: Read a => C.ByteString -> a
readB = C.unpack >>> read

readBase :: Int -> [Char] -> String -> Int
readBase b digits s =
  case N.readInt b (`elem` digits) (flip elemIndex digits >>> fromJust) s of
    ((a,_):_) -> a

-- Discrete binary search.  Find the smallest integer in [lo,hi] such
-- that monotone predicate p holds.
binarySearchD :: Int -> Int -> (Int -> Bool) -> Int
binarySearchD lo hi p
  | lo == hi = lo
  | p mid     = binarySearchD lo mid p
  | otherwise = binarySearchD (mid+1) hi p
  where
    mid = (lo + hi) `div` 2

infixl 0 >$>
(>$>) = flip ($)
{-# INLINE (>$>) #-}

class Indexable f where
  type Index f :: *
  type C f a :: Constraint
  (!) :: C f a => f a -> Index f -> a

instance Ix i => Indexable (Array i) where
  type Index (Array i) = i
  type C (Array i) a = ()
  (!) = (A.!)

instance Ix i => Indexable (UArray i) where
  type Index (UArray i) = i
  type C (UArray i) a = IArray UArray a
  (!) = (U.!)

instance Ord k => Indexable (Map k) where
  type Index (Map k) = k
  type C (Map k) a = ()
  (!) = (M.!)

instance (Eq k, Hashable k) => Indexable (HashMap k) where
  type Index (HashMap k) = k
  type C (HashMap k) a = ()
  (!) = (HM.!)

toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

fromTable :: Ix i => Array i a -> (i -> a)
fromTable = (!)

memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
memo rng = fromTable . toTable rng

memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
memoFix rng f = fix (memo rng . f)

-- This is annoying --- the one in the parsec library is specialized
-- to String, even though the below copy-pasted code typechecks at
-- ByteString instead!

emptyDef   :: T.GenLanguageDef ByteString st Identity
emptyDef    = T.LanguageDef
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = P.letter <|> P.char '_'
               , identLetter    = P.alphaNum <|> P.oneOf "_'"
               , opStart        = T.opLetter emptyDef
               , opLetter       = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
-- }}}

main = C.interact $ runScanner tc >>> solve >>> format

data TC = TC { }
  deriving (Eq, Show)

tc :: Scanner TC
tc = do

  return TC{..}

type Output = String

format :: Output -> ByteString
format = undefined

solve :: TC -> Output
solve TC{..} = undefined
