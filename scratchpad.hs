-- | References:
--
-- https://news.ycombinator.com/item?id=1532179
-- https://wiki.haskell.org/Memoization
-- https://wildonblog.wordpress.com/2016/10/07/the-fixed-point-combinator-and-memoization-in-haskell/
-- http://matt.might.net/articles/implementation-of-recursive-fixed-point-y-combinator-in-javascript-for-memoization/
-- https://hendrix-cs.github.io/csci365/
-- https://stackoverflow.com/questions/23214296/how-to-use-the-memoize-function-in-data-function-memoize
-- http://hackage.haskell.org/package/memoize-0.6/docs/Data-Function-Memoize.html#v%3AmemoFix
--
module Scratchpad where

import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
import System.IO.Unsafe

data Ingredient = Almonds | Chocolate | Bananas
    deriving Show

data Cake = Slice [Ingredient]
          | Cake Cake Cake
          deriving Show

exampleIngredientsList = [Chocolate, Almonds, Bananas]
exampleIngredientsList' = [Almonds, Bananas]

exampleCake0 = Slice []
exampleCake1 = Cake (Slice [Almonds]) (Slice [Chocolate])
exampleCake2 = Cake (Slice [Almonds, Bananas]) (Slice [Chocolate])
exampleCake3 = Cake (Cake (Slice [Almonds, Bananas]) (Cake (Slice [Chocolate]) (Slice [Bananas]))) (Slice [Chocolate])
exampleCake4 = Slice exampleIngredientsList


countIngredients :: [Ingredient] -> Int -> Int
countIngredients [] acc = acc
countIngredients (x:xs) acc = countIngredients xs (acc + 1)

-- | Examples for foldCake
--
-- >>> foldCake countIngredients 0 exampleCake0
-- 0
--
-- >>> foldCake countIngredients 0 exampleCake1
-- 2
--
-- >>> foldCake countIngredients 0 exampleCake2
-- 3
--
-- >>> foldCake countIngredients 0 exampleCake3
-- 5
--
-- >>> foldCake countIngredients 0 exampleCake4
-- 3
foldCake :: ([Ingredient] -> a -> a) -> a -> Cake -> a
foldCake f base (Slice ingredientList) = f ingredientList base
foldCake f base (Cake c1 c2) = foldCake f base' c2
    where base' = foldCake f base c1
          --
-- Equivalent to:
--foldCake :: ([Ingredient] -> a -> a) -> a -> Cake -> a
--foldCake f base cake =
    --case cake of
      --Slice ingredientList -> f ingredientList base
      --Cake c1 c2           -> foldCake f (foldCake f base c1) c2
      --


-- Fibonacci with State
fib :: Int -> State (M.Map Int Int) Int
fib n = do
    store <- get
    case M.lookup n store of
      Just res -> pure res
      Nothing -> calc n

calc :: Int -> State (M.Map Int Int) Int
calc n = do
    x1 <- fib (n - 1)
    x2 <- fib (n - 2)
    let next = x1 + x2
    modify (\store -> M.insert n next store)
    pure next

startState = M.fromList [(0, 0), (1, 1)]



-- Fixed points and Y combinators with memoization
fix' :: (a -> a) -> a
fix' f = f (fix' f)

factorial1 :: (Int -> Int) -> Int -> Int
factorial1 _ 0 = 1
factorial1 f n = n * f (n - 1)

factorialF :: Int -> Int
factorialF = fix' factorial1


fibonacci1 :: (Int -> Int) -> Int -> Int
fibonacci1 _ 0 = 1
fibonacci1 _ 1 = 1
fibonacci1 f n =
    f (n - 1) + f (n - 2)
    
fibonacciF :: Int -> Int
fibonacciF = fix' fibonacci1

sum1 :: (Int -> Int) -> Int -> Int
sum1 _ 0 = 0
sum1 f n = n + f (n - 1)

sumF :: Int -> Int
sumF = fix' sum1


memoizeInt :: (Int -> Int) -> Int -> Int
memoizeInt f = \m -> values !! m
    where values = [f m | m <- [0..]]

fibBuilder' :: (Int -> Int) -> (Int -> Int)
fibBuilder' f = g
   where g n | n <= 1    = 1
             | otherwise = f (n-2) + f (n-1) 


fibInf' = fix' fibBuilder'
fibInf'' = fibBuilder' fibInf''

fibMInf = fibBuilder' (memoizeInt fibMInf)
fibMInf' = fix' (fibBuilder' . memoizeInt)

memoize :: Ord a => (a -> b) -> a -> b
memoize f = 
    let store = M.empty
    in
    (\a -> 
        let res = M.lookup a store
        in 
        case res of
          Just b -> b
          Nothing -> f a)

fibMem = fix' (fibBuilder' . memoize)


memoize' :: Ord a => (a -> b) -> IO (a -> b)
memoize' f = do
    ref <- newIORef M.empty
    return $ \x -> unsafePerformIO $ do
        m <- liftM (M.lookup x) (readIORef ref)
        case m of
            Just y -> return y
            Nothing -> let y = f x
                       in do modifyIORef ref (M.insert x y)
                             return y

