module Util.Util where

import Debug.Trace
import Control.Monad

debug :: Bool
debug = False

printD :: Show a => a -> IO ()
printD a = when debug $ print a

traceD :: Show a => a -> b -> b
traceD a b = 
    if debug
        then
            trace (show a) b
        else
            b

dList :: (Show a, Show b) => [a] -> [b] -> [(a, b)]
dList [] _ = []
dList _ [] = []
dList (x:xs) (y:ys) = (x, y):dList xs ys

tList :: (Show a, Show b, Show c) => [a] -> [b] -> [c] -> [(a, b, c)]
-- tList [] [] [] = []
tList (x:xs) (y:ys) (z:zs) = (x, y, z):tList xs ys zs
-- tList xs ys zs = trace (show xs ++ show ys ++ show zs) undefined
-- temporary, most likely
tList _ _ _ = []

first :: Show a => (a -> Bool) -> [a] -> a
first _ [] = undefined
first f (x:xs) = if f x then x else first f xs

cutLast :: [a] -> [a]
cutLast [] = undefined
cutLast [x] = []
cutLast (x:x2:xs) = x:cutLast (x2:xs)

ioAll :: (a -> IO Bool) -> [a] -> IO Bool
ioAll _ [] = return True
ioAll f (x:xs) = do
    val <- f x
    if val 
        then ioAll f xs
        else return False

cutFirst :: Int -> [a] -> [a]
cutFirst 0 l = l
cutFirst d (x:xs) =
    if d < 0
        then undefined
        else cutFirst (d - 1) xs

takeLast :: (Show a) => Int -> [a] -> [a]
takeLast d l = cutFirst (length l - d) l