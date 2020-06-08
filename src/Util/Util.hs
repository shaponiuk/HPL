module Util.Util where

import Debug.Trace
import Control.Monad

debug :: [Int]
debug = [2]

printD :: Show a => Int -> a -> IO ()
printD debugFrequency a = when (debugFrequency `elem` debug) $ print a

traceD :: Show a => a -> b -> b
traceD a b = 
    if debug /= [0]
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
cutFirst _ [] = undefined

takeLast :: (Show a) => Int -> [a] -> [a]
takeLast d l = cutFirst (length l - d) l

second :: (a, b) -> b
second (_, b) = b

takeNth :: Int -> [a] -> a
takeNth 0 (x:_) = x
takeNth d (_:xs) = takeNth (d - 1) xs
takeNth _ [] = undefined

getFirsts :: [[a]] -> ([a], [[a]])
getFirsts [] = ([], [])
getFirsts ([]:xs) = getFirsts xs
getFirsts ((x:xs):xss) =
    let
        (firsts, lasts) = getFirsts xss
    in (x:firsts, xs:lasts)

getIndexedInt :: Int -> [a] -> [Int] -> [a]
getIndexedInt _ [] _ = []
getIndexedInt _ _ [] = []
getIndexedInt id (x:xs) (id_:ids) =
    if id == id_
        then x : getIndexedInt (id + 1) xs ids
        else getIndexedInt (id + 1) xs (id_:ids)

getIndexed :: [a] -> [Int] -> [a]
getIndexed = getIndexedInt 0