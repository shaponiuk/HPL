module Util.Util where

import Debug.Trace

dList :: (Show a, Show b) => [a] -> [b] -> [(a, b)]
dList [] _ = []
dList (x:xs) (y:ys) = (x, y):dList xs ys
dList l1 l2 = trace (show l1 ++ show l2) undefined

tList :: (Show a, Show b, Show c) => [a] -> [b] -> [c] -> [(a, b, c)]
tList [] [] [] = []
tList (x:xs) (y:ys) (z:zs) = (x, y, z):tList xs ys zs
tList xs ys zs = trace (show xs ++ show ys ++ show zs) undefined

forceUnwrapMaybe :: Maybe a -> a
forceUnwrapMaybe (Just x) = x
forceUnwrapMaybe Nothing = undefined

first :: (a -> Bool) -> [a] -> a
first _ [] = undefined
first f (x:xs) = if f x then x else first f xs