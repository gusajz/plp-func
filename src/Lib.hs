module Lib
    ( foldNat
    ) where


foldNat :: (Integer -> a -> a) -> a -> Integer -> a
foldNat _ z 0 = z
foldNat f z n = f n accum
            where accum = foldNat f z (n-1)
