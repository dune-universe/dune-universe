{-# LANGUAGE InstanceSigs, ExplicitForAll #-}
module Main where

import Length (add, to_miles)

import Data.Generics (everywhere, mkT, Data, Typeable)

main :: IO ()
main = do
    print l1
    print l2
    print l3
    print l4  
  where 
    l1 = to_miles 10
    l2 = to_miles 20
    l3 = l1 >>= \x -> l2 >>= \y -> Just $ add x y
    incrLen :: Int -> Int
    incrLen n = n-100
    l4 = everywhere (mkT incrLen) l2

{-
prints: 

Just (Length (Foo "10" 10))
Just (Length (Foo "20" 20))
Just (Length (Foo "30" 30))
Just (Length (Foo "20" (-80)))
-}    