{-# LANGUAGE InstanceSigs, ExplicitForAll, DeriveDataTypeable #-}
module Length 
  (Length(..), to_miles, add)
  where

import Data.Generics (everywhere, mkT, Data, Typeable)

data  Foo = Foo String Int 
  deriving (Eq, Show, Typeable, Data)
newtype Length = Length Foo 
  deriving (Eq, Show, Typeable, Data)

-- data Miles  deriving (Data)
-- data Kilometers


to_miles :: Int -> Maybe Length
to_miles x | x<0 = Nothing
to_miles x       = Just (Length $ Foo (show x) x)

add :: Length -> Length -> Length
add (Length (Foo _ x)) (Length (Foo _ y)) | (x<0) || (y<0) = undefined
add (Length (Foo _ x)) (Length (Foo _ y))                  = Length $ Foo (show $ x+y) (x+y)
