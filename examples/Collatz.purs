module Main where

import Prelude

import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Unfoldable
import Data.Int

import Console

collatz :: Int -> Array Int
collatz = unfoldr step 
  where
  step n | n == one = Nothing
         | otherwise = Just $ Tuple n $ if n `mod` fromNumber 2 == zero 
                                          then n / fromNumber 2 
                                          else n * fromNumber 3 + one

main = print $ collatz $ fromNumber 1000
