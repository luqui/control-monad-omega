module Main where

import Control.Applicative
import Control.Monad.Omega
import Prelude hiding (Applicative)
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bench "liftA2" $ nf
    (\n -> sum $ runOmega $ liftA2 (+) (each [0..n]) (each [0..n]))
    (100 :: Int)
  ]
