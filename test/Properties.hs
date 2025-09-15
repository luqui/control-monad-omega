{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (Applicative(..), (<|>))
import Control.Monad (join)
import Control.Monad.Omega
import Data.List (sort)
import Prelude hiding (Applicative(..))
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "pure" $ \(x :: Int) ->
    sort (runOmega (pure x)) ===
      sort (pure x)
  , testProperty "liftA2" $ \(xs :: [Int]) ys ->
    sort (runOmega (liftA2 (+) (each xs) (each ys))) ===
      sort (liftA2 (+) xs ys)
  , testProperty "(<|>)" $ \(xs :: [Int]) ys ->
    sort (runOmega (each xs <|> each ys)) ===
      sort (xs <|> ys)
  , testProperty "join" $ \(xss :: [[Int]]) ->
    sort (runOmega (join (each (map each xss)))) ===
      sort (join xss)

  , testProperty "liftA2 vs. join" $ \(xs :: [Int]) ys ->
    let f x y = x * 10 + y in
    runOmega (liftA2 f (each xs) (each ys)) ===
      runOmega (join (each (map (\x -> each (map (\y -> f x y) ys)) xs)))

  , adjustOption (min (mkTimeout 1000000)) $
    testProperty "enumerate arithGrammar" $ once $
      take 10 (runOmega (enumerate arithGrammar)) ===
        ["0","1","0+0","0*0","0+1","(0)","1+0","0*1","0+0*0","00"]
  ]

-- From https://web.archive.org/web/20140823135714/http://lukepalmer.wordpress.com/2008/05/02/enumerating-a-context-free-language/

data Symbol a
  = Terminal a
  | Nonterminal [[Symbol a]] -- a disjunction of juxtapositions

enumerate :: Symbol a -> Omega [a]
enumerate (Terminal a) = return [a]
enumerate (Nonterminal alts) = do
  alt <- each alts          -- for each alternative
  rep <- mapM enumerate alt -- enumerate each symbol in the sequence
  return $ concat rep       -- and concatenate the results

arithGrammar :: Symbol Char
arithGrammar = s
  where
    s      = Nonterminal [[add]]
    add    = Nonterminal [[mul], [add, Terminal '+', mul]]
    mul    = Nonterminal [[term], [mul, Terminal '*', term]]
    term   = Nonterminal [[number], [Terminal '(', s, Terminal ')']]
    digit  = Nonterminal $ map (map Terminal . show) [0..9]
    number = Nonterminal [[digit], [digit, number]]
