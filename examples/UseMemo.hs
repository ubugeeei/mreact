{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useMemo example — demonstrates memoized computation.
module UseMemo
  ( useMemoApp
  ) where

import MReact.Prelude
import qualified Prelude

default (Int, String)

-- | A component that memoizes an expensive computation.
--
-- @useMemo@ caches the result and only recomputes when deps change.
-- Without memoization, the computation would run on every render.
useMemoApp :: FC _ ()
useMemoApp () = do
  (n, setN) <- useState 10

  -- Only recomputed when `n` changes
  fib <- useMemo (deps n) (\() -> fibonacci n)

  return $ div []
    [ h1 [] ["useMemo Demo"]
    , p  [] [text ("n = " ++ show n)]
    , p  [] [text ("fibonacci(n) = " ++ show fib)]
    , button [onClick (\_ -> setN (n + 1))] ["+"]
    , button [onClick (\_ -> setN (Prelude.max 0 (n - 1)))] ["-"]
    ]

-- | Naive fibonacci (intentionally slow for large n).
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)
