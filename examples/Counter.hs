{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | Counter example — demonstrates useState and useEffect.
module Counter
  ( counterApp
  ) where

import MReact.Prelude

default (Int, String)

-- | A simple counter component.
--
-- The hook list is inferred via @PartialTypeSignatures@:
--   * @'SEffect@ — one useEffect call
--   * @'SState Int@ — one useState(Int) call
--
-- @doubleCount@ and @parity@ are derived state — plain Haskell @let@
-- bindings that recompute on every render, not memoized via @useMemo@.
-- This is the idiomatic approach when the derivation is cheap.
counterApp :: FC _ ()
counterApp () = do
  (count, setCount) <- useState 0

  let doubleCount   =  count * 2

  let parity        =  if even count then "even" else "odd"

  useEffect (deps count) $
    putStrLn ("Count changed (from useEffect): " ++ show count)

  return $ div [class_ "counter"]
    [ h1 [] ["Counter"]
    , p  [] [text ("Count: " ++ show count)]
    , p  [] [text ("Double: " ++ show doubleCount)]
    , p  [] [text ("Parity: " ++ parity)]
    , button [onClick (\_ -> setCount (count + 1))]   ["+"]
    , button [onClick (\_ -> setCount (count - 1))]   ["-"]
    , button [onClick (\_ -> setCount 0)]              ["Reset"]
    ]
