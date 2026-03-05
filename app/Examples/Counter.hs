{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Counter example — demonstrates useState and useEffect.
module Examples.Counter
  ( counterApp
  ) where

import MReact.Prelude

-- | A simple counter component.
--
-- Type signature reveals exactly which hooks are used:
--   * @'SEffect@ — one useEffect call
--   * @'SState Int@ — one useState(Int) call
--
-- (Read right-to-left: hooks are prepended by each call.)
--
-- @doubleCount@ and @parity@ are derived state — plain Haskell @let@
-- bindings that recompute on every render, not memoized via @useMemo@.
-- This is the idiomatic approach when the derivation is cheap.
counterApp :: FC '[] '[ SEffect, SState Int] ()
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
