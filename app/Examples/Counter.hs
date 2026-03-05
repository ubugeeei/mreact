{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

-- | Counter example — demonstrates useState and useEffect.
module Examples.Counter
  ( counterApp
  ) where

import MReact.Prelude
import qualified Prelude

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
  (count, setCount) <- useState (0 :: Int)

  let doubleCount   =  count * 2

  let parity        =  if even count then "even" else "odd" :: String

  useEffect (deps count)
    (Prelude.putStrLn ("Count changed to: " ++ show count)
      Prelude.>> Prelude.pure (Prelude.pure ()))

  return $ div_ [class_ "counter"]
    [ h1_ [] [text_ "Counter"]
    , p_  [] [text_ ("Count: " ++ show count)]
    , p_  [] [text_ ("Double: " ++ show doubleCount)]
    , p_  [] [text_ ("Parity: " ++ parity)]
    , button_ [onClick (\_ -> setCount (count + 1))]   [text_ "+"]
    , button_ [onClick (\_ -> setCount (count - 1))]   [text_ "-"]
    , button_ [onClick (\_ -> setCount 0)]              [text_ "Reset"]
    ]
